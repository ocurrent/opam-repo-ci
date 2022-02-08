let download_cache = "opam-archives"
let cache = [ Obuilder_spec.Cache.v download_cache ~target:"/home/opam/.opam/download-cache" ]
let network = ["host"]

let opam_install ~variant ~opam_version ~pin ~lower_bounds ~with_tests ~pkg =
  let pkg = OpamPackage.to_string pkg in
  let with_tests_opt = if with_tests then " --with-test" else "" in
  let open Obuilder_spec in
  (if lower_bounds then
     [
       env "OPAMCRITERIA" "+removed,+count[version-lag,solution]";
       env "OPAMEXTERNALSOLVER" "builtin-0install";
     ]
   else
     []
  ) @
  (if pin then
     let version =
       let idx = String.index pkg '.' + 1 in
       String.sub pkg idx (String.length pkg - idx)
     in
     [ run "opam pin add -k version -yn %s %s" pkg version ]
   else
     []
  ) @ [
    run ~network "opam %s" (match opam_version with `V2_1 -> "update --depexts" | `V2_0 -> "depext -u");
    (* TODO: Replace by two calls to opam install + opam install -t using the OPAMDROPINSTALLEDPACKAGES feature *)
    run ~cache ~network
      {|opam remove %s%s && (opam install --deps-only%s %s || echo @@@FAILED-DEPENDS@@@) && opam install -v%s %s;
        res=$?;
        test "$res" != 31 && exit "$res";
        export OPAMCLI=2.0;
        build_dir=$(opam var prefix)/.opam-switch/build;
        failed=$(ls "$build_dir");
        for pkg in $failed; do
          if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"%s\""; then
            echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field.";
          fi;
        done;
        exit 1|}
      pkg (match opam_version with `V2_1 -> "" | `V2_0 -> " && opam depext"^with_tests_opt^" "^pkg) with_tests_opt pkg with_tests_opt pkg
      (Variant.distribution variant)
  ]

let setup_repository ~variant ~for_docker ~opam_version =
  let open Obuilder_spec in
  user ~uid:1000 ~gid:1000 ::
  run "for pkg in $(opam pin list --short); do opam pin remove \"$pkg\"; done" :: (* The ocaml/opam base images have a pin to their compiler package.
                                                                                     Such pin is useless for opam 2.0 as we don't use --unlock-base,
                                                                                     and causes issues for opam 2.1 as it allows to upgrade the compiler
                                                                                     package (not what we want)
                                                                                     See: https://github.com/ocaml/opam/issues/4501 *)
  run "opam repository remove -a multicore || true" :: (* We remove this non-standard repository
                                                          because we don't have access and it hosts
                                                          non-official packages *)
  run "sudo ln -f /usr/bin/opam-%s /usr/bin/opam"
    (match opam_version with
     | `V2_0 -> "2.0"
     | `V2_1 -> "2.1"
    ) ::
  (* NOTE: [for_docker] is required because docker does not support bubblewrap in docker build *)
  (* docker run has --privileged but docker build does not have it *)
  (* so we need to remove the part re-enabling the sandbox. *)
  (* NOTE: On alpine-3.12 bwrap fails with "capset failed: Operation not permitted". *)
  let sandboxing_not_supported =
    let distro = variant.Variant.distribution in
    String.equal distro (Dockerfile_distro.tag_of_distro (`Alpine `V3_12)) ||
    for_docker
  in
  run "opam init --reinit%s -ni" (if sandboxing_not_supported then "" else " --config ~/.opamrc-sandbox") ::
  env "OPAMDOWNLOADJOBS" "1" :: (* Try to avoid github spam detection *)
  env "OPAMERRLOGLEN" "0" :: (* Show the whole log if it fails *)
  env "OPAMSOLVERTIMEOUT" "500" :: (* Increase timeout. Poor mccs is doing its best *)
  env "OPAMPRECISETRACKING" "1" :: (* Mitigate https://github.com/ocaml/opam/issues/3997 *)
  [
    copy ["."] ~dst:"/src/";
    run "opam repository set-url --strict default file:///src";
  ]

let set_personality ~variant =
  if Variant.arch variant |> Ocaml_version.arch_is_32bit then
    [Obuilder_spec.shell ["/usr/bin/linux32"; "/bin/sh"; "-c"]]
  else
    []

let spec ~for_docker ~opam_version ~base ~variant ~revdep ~lower_bounds ~with_tests ~pkg =
  let opam_install = opam_install ~variant ~opam_version in
  let revdep = match revdep with
    | None -> []
    | Some revdep -> opam_install ~pin:false ~lower_bounds:false ~with_tests:false ~pkg:revdep
  and tests = match with_tests, revdep with
    | true, None -> opam_install ~pin:false ~lower_bounds:false ~with_tests:true ~pkg
    | true, Some revdep -> opam_install ~pin:false ~lower_bounds:false ~with_tests:true ~pkg:revdep
    | false, _ -> []
  and lower_bounds = match lower_bounds with
    | true -> opam_install ~pin:false ~lower_bounds:true ~with_tests:false ~pkg
    | false -> []
  in
  Obuilder_spec.stage ~from:base (
    set_personality ~variant
    @ setup_repository ~variant ~for_docker ~opam_version
    @ opam_install ~pin:true ~lower_bounds:false ~with_tests:false ~pkg
    @ lower_bounds
    @ revdep
    @ tests
  )

let revdeps ~for_docker ~base ~variant ~pkg =
  let open Obuilder_spec in
  let pkg = Filename.quote (OpamPackage.to_string pkg) in
  Obuilder_spec.stage ~from:base (
    (* TODO: Switch to opam 2.1 when https://github.com/ocaml/opam/issues/4311 is fixed *)
    setup_repository ~variant ~for_docker ~opam_version:`V2_0
    @ [
      run "echo '@@@OUTPUT' && \
           opam list -s --color=never --depends-on %s --coinstallable-with %s --installable --all-versions --recursive --depopts && \
           opam list -s --color=never --depends-on %s --coinstallable-with %s --installable --all-versions --with-test --depopts && \
           echo '@@@OUTPUT'"
        pkg pkg
        pkg pkg
    ]
  )
