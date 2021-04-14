let download_cache = "opam-archives"
let cache = [ Obuilder_spec.Cache.v download_cache ~target:"/home/opam/.opam/download-cache" ]
let network = ["host"]

let opam_install ~variant ~upgrade_opam ~pin ~with_tests ~pkg =
  let pkg = OpamPackage.to_string pkg in
  let open Obuilder_spec in
  let pin =
    if pin then
      let version =
        let idx = String.index pkg '.' + 1 in
        String.sub pkg idx (String.length pkg - idx)
      in
      [ run "opam pin add -k version -yn %s %s" pkg version ]
    else
      []
  in
  pin @ [
    run ~network "opam %s" (if upgrade_opam then "update --depexts" else "depext -yu");
    (* TODO: Replace by two calls to opam install + opam install -t using the OPAMDROPINSTALLEDPACKAGES feature *)
    run ~cache ~network {|
        opam remove -y %s && opam %s%s %s
        res=$?
        test "$res" = 0 && exit 0
        if test "$res" = 60 && diff -q /usr/bin/opam /usr/bin/opam-2.0; then
          sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
          opam remove -y %s && opam install -vy%s %s%s
          exit 1
        fi
        test "$res" != 31 && exit 1
        export OPAMCLI=2.0
        build_dir=$(opam var prefix)/.opam-switch/build
        failed=$(ls "$build_dir")
        for pkg in $failed; do
          if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"%s\""; then
            echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field."
          fi
        done
        exit 1|}
      pkg (if upgrade_opam then "install -vy" else "depext -ivy") (if with_tests then "t" else "") pkg
      pkg (if with_tests then "t" else "") pkg (if with_tests then "" else " && opam reinstall -vyt "^pkg)
      (Variant.distribution variant)
  ]

let setup_repository ~variant ~for_docker ~upgrade_opam =
  let open Obuilder_spec in
  user ~uid:1000 ~gid:1000 ::
  (if upgrade_opam then [
    run "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam";
    env "OPAMDEPEXTYES" "1"] else []) @
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

let spec ~for_docker ~upgrade_opam ~base ~variant ~revdep ~with_tests ~pkg =
  let open Obuilder_spec in
  let revdep = match revdep with
    | None -> []
    | Some revdep -> opam_install ~variant ~upgrade_opam ~pin:false ~with_tests:false ~pkg:revdep
  and tests = match with_tests, revdep with
    | true, None -> opam_install ~variant ~upgrade_opam ~pin:false ~with_tests:true ~pkg
    | true, Some revdep -> opam_install ~variant ~upgrade_opam ~pin:false ~with_tests:true ~pkg:revdep
    | false, _ -> []
  in
  { from = base;
    ops =
      set_personality ~variant
      @ setup_repository ~variant ~for_docker ~upgrade_opam
      @ opam_install ~variant ~upgrade_opam ~pin:true ~with_tests:false ~pkg
      @ revdep
      @ tests
  }

let revdeps ~for_docker ~base ~variant ~pkg =
  let open Obuilder_spec in
  let pkg = Filename.quote (OpamPackage.to_string pkg) in
  { from = base;
    ops =
      setup_repository ~variant ~for_docker ~upgrade_opam:false
      @ [
        run "echo '@@@OUTPUT' && \
             opam list -s --color=never --depends-on %s --coinstallable-with %s --installable --all-versions --recursive --depopts && \
             opam list -s --color=never --depends-on %s --coinstallable-with %s --installable --all-versions --with-test --depopts && \
             echo '@@@OUTPUT'"
          pkg pkg
          pkg pkg
      ]
  }
