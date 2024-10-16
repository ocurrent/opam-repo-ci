let fmt = Fmt.str

let download_cache = "opam-archives"
let cache ~variant =
  match Variant.os variant with
  | `Freebsd
  | `Linux -> [ Obuilder_spec.Cache.v download_cache ~target:"/home/opam/.opam/download-cache" ]
  | `Macos -> [ Obuilder_spec.Cache.v download_cache ~target:"/Users/mac1000/.opam/download-cache";
                Obuilder_spec.Cache.v "homebrew" ~target:"/Users/mac1000/Library/Caches/Homebrew" ]
let network = ["host"]

let opam_install ~variant ~opam_version ~pin ~lower_bounds ~with_tests ~revdep ~pkg =
  let pkg_s = OpamPackage.to_string pkg in
  let with_tests_opt = if with_tests then " --with-test" else "" in
  let cache = cache ~variant in
  let open Obuilder_spec in
  (if lower_bounds then
     [
       env "OPAMCRITERIA"        "+removed,+count[version-lag,solution]";
       env "OPAMFIXUPCRITERIA"   "+removed,+count[version-lag,solution]";
       env "OPAMUPGRADECRITERIA" "+removed,+count[version-lag,solution]";
     ]
   else
     []
  ) @
  (if revdep || lower_bounds then
     [
       run "opam option solver=builtin-0install";
     ]
   else
     []
  ) @
  (if pin then
     let version =
       let idx = String.index pkg_s '.' + 1 in
       String.sub pkg_s idx (String.length pkg_s - idx)
     in
     [ run "opam pin add -k version -yn %s %s" pkg_s version ]
   else
     []
  ) @
  (if with_tests then [
     (* TODO: Remove this hack when https://github.com/ocurrent/obuilder/issues/77 is fixed *)
     (* NOTE: This hack will fail for packages that have src: "git+https://..." *)
     run ~network "(%sopam reinstall --with-test %s) || true"
       (match opam_version with `V2_1 | `V2_2 | `Dev -> "" | `V2_0 -> fmt "opam depext%s %s && " with_tests_opt pkg_s) pkg_s
   ] else []) @ [
    (* TODO: Replace by two calls to opam install + opam install -t using the OPAMDROPINSTALLEDPACKAGES feature *)
    (* NOTE: See above for the ~network:(if with_tests ...) hack *)
    (* NOTE: We cannot use the cache as concurrent access to the cache might overwrite it and the required archives might not be available anymore at this point *)
    let depext =
      match opam_version with
      | `V2_1 | `V2_2 | `Dev -> ""
      | `V2_0 -> fmt "opam depext%s %s && " with_tests_opt pkg_s
    in
    let verbose = if with_tests then " --verbose" else "" in
    let update_invariant =
      if String.equal (OpamPackage.name_to_string pkg) "ocaml-variants" then
        " --update-invariant"
      else ""
    in
    run ~cache:(if with_tests then [] else cache) ~network:(if with_tests then [] else network)
      {|%sopam reinstall%s%s%s %s;
        res=$?;
        test "$res" != 31 && exit "$res";
        export OPAMCLI=2.0;
        build_dir=$(opam var prefix)/.opam-switch/build;
        failed=$(ls "$build_dir");
        partial_fails="";
        for pkg in $failed; do
          if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"%s\""; then
            echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field.";
          fi;
          test "$pkg" != '%s' && partial_fails="$partial_fails $pkg";
        done;
        test "${partial_fails}" != "" && echo "opam-repo-ci detected dependencies failing: ${partial_fails}";
        exit 1|}
      depext with_tests_opt verbose update_invariant pkg_s
      (Variant.distribution variant)
      pkg_s
  ]

let setup_repository ?(local=false) ~variant ~for_docker ~opam_version () =
  let open Obuilder_spec in
  let home_dir = match Variant.os variant with
    | `Macos -> None
    | `Freebsd
    | `Linux -> Some "/home/opam"
  in
  let prefix = match Variant.os variant with
    | `Macos -> "~/local"
    | `Freebsd -> "/usr/local"
    | `Linux -> "/usr"
  in
  let ln = match Variant.os variant with
    | `Macos -> "ln"
    | `Freebsd | `Linux -> "sudo ln"
  in
  let opam_version_str = Opam_version.to_string opam_version
  in
  let opam_repo_args = match Variant.os variant with
    | `Macos -> " -k local" (* TODO: (copy ...) do not copy the content of .git or something like that and make the subsequent opam pin fail *)
    | `Freebsd | `Linux -> ""
  in
  let opamrc = match Variant.os variant with
    (* NOTE: [for_docker] is required because docker does not support bubblewrap in docker build *)
    (* docker run has --privileged but docker build does not have it *)
    (* so we need to remove the part re-enabling the sandbox. *)
    | `Linux when not for_docker -> " --config .opamrc-sandbox"
    | `Freebsd | `Macos | `Linux -> ""
    (* TODO: On MacOS, the sandbox is always (and should be) enabled by default but does not have those ~/.opamrc-sandbox files *)
  in
  (* If we are testing a minimal opam-repository without
    the base packages, we have to pin them to keep them around.
    Related: https://github.com/ocaml/opam/issues/5895 *)
  let keep_installed =
    run "for pkg in $(opam list -s --installed) ; do opam pin --current \"$pkg\" ; done"
  in
  user_unix ~uid:1000 ~gid:1000 ::
  (match home_dir with Some home_dir -> [workdir home_dir] | None -> []) @
  (* TODO: MacOS seems to have a bug in (copy ...) so I am forced to remove the (workdir ...) here.
     Otherwise the "opam pin" after the "opam repository set-url" will fail (cannot find the new package for some reason) *)
  run "%s -f %s/bin/opam-%s %s/bin/opam" ln prefix opam_version_str prefix ::
  run ~network "opam init --reinit%s -ni" opamrc :: (* TODO: Remove ~network when https://github.com/ocurrent/ocaml-dockerfile/pull/132 is merged *)
  run "uname -rs && opam exec -- ocaml -version && opam --version" ::
  env "OPAMDOWNLOADJOBS" "1" :: (* Try to avoid github spam detection *)
  env "OPAMERRLOGLEN" "0" :: (* Show the whole log if it fails *)
  env "OPAMSOLVERTIMEOUT" "1000" :: (* Increase timeout. Poor mccs is doing its best *)
  env "OPAMPRECISETRACKING" "1" :: (* Mitigate https://github.com/ocaml/opam/issues/3997 *)
  env "CI" "true" :: env "OPAM_REPO_CI" "true" :: (* Advertise CI for test frameworks *)
  [
    run "rm -rf opam-repository/";
    copy ["."] ~dst:"opam-repository/";
    run "opam repository set-url%s --strict default opam-repository/" opam_repo_args;
    run ~network "opam %s || true" (match opam_version with `V2_1 | `V2_2 | `Dev -> "update --depexts" | `V2_0 -> "depext -u");
  ] @
  if local then [ keep_installed ] else []

let set_personality ~variant =
  if Variant.arch variant |> Ocaml_version.arch_is_32bit then
    [Obuilder_spec.shell ["/usr/bin/linux32"; "/bin/sh"; "-c"]]
  else
    []

let spec ?(local=false) ~for_docker ~opam_version ~base ~variant ~revdep ~lower_bounds ~with_tests ~pkg () =
  let opam_install = opam_install ~variant ~opam_version in
  let revdep = match revdep with
    | None -> []
    | Some revdep -> opam_install ~pin:false ~lower_bounds:false ~with_tests:false ~revdep:true ~pkg:revdep
  and tests = match with_tests, revdep with
    | true, None -> opam_install ~pin:false ~lower_bounds:false ~with_tests:true ~revdep:false ~pkg
    | true, Some revdep -> opam_install ~pin:false ~lower_bounds:false ~with_tests:true ~revdep:true ~pkg:revdep
    | false, _ -> []
  and lower_bounds = match lower_bounds with
    | true -> opam_install ~pin:false ~lower_bounds:true ~with_tests:false ~revdep:false ~pkg
    | false -> []
  in
  Obuilder_spec.stage ~from:base (
    set_personality ~variant
    @ setup_repository ~local ~variant ~for_docker ~opam_version ()
    @ opam_install ~pin:true ~lower_bounds:false ~with_tests:false ~revdep:false ~pkg
    @ lower_bounds
    @ revdep
    @ tests
  )

let revdeps ?(local=false) ~for_docker ~opam_version ~base ~variant ~pkg () =
  let open Obuilder_spec in
  let pkg = Filename.quote (OpamPackage.to_string pkg) in
  Obuilder_spec.stage ~from:base (
    setup_repository ~local ~variant ~for_docker ~opam_version ()
    @ [
      run "echo '@@@OUTPUT' && \
           opam list -s --color=never --depends-on %s --coinstallable-with %s --all-versions --depopts && \
           opam list -s --color=never --depends-on %s --coinstallable-with %s --all-versions --recursive && \
           opam list -s --color=never --depends-on %s --coinstallable-with %s --all-versions --with-test --depopts && \
           echo '@@@OUTPUT'"
        pkg pkg
        pkg pkg
        pkg pkg
    ]
  )

let build_spec ?(local=false) ~for_docker ~base config =
  let {Spec.variant; ty} = config in
  let base = Spec.base_to_string base in
  match ty with
  | `Opam (`List_revdeps { opam_version }, pkg) ->
    revdeps ~local ~for_docker ~opam_version ~base ~variant ~pkg ()
  | `Opam (`Build { revdep; lower_bounds; with_tests; opam_version }, pkg) ->
    spec ~local ~for_docker ~opam_version ~base ~variant ~revdep ~lower_bounds ~with_tests ~pkg ()
