let download_cache = "opam-archives"
let cache = [ Obuilder_spec.Cache.v download_cache ~target:"/home/opam/.opam/download-cache" ]
let network = ["host"]

let opam_install ~is_revdeps_tests ~pin ~with_tests ~pkg =
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
    (* TODO: Replace by two calls to opam install + opam install -t using the OPAMDROPINSTALLEDPACKAGES feature *)
    run ~cache ~network "opam depext -uivy%s %s%s" (if with_tests then "t" else "") pkg
      (if is_revdeps_tests then " || test \"$?\" -eq 20" else "")
      (* opam list --depends-on <pkg> --installable --with-test can't distinguish between packages
         installable with and without tests overall. Revdeps should not fail without tests but might
         with tests. So we detect this with exit code = 20 which means "no solution to the user request" *)
  ]

let setup_repository ~variant =
  let open Obuilder_spec in
  let opam_extras =
    if Astring.String.is_suffix ~affix:"-ocaml-4.07" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.06" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.05" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.04" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.03" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.02" variant then
      [ run ~cache ~network "opam install -y ocaml-secondary-compiler" ] (* Speed up builds for dune >= 2.0 *)
    else
      []
  in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" variant then
      [ run ~network "sudo dnf install -y findutils" ] (* (we need xargs) *)
    else
      []
  in
  user ~uid:1000 ~gid:1000 :: distro_extras @ opam_extras @ [
    copy ["."] ~dst:"/src/";
    run "opam repository set-url --strict default file:///src";
  ]

let spec ~base ~variant ~revdep ~with_tests ~pkg =
  let open Obuilder_spec in
  let revdep = match revdep with
    | None -> []
    | Some revdep -> opam_install ~is_revdeps_tests:false ~pin:false ~with_tests:false ~pkg:revdep
  and tests = match with_tests, revdep with
    | true, None -> opam_install ~is_revdeps_tests:false ~pin:false ~with_tests:true ~pkg
    | true, Some revdep -> opam_install ~is_revdeps_tests:true ~pin:false ~with_tests:true ~pkg:revdep
    | false, _ -> []
  in
  { from = base;
    ops =
      setup_repository ~variant
      @ opam_install ~is_revdeps_tests:false ~pin:true ~with_tests:false ~pkg
      @ revdep
      @ tests
  }

let revdeps ~base ~variant ~pkg =
  let open Obuilder_spec in
  let pkg = Filename.quote (OpamPackage.to_string pkg) in
  { from = base;
    ops =
      setup_repository ~variant
      @ [
        run "echo '@@@OUTPUT' && \
             (opam list -s --color=never --depends-on %s --coinstallable-with %s --installable --all-versions --depopts && \
              opam list -s --color=never --depends-on %s --coinstallable-with %s --installable --all-versions --depopts --with-test) \
             | sort -u && \
             echo '@@@OUTPUT'"
          pkg pkg pkg pkg
      ]
  }
