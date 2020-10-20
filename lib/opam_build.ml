let download_cache = "opam-archives"
let cache = [ Obuilder_spec.Cache.v download_cache ~target:"/home/opam/.opam/download-cache" ]

let opam_install ~pin ~with_tests ~pkg =
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
    run ~cache "opam depext -uivy%s %s" (if with_tests then "t" else "") pkg
  ]

let dockerfile ~base ~variant ~revdep ~with_tests ~pkg =
  let pkg = OpamPackage.to_string pkg in
  let open Obuilder_spec in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" variant then
      [ run "sudo dnf install -y findutils" ] (* (we need xargs) *)
    else
      []
  in
  let opam_extras =
    if Astring.String.is_suffix ~affix:"-ocaml-4.07" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.06" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.05" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.04" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.03" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.02" variant then
      [ run ~cache "opam install -y ocaml-secondary-compiler" ] (* Speed up builds for dune >= 2.0 *)
    else
      []
  in
  let revdep = match revdep with
    | None -> []
    | Some revdep -> opam_install ~pin:false ~with_tests:false ~pkg:revdep
  and tests = match with_tests, revdep with
    | true, None -> opam_install ~pin:false ~with_tests:true ~pkg
    | true, Some revdep -> opam_install ~pin:false ~with_tests:true ~pkg:revdep
    | false, _ -> []
  in
  { from = base;
    ops =
      user ~uid:1000 ~gid:1000 ::
      distro_extras
      @ opam_extras
      @ [
        copy ["."] ~dst:"/src/";
        run "opam repository set-url --strict default file:///src";
      ]
      @ opam_install ~pin:true ~with_tests:false ~pkg
      @ revdep
      @ tests
  }
