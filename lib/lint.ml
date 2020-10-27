let install_ocamlformat =
  let open Obuilder_spec in
  function
  | None -> []
  | Some Analyse_ocamlformat.Vendored { path } ->
    let opam_file = Filename.concat path "ocamlformat.opam" in
    [
      copy [ opam_file ] ~dst:opam_file;
      run "opam pin add -k path -n ocamlformat %S" path;
      (* Pinned to a directory containing only the .opam file *)
      run "opam depext ocamlformat";
      run "opam install --deps-only -y ocamlformat";
    ]
  | Some Opam { version } ->
    [
      run "opam depext ocamlformat=%s" version;
      run "opam install ocamlformat=%s" version
    ]

let fmt_dockerfile ~base ~ocamlformat_source ~variant =
  ignore variant; (* (todo) *)
  let open Obuilder_spec in
  { from = base;
    ops = [
      user ~uid:1000 ~gid:1000;
      run "opam install dune"; (* Not necessarily the dune version used by the project *)
      workdir "src";
    ] @ install_ocamlformat ocamlformat_source @ [
      copy ["./"] ~dst:"./";
      run "opam exec -- dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)";
    ];
  }
