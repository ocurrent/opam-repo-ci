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

let fmt_dockerfile ~base ~info ~variant =
  ignore variant; (* (todo) *)
  let ocamlformat_source = Analyse.Analysis.ocamlformat_source info in
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

let doc_dockerfile ~base ~info:_ ~variant:_ =
  let open Obuilder_spec in
  (*  Opam_build.install_project_deps ~base ~info ~variant ~for_user *)
  if true then assert false;
  { from = base;
    ops = [
      run "opam depext odoc";
      (* Warnings-as-errors was introduced in Odoc.1.5.0 *)
      run "opam install dune odoc>=1.5.0";
      run "ODOC_WARN_ERROR=true opam exec -- dune build @doc \
           || (echo \"dune build @doc failed\"; exit 2)";
    ];
  }
