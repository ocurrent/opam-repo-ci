let write_file ~file ~contents =
  let ch = open_out file in
  output_string ch contents;
  close_out_noerr ch

let chdir dir ~f =
  let old_dir = Sys.getcwd () in
  (try
     Sys.chdir dir;
     f ()
   with e ->
     Sys.chdir old_dir;
     raise e);
  Sys.chdir old_dir

let create_temp_dir prefix =
  let base_temp_dir = Filename.get_temp_dir_name () in
  let unique_temp_dir =
    Filename.concat base_temp_dir
      (prefix ^ (Unix.time () |> int_of_float |> string_of_int))
  in
  Unix.mkdir unique_temp_dir 0o700;
  unique_temp_dir

let generate_dune_project packages =
  let open Sexplib0 in
  let pkg_sexps =
    List.map
      (fun pkg ->
        Sexp.(
          List
            [
              Atom (OpamPackage.name_to_string pkg);
              List [ Atom "="; Atom (OpamPackage.version_to_string pkg) ];
            ]))
      packages
  in
  let expressions =
    Sexp.
      [
        List [ Atom "lang"; Atom "dune"; Atom "3.15" ];
        List
          [
            Atom "package";
            List [ Atom "name"; Atom "dummy" ];
            List ([ Atom "depends" ] @ pkg_sexps);
          ];
      ]
  in
  expressions |> List.map Sexp.to_string |> String.concat "\n"

let generate_dune_workspace opam_repository =
  let open Sexplib0 in
  let expressions =
    Sexp.
      [
        List [ Atom "lang"; Atom "dune"; Atom "3.15" ];
        List [ Atom "lock_dir"; List [ Atom "repositories"; Atom "local" ] ];
        List
          [
            Atom "repository";
            List [ Atom "name"; Atom "local" ];
            List [ Atom "source"; Atom opam_repository ];
          ];
      ]
  in
  expressions |> List.map Sexp.to_string |> String.concat "\n"

let create_dummy_project root opam_repository packages =
  let dir_name =
    packages |> List.map OpamPackage.to_string |> String.concat "-"
  in
  let dir = Printf.sprintf "%s/%s" root dir_name in
  (* Create dune-project *)
  let contents = generate_dune_project packages in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" dir) in
  chdir dir ~f:(fun () -> write_file ~file:"dune-project" ~contents);
  (* Create dune-workspace *)
  let contents = generate_dune_workspace opam_repository in
  chdir dir ~f:(fun () -> write_file ~file:"dune-workspace" ~contents);
  print_endline (Printf.sprintf "Generated dune project in %s/dune-project" dir)

let create_dummy_projects root opam_repository target packages =
  List.iter
    (fun pkg -> create_dummy_project root opam_repository [ target; pkg ])
    packages
