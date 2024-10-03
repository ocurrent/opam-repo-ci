(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

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

let generate_dune_project packages =
  let open Sexplib in
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
  let open Sexplib in
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
  print_endline (Printf.sprintf "Generated dune project in %s/dune-project" dir);
  dir

let take n lst =
  List.rev
    (List.fold_left
       (fun acc x -> if List.length acc < n then x :: acc else acc)
       [] lst)

let get_ocaml_version () =
  let ic = Unix.open_process_in "ocamlc --version" in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let exit_status = Unix.close_process_in ic in
  match exit_status with
  | Unix.WEXITED 0 -> output |> String.trim
  | _ -> "5.1.1"

let create_dummy_projects root opam_repository target packages =
  let ocaml_version = get_ocaml_version () in
  let ocaml_pkg = "ocaml-system." ^ ocaml_version in
  List.map
    (fun pkg ->
      create_dummy_project root opam_repository
        (* FIXME: Current OCaml version? Should be configurable *)
        [ target; pkg; OpamPackage.of_string ocaml_pkg ])
    packages

let generate_lock_and_build dir =
  chdir dir ~f:(fun () ->
      let _ = Sys.command "dune pkg lock --verbose" in
      (* FIXME: Just build won't run the tests of the revdep package. We need
         to run the tests of the rev-dep package, not the dummy project! *)
      let _ = Sys.command "dune build --verbose" in
      ())
