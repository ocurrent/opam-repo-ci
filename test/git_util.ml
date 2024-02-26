open Lwt.Infix
(* open Current.Syntax *)

module Cmd = struct
  let exec_or_fail ?cwd ~name cmd =
    let cmd_s = String.concat " " cmd in
    let cmd = Array.of_list cmd in
    let cwd = Option.map Fpath.to_string cwd in
    Lwt_process.exec ?cwd ("", cmd) >|= function
    | Unix.WEXITED n ->
        Alcotest.(check int) (Printf.sprintf "Process %s: %s" name cmd_s) 0 n
    | Unix.WSTOPPED _ -> Alcotest.fail "Process stopped."
    | Unix.WSIGNALED _ -> Alcotest.fail "Process received signal."

  let mkdir ?cwd dir =
    let cmd = [ "mkdir"; Fpath.to_string dir ] in
    exec_or_fail ?cwd ~name:"mkdir" cmd

  let rm ?cwd flags targets =
    let targets = List.map Fpath.to_string targets in
    let cmd = "rm" :: flags @ targets in
    exec_or_fail ?cwd ~name:"rm" cmd

  let mv ?cwd source target =
    let cmd = [ "mv"; Fpath.to_string source; Fpath.to_string target ] in
    exec_or_fail ?cwd ~name:"mv" cmd

  let cp ?cwd flags source target =
    let cmd =
      "cp" :: flags @ [ Fpath.to_string source; Fpath.to_string target ]
    in
    exec_or_fail ?cwd ~name:"cp" cmd

  let touch ?cwd file =
    let cmd = [ "touch"; Fpath.to_string file ] in
    exec_or_fail ?cwd ~name:"touch" cmd

  let echo_to ?(cwd = "./") file content =
    let file = Filename.concat cwd file in
    Lwt_io.(
      with_file ~mode:Output file (fun cout -> Lwt_io.write_line cout content))

  let git ?cwd cmd =
    let cmd = "git" :: "-c" :: "protocol.file.allow=always" :: cmd in
    exec_or_fail ?cwd ~name:"git" cmd

  let git_with ?cwd ~path cmd =
    let cmd = "-C" :: path :: cmd in
    git ?cwd cmd
end

let set_config ~cwd =
  Cmd.git ~cwd [ "config"; "--local"; "user.email"; "test@test.com" ] >>= fun () ->
  Cmd.git ~cwd [ "config"; "--local"; "user.name"; "Test" ]

let init cwd =
  Cmd.cp [ "-R" ] (Fpath.v "./dummy-opam-repository/.") cwd >>= fun () ->
  Cmd.git ~cwd [ "init"; "-q"; "." ] >>= fun () ->
  set_config ~cwd >>= fun () ->
  Cmd.git ~cwd [ "add"; "." ] >>= fun () ->
  Cmd.git ~cwd [ "commit"; "-qm"; "init" ]

let deinit cwd =
  Cmd.rm ~cwd [ "-rf" ] [ Fpath.v "./.git" ]

(** Apply a list of patches in order then commit the result *)
let apply_patches ~cwd cm patches =
  List.fold_left
    (fun pre patch -> pre >>= fun () ->
      Cmd.git ~cwd [ "apply"; patch ])
    Lwt.return_unit
    patches
  >>= fun () ->
  Cmd.git ~cwd [ "add"; "." ] >>= fun () ->
  Cmd.git ~cwd [ "commit"; "-qm"; cm ]
