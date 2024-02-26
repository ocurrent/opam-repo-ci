module Lint = Opam_repo_ci.Lint
module Analyse = Opam_repo_ci.Analyse

open Lwt.Infix
open Current.Syntax

(* Use [Lwt_stream] to make state in the pipeline observable while it's running *)
let results, push_result = Lwt_stream.create ()

let pipeline repo_dir () =
  let open Current_git in
  let repo = Local.v repo_dir in
  let master = Local.commit_of_ref repo "refs/heads/master" in
  let other = Local.commit_of_ref repo "refs/heads/new-branch" in
  let packages =
    Current.map Analyse.Analysis.packages @@ Analyse.examine ~master other
  in
  let result = Lint.check ~host_os:"macos" ~master ~packages other in
  let+ result = Current.catch result in
  push_result @@ Option.some result

open Git_util

let check_result label expected stream =
  Lwt_stream.get stream >|= fun value ->
  let value = Option.map (Result.map_error (fun (`Msg s) -> s)) value in
  Alcotest.(check (option (result unit string)) label (Some expected) value)

let with_temp_repo f =
  Lwt_io.with_temp_dir ~prefix:"dummy-opam-repository-" @@
  fun repo_dir ->
  let repo_dir = Fpath.v repo_dir in
  init repo_dir >>= fun () ->
  apply_patches ~cwd:repo_dir "a-1" [ "a-1.patch" ] >>= fun () ->
  Cmd.git ~cwd:repo_dir [ "checkout"; "-qb"; "new-branch" ] >>= fun () ->
  f repo_dir

let test_correct cwd =
  apply_patches ~cwd "b-correct" [ "b-correct.patch" ] >>= fun () ->
  check_result "Correct" (Ok ()) results >>= fun () ->
  Cmd.git ~cwd [ "reset"; "--hard"; "HEAD~1" ]

(** Tests the following:
    - [b.0.0.1] is missing the [author] field
    - [b.0.0.2] has an extra unknown field
    - [b.0.0.3] is correct *)
let test_incorrect_opam cwd =
  apply_patches ~cwd "b-incorrect-opam" [ "b-incorrect-opam.patch" ] >>= fun () ->
  check_result "Incorrect opam" (Error "2 errors") results >>= fun () ->
  Cmd.git ~cwd [ "reset"; "--hard"; "HEAD~1" ]

(** Tests the package name collision detection by adding four versions
    of a package [a_1] that conflicts with the existing [a-1] package *)
let test_name_collision cwd =
  apply_patches ~cwd "a_1-name-collision" [ "a_1-name-collision.patch" ] >>= fun () ->
  check_result "Package name collision" (Error "4 errors") results >>= fun () ->
  Cmd.git ~cwd[ "reset"; "--hard"; "HEAD~1" ]

let run_tests _switch () =
  with_temp_repo @@ fun repo_dir ->
  let _engine = Current.Engine.create @@ pipeline repo_dir in
  test_correct repo_dir >>= fun () ->
  test_incorrect_opam repo_dir >>= fun () ->
  test_name_collision repo_dir

(** Tests are marked [`Slow] as the [Lint.check] call can take a few seconds *)
let tests = [
    Alcotest_lwt.test_case "lint" `Slow run_tests;
  ]
