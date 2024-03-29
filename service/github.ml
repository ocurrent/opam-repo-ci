open Current.Syntax

module Git = Current_git

include Current_github

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Fmt.str "https://opam.ci.ocaml.org/github/%s/%s/commit/%s" owner name hash)

let status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Repo_id.owner; name } = Api.Commit.repo_id head in
  let hash = Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  let lint_status = match result with
    | true, _ -> Some (Api.Status.v `Success ~description:"Passed")
    | false, _ -> None
  in
  let main_status = match result with
    | _, Ok m              -> Api.Status.v ~url `Success ~description:("Passed - "^m)
    | _, Error (`Active _) -> Api.Status.v ~url `Pending
    | _, Error (`Msg m)    -> Api.Status.v ~url `Failure ~description:("Failed - "^m)
  in
  (lint_status, main_status)

  let set_statuses ~head statuses =
    let+ () = Current.option_iter (Api.Commit.set_status head "opam-repo-ci (linter)") (Current.map fst statuses)
    and+ () = Api.Commit.set_status head "opam-ci" (Current.map snd statuses) (* TODO: Change to opam-repo-ci *)
    in
    ()

let update_status ~head state =
  status_of_state ~head state
  |> set_statuses ~head

open Opam_repo_ci

let set_active_installations i =
  let+ i = i in
  List.fold_left
    (fun acc i ->
      Index.Account_set.add (Installation.account i) acc)
    Index.Account_set.empty i
  |> Index.set_active_accounts

let set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  let repo = Api.Repo.id repo in
  Index.set_active_refs ~repo (
    xs |> List.map @@ fun x ->
    let commit = Api.Commit.id x in
    let gref = Git.Commit_id.gref commit in
    let hash = Git.Commit_id.hash commit in
    (gref, hash))
