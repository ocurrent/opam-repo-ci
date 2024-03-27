open Current.Syntax
open Opam_repo_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Common = Opam_repo_ci_api.Common
module Distro = Dockerfile_opam.Distro

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Fmt.str "https://opam.ci.ocaml.org/github/%s/%s/commit/%s" owner name hash)

let github_status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  let lint_status = match result with
    | true, _ -> Some (Github.Api.Status.v `Success ~description:"Passed")
    | false, _ -> None
  in
  let main_status = match result with
    | _, Ok m              -> Github.Api.Status.v ~url `Success ~description:("Passed - "^m)
    | _, Error (`Active _) -> Github.Api.Status.v ~url `Pending
    | _, Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:("Failed - "^m)
  in
  (lint_status, main_status)

let set_active_installations installations =
  let+ installations = installations in
  installations
  |> List.fold_left (fun acc i -> Index.Account_set.add (Github.Installation.account i) acc) Index.Account_set.empty
  |> Index.set_active_accounts;
  installations

let set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  let repo = Github.Api.Repo.id repo in
  Index.set_active_refs ~repo (
    xs |> List.map @@ fun x ->
    let commit = Github.Api.Commit.id x in
    let gref = Git.Commit_id.gref commit in
    let hash = Git.Commit_id.hash commit in
    (gref, hash)
  );
  xs

module Results : sig
  type t = Index.job_ids Current.t * Summary.t Current.t
  val empty : t
  val merge : t -> t -> t
end = struct
  type t = Index.job_ids Current.t * Summary.t Current.t

  let empty = (Current.return Index.Job_map.empty, Current.return Summary.empty)

  let map2 f x y =
    let+ x = x
    and+ y = y
    in f x y

  let merge (j1, s1) (j2, s2) =
    let impossible name _ _ = Printf.ksprintf failwith "Two jobs have the same name %S" name in
    map2 (Index.Job_map.union impossible) j1 j2,
    map2 Summary.merge s1 s2
end

(* An in-memory-only latch of the last successful value. *)
let latch ~label x =
  let prev = ref (Error (`Active `Ready), None) in
  Current.component "latch %s" label |>
  let> x = Current.state ~hidden:true x
  and> meta = Current.Analysis.metadata x in
  Result.iter (fun x -> prev := Ok x, meta) x;
  Current_incr.const !prev

let get_prs repo =
  let refs =
    Current.component "Get PRs" |>
    let> (api, repo) = repo in
    Github.Api.refs api repo
  in
  let master =
    refs
    |> Current.map (fun refs ->
      Github.Api.all_refs refs |>
      Github.Api.Ref_map.find (`Ref (Github.Api.default_ref refs)))
    |> Current.map Github.Api.Commit.id
    |> Git.fetch
  in
  let prs =
    let+ refs in
    Github.Api.Ref_map.fold (fun key head acc ->
      match key with
      | `Ref _ -> acc (* Skip branches, only check PRs *)
      | `PR _ -> head :: acc
    ) (Github.Api.all_refs refs) []
  in
  master, prs

let analyse ~master src =
  Analyse.examine ~master src
  |> Current.cutoff ~eq:Analyse.Analysis.equal

let flatten builds =
  let f ~label kind job =
    let db_record =
      let+ variant = label
      and+ job_id = Node.job_id job
      in
      Index.Job_map.singleton variant job_id
    in
    let result =
      let+ _ = job in
      kind
    in
    Current.return (db_record, Summary.of_current result)
  in
  Node.flatten builds
    ~map:{ Node.f }
    ~merge:Results.merge
    ~empty:Results.empty

let summarise ~repo ~hash builds =
  let* (jobs, summary) = flatten builds in
  let+ () =
    let+ jobs = jobs
    and+ repo = repo
    and+ hash = hash in
    Index.record ~repo ~hash jobs
  and+ summary =
    let+ summary = summary
    and+ { Current_github.Repo_id.owner; name } = repo
    and+ hash = hash in
    let summary_str = Summary.to_string summary in
    let status =
      match summary_str with
      | _, Ok _ -> `Passed
      | _, Error (`Active `Running) -> `Pending
      | _, Error (`Msg _) -> `Failed
    in
    Index.set_status ~owner ~name ~hash (status, summary);
    summary_str
  in
  summary

let test_pr ~ocluster ~master head =
  let repo = Current.map Current_github.Api.Commit.repo_id head in
  let commit_id = Current.map Github.Api.Commit.id head in
  let hash = Current.map Git.Commit_id.hash commit_id in
  let src = Git.fetch commit_id in
  let latest_analysis = analyse ~master src in
  let analysis = latest_analysis |> latch ~label:"analysis" (* ignore errors from a rerun *) in
  let lint =
    let packages = Current.map Analyse.Analysis.packages analysis in
    Lint.check ~host_os:Conf.host_os ~master ~packages src
  in
  let builds =
    Node.root
      (Node.leaf ~label:"(analysis)" (Node.action `Analysed latest_analysis)
      :: Build.with_cluster ~ocluster ~analysis ~lint ~master commit_id)
  in
  summarise ~repo ~hash builds

let github_set_statuses ~head statuses =
  let+ () = Current.option_iter (Github.Api.Commit.set_status head "opam-repo-ci (linter)") (Current.map fst statuses)
  and+ () = Github.Api.Commit.set_status head "opam-ci" (Current.map snd statuses) (* TODO: Change to opam-repo-ci *)
  in
  ()

let test_repo ~ocluster ~push_status repo =
  let master, prs = get_prs repo in
  let master = latch ~label:"master" master in  (* Don't cancel builds while fetching updates to this *)
  let prs = set_active_refs ~repo prs in
  prs |> Current.list_iter ~collapse_key:"pr" (module Github.Api.Commit) @@ fun head ->
    test_pr ~ocluster ~master head
    |> github_status_of_state ~head
    |> (if push_status then github_set_statuses ~head
        else Current.ignore_value)

let set_metrics_primary_repo repo =
  let repo = Current.map Current_github.Api.Repo.id repo in
  let+ repo = repo in
  Metrics.set_primary_repo repo

let v ~ocluster ~app () =
  let ocluster = Cluster_build.config ~timeout:Conf.build_timeout ocluster in
  let installations = Github.App.installations app |> set_active_installations in
  installations |> Current.list_iter (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter (module Github.Api.Repo) @@ fun repo ->
  Current.all [
    set_metrics_primary_repo repo;
    test_repo ~ocluster ~push_status:(Conf.profile = `Production) repo
  ]

let set_index_local ~repo gref hash =
  let+ repo
  and+ hash in
  Index.(set_active_accounts @@ Account_set.singleton repo.Github.Repo_id.owner);
  Index.set_active_refs ~repo [(gref, hash)]

let local_test_pr ?test_config repo pr_branch () =
  let master = Git.Local.commit_of_ref repo "refs/heads/master" in
  let pr_gref = Printf.sprintf "refs/heads/%s" pr_branch in
  let pr_branch = Git.Local.commit_of_ref repo pr_gref in
  let pr_branch_id = Current.map Git.Commit.id pr_branch in
  let analysis = analyse ~master pr_branch in
  let lint =
    let packages = Current.map Analyse.Analysis.packages analysis in
    Lint.check ?test_config ~host_os:Conf.host_os ~master ~packages pr_branch
  in
  let builds =
    Node.root
      (Node.leaf ~label:"(analysis)" (Node.action `Analysed analysis)
      :: Build.with_docker ~host_arch:Conf.host_arch ~analysis ~lint ~master pr_branch_id)
  in
  let dummy_repo =
    Current.return { Github.Repo_id.owner = "local-owner"; name = "local-repo" }
  in
  let pr_hash = Current.map Git.Commit.hash pr_branch in
  (let+ _ = set_index_local ~repo:dummy_repo pr_gref pr_hash
  and+ result = summarise ~repo:dummy_repo ~hash:pr_hash builds in
  result)
  |> Current.ignore_value
