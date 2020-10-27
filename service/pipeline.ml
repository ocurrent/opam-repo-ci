open Current.Syntax
open Opam_repo_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Common = Opam_repo_ci_api.Common

let default_compiler = "4.10"

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "http://147.75.80.95/github/%s/%s/commit/%s" owner name hash)

let _github_status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg _)    -> Github.Api.Status.v ~url `Failure ~description:"Failed"

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

module OpamPackage = struct
  include OpamPackage
  let pp = Fmt.of_to_string to_string
end

let list_revdeps ~builder ~image ~pkg =
  Current.component "list revdeps" |>
  let> pkg = pkg
  and> image = image in
  let args = ["opam";"list";"-s";"--color=never";"--depends-on";OpamPackage.to_string pkg;"--installable";"--all-versions";"--depopts"] in
  Builder.pread builder ~args image
  |> Current.Primitive.map_result (Result.map (fun output ->
      String.split_on_char '\n' output |>
      List.filter_map (function
          | "" -> None
          | pkg -> Some (OpamPackage.of_string pkg)
        )
    ))

let with_label l t =
  Current.component "%s" l |>
  let> v = t in
  Current.Primitive.const v

(* [dep_list_map] is like [Current.list_map], except that the output is fixed to the empty list until
   the input is successful. You must ensure that the status of the input is reported elsewhere. *)
let dep_list_map (type a) (module M : Current_term.S.ORDERED with type t = a) ?collapse_key f input =
  let results = Current.list_map ?collapse_key (module M) f input in
  let+ state = Current.state ~hidden:true results
  and+ input = Current.state ~hidden:true input
  in
  match input, state with
  | Error _, _ -> []
  | Ok _, Ok x -> x
  (* The results of a [dep_list_map] are nodes, so they should always be ready and successful. *)
  | Ok _, Error (`Msg m) -> failwith m
  | Ok _, Error (`Active _) ->
    Logs.warn (fun f -> f "dep_list_map: input is ready but output is pending!");
    []

(* List the revdeps of [pkg] (using [builder] and [image]) and test each one
   (using [spec] and [base], merging [source] into [master]). *)
let test_revdeps ~builder ~image ~master ~base ~spec ~pkg source =
  let revdeps = list_revdeps ~builder ~image ~pkg in
  let+ list_op = Node.of_job `Checked revdeps ~label:"list revdeps"
  and+ tests =
    revdeps
    |> dep_list_map (module OpamPackage) (fun revdep ->
        let image = Build.v ~base ~spec ~revdep ~with_tests:false ~pkg ~master source in
        let tests = Build.v ~base ~spec ~revdep ~with_tests:true ~pkg ~master source in
        let+ label = Current.map OpamPackage.to_string revdep
        and+ build = Node.of_job `Built image ~label:"build"
        and+ tests = Node.of_job `Built tests ~label:"tests"
        in
        Node.branch ~label [build; tests]
      )
  in
  [Node.branch ~label:"revdeps" (list_op :: tests)]

let build_with_docker ~analysis ~master source =
  let pkgs = Current.map Analyse.Analysis.packages analysis in
  let build ~revdeps label builder variant =
    let analysis = with_label variant analysis in
    let spec =
      let platform = {Platform.label; builder; variant} in
      let+ analysis = analysis in
      Build.Spec.opam ~label:variant ~platform ~analysis `Build
    in
    let pkgs =
      (* Add fake dependency from pkgs to spec so that the package being tested appears
         below the platform, to make the diagram look nicer. Ideally, the pulls of the
         base images should be moved to the top (not be per-package at all). *)
      let+ _ = spec
      and+ pkgs = pkgs in
      pkgs
    in
    pkgs |> dep_list_map ~collapse_key:"pkg" (module OpamPackage) (fun pkg ->
        let base = Build.pull ~schedule:weekly spec in
        let image =
          Build.v ~base ~spec ~with_tests:false ~pkg ~master source in
        let tests =
          let pkg = pkg |> Current.gate ~on:(Current.ignore_value image) in
          Build.v ~base ~spec ~with_tests:true ~pkg ~master source
        in
        let+ label = Current.map OpamPackage.to_string pkg
        and+ build = Node.of_job `Built image ~label:"build"
        and+ tests = Node.of_job `Built tests ~label:"tests"
        and+ revdeps =
          if revdeps then test_revdeps ~builder ~image ~master ~base ~spec ~pkg source
          else Current.return []
        in
        Node.branch ~label (build :: tests :: revdeps)
      )
    |> Current.map (Node.branch ~label)
    |> Current.collapse ~key:"platform" ~value:label ~input:analysis
  in
  let+ analysis = Node.of_job `Checked analysis ~label:"(analysis)"
  and+ compilers = Current.list_seq [
      build ~revdeps:true "4.11" Conf.Builder.amd1 "debian-10-ocaml-4.11";
      build ~revdeps:true "4.10" Conf.Builder.amd1 "debian-10-ocaml-4.10";
      build ~revdeps:true "4.09" Conf.Builder.amd1 "debian-10-ocaml-4.09";
      build ~revdeps:true "4.08" Conf.Builder.amd1 "debian-10-ocaml-4.08";
      build ~revdeps:true "4.07" Conf.Builder.amd1 "debian-10-ocaml-4.07";
      build ~revdeps:true "4.06" Conf.Builder.amd1 "debian-10-ocaml-4.06";
      build ~revdeps:true "4.05" Conf.Builder.amd1 "debian-10-ocaml-4.05";
      build ~revdeps:true "4.04" Conf.Builder.amd1 "debian-10-ocaml-4.04";
      build ~revdeps:true "4.03" Conf.Builder.amd1 "debian-10-ocaml-4.03";
      build ~revdeps:true "4.02" Conf.Builder.amd1 "debian-10-ocaml-4.02";
    ]
  and+ distributions = Current.list_seq [
      build ~revdeps:false "alpine-3.11" Conf.Builder.amd1 ("alpine-3.11-ocaml-"^default_compiler);
      build ~revdeps:false "debian-testing" Conf.Builder.amd1 ("debian-testing-ocaml-"^default_compiler);
      build ~revdeps:false "debian-unstable" Conf.Builder.amd1 ("debian-unstable-ocaml-"^default_compiler);
      build ~revdeps:false "centos-8" Conf.Builder.amd1 ("centos-8-ocaml-"^default_compiler);
      build ~revdeps:false "fedora-31" Conf.Builder.amd1 ("fedora-31-ocaml-"^default_compiler);
      build ~revdeps:false "opensuse-15.1" Conf.Builder.amd1 ("opensuse-15.1-ocaml-"^default_compiler);
      build ~revdeps:false "ubuntu-18.04" Conf.Builder.amd1 ("ubuntu-18.04-ocaml-"^default_compiler);
      build ~revdeps:false "ubuntu-20.04" Conf.Builder.amd1 ("ubuntu-20.04-ocaml-"^default_compiler);
    ]
  and+ extras = Current.list_seq [
      build ~revdeps:false "flambda" Conf.Builder.amd1 ("debian-10-ocaml-"^default_compiler^"-flambda");
    ]
  in
  Node.root [
    analysis;
    Node.branch ~label:"compilers" compilers;
    Node.branch ~label:"distributions" distributions;
    Node.branch ~label:"extras" extras;
  ]

let list_errors ~ok errs =
  let groups =  (* Group by error message *)
    List.sort compare errs |> List.fold_left (fun acc (msg, l) ->
        match acc with
        | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
        | _ -> (msg, [l]) :: acc
      ) []
  in
  Error (`Msg (
      match groups with
      | [] -> "No builds at all!"
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") pp_label) errs
    ))

let summarise results =
  results
  |> Node.flatten (fun ~label ~job_id:_ ~result -> (label, result))
  |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok ()                     (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)

let get_prs repo =
  let refs =
    Current.component "Get PRs" |>
    let> (api, repo) = repo in
    Github.Api.refs api repo
  in
  let master =
    refs
    |> Current.map (Github.Api.Ref_map.find (`Ref "refs/heads/master"))
    |> Current.map Github.Api.Commit.id
    |> with_label "master"
    |> Git.fetch
  in
  let prs =
    let+ refs = refs in
    Github.Api.Ref_map.fold begin fun key head acc ->
      match key with
      | `Ref _ -> acc (* Skip branches, only check PRs *)
      | `PR _ -> head :: acc
    end refs []
  in
  master, prs

let local_test repo () =
  let src = Git.Local.head_commit repo in
  let master = Git.Local.commit_of_ref repo "refs/remotes/origin/master" in
  let analysis = Analyse.examine ~master src in
  Current.component "summarise" |>
  let** result =
    build_with_docker ~analysis ~master src
    (* |> Current.map (fun x -> Fmt.pr "%a@." Node.dump x; x) *)
    |> Current.map summarise
  in
  Current.of_output result

let v ~app () =
  Github.App.installations app |> Current.list_iter (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter (module Github.Api.Repo) @@ fun repo ->
  let master, prs = get_prs repo in
  let prs = set_active_refs ~repo prs in
  prs |> Current.list_iter ~collapse_key:"pr" (module Github.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let analysis = Analyse.examine ~master src in
  let builds = build_with_docker ~analysis ~master src in
  let summary = Current.map summarise builds in
  let status =
    let+ summary = summary in
    match summary with
    | Ok () -> `Passed
    | Error (`Active `Running) -> `Pending
    | Error (`Msg _) -> `Failed
  in
  let index =
    let+ commit = head
    and+ jobs = Current.map (Node.flatten (fun ~label ~job_id ~result:_ -> (label, job_id))) builds
    and+ status = status in
    let repo = Current_github.Api.Commit.repo_id commit in
    let hash = Current_github.Api.Commit.hash commit in
    Index.record ~repo ~hash ~status jobs
  in index
    (*
  and set_github_status =
    summary
    |> github_status_of_state ~head
    |> Github.Api.Commit.set_status head "opam-ci"
  in
  Current.all [index; set_github_status]
       *)
