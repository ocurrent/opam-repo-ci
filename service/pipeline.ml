open Current.Syntax
open Opam_repo_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Common = Opam_repo_ci_api.Common

let master_distro = Dockerfile_distro.resolve_alias Dockerfile_distro.master_distro
let default_compiler = Ocaml_version.with_just_major_and_minor Ocaml_version.Releases.latest

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://opam.ci.ocaml.org/github/%s/%s/commit/%s" owner name hash)

let github_status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok description           -> Github.Api.Status.v ~url `Success ~description
  | Error (`Active `Running) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg description) -> Github.Api.Status.v ~url `Failure ~description

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

module OpamPackage = struct
  include OpamPackage
  let pp = Fmt.of_to_string to_string
end

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

let build_spec ~platform ~upgrade_opam pkg =
  let+ pkg = pkg in
  Build.Spec.opam ~platform ~lower_bounds:false ~with_tests:false ~upgrade_opam pkg

let test_spec ~platform ~upgrade_opam ~after pkg =
  let+ _ = after
  and+ pkg = pkg
  in
  Build.Spec.opam ~platform ~lower_bounds:false  ~with_tests:true ~upgrade_opam pkg

let lower_bounds_spec ~platform ~upgrade_opam ~after pkg =
  let+ _ = after
  and+ pkg = pkg
  in
  Build.Spec.opam ~platform ~lower_bounds:true ~with_tests:false ~upgrade_opam pkg

let revdep_spec ~platform ~upgrade_opam ~revdep pkgopt =
  let+ revdep = revdep
  and+ {PackageOpt.pkg; wait_for_revdeps = _} = pkgopt
  in
  Build.Spec.opam ~platform ~lower_bounds:false ~with_tests:true ~revdep ~upgrade_opam pkg

let combine_revdeps revdeps =
  let map =
    List.fold_left (fun set pkg -> OpamPackage.Set.add pkg set) OpamPackage.Set.empty revdeps
  in
  OpamPackage.Set.elements map

(* List the revdeps of [pkgopt] (using [builder] and [image]) and test each one
   (using [spec] and [base], merging [source] into [master]). *)
let test_revdeps ~ocluster ~upgrade_opam ~master ~base ~platform ~pkgopt ~after:main_build source =
  let revdeps = Build.list_revdeps ~base ocluster ~platform ~pkgopt ~master source in
  let revdeps = Current.map combine_revdeps revdeps in
  let+ tests =
    revdeps
    |> Current.gate ~on:main_build
    |> dep_list_map (module OpamPackage) (fun revdep ->
        let image =
          let spec = revdep_spec ~platform ~upgrade_opam ~revdep pkgopt in
          Build.v ocluster ~label:"build" ~base ~spec ~master source
        in
        let+ label = Current.map OpamPackage.to_string revdep
        and+ build = Node.action `Built image
        in
        Node.leaf ~label build
      )
  and+ list_revdeps = Node.action `Analysed revdeps
  in
  [Node.actioned_branch ~label:"revdeps" list_revdeps tests]

let get_significant_available_pkg = function
  | pkg, Analyse.Analysis.New -> Some {PackageOpt.pkg; wait_for_revdeps = false}
  | pkg, Analyse.Analysis.SignificantlyChanged -> Some {PackageOpt.pkg; wait_for_revdeps = true}
  | _, Analyse.Analysis.(Deleted | UnsignificantlyChanged) -> None

let build_with_cluster ~ocluster ~analysis ~lint ~master source =
  let pkgs = Current.map Analyse.Analysis.packages analysis in
  let pkgs = Current.map (List.filter_map get_significant_available_pkg) pkgs in
  let build ~upgrade_opam ~lower_bounds ~revdeps label variant =
    let arch = Variant.arch variant in
    let pool = Conf.pool_of_arch arch in
    let platform = {Platform.label; pool; variant} in
    let analysis = with_label label analysis in
    let pkgs =
      (* Add fake dependency from pkgs to analysis so that the package being tested appears
         below the platform, to make the diagram look nicer. Ideally, the pulls of the
         base images should be moved to the top (not be per-package at all). *)
      let+ _ = analysis
      and+ pkgs = pkgs in
      pkgs
    in
    pkgs |> dep_list_map ~collapse_key:"pkg" (module PackageOpt) (fun pkgopt ->
        let pkg = Current.map (fun {PackageOpt.pkg; wait_for_revdeps = _} -> pkg) pkgopt in
        let base =
          let+ repo_id =
            Docker.peek ~schedule:weekly ~arch:(Ocaml_version.to_docker_arch arch)
              ("ocaml/opam:" ^ Variant.docker_tag variant)
          in
          Current_docker.Raw.Image.of_hash repo_id
        in
        let image =
          let spec = build_spec ~platform ~upgrade_opam pkg in
          Build.v ocluster ~label:"build" ~base ~spec ~master source in
        let tests =
          let spec = test_spec ~platform ~upgrade_opam pkg ~after:image in
          Build.v ocluster ~label:"test" ~base ~spec ~master source
        in
        let+ pkg = pkg
        and+ build = Node.action `Built image
        and+ tests = Node.action `Built tests
        and+ lower_bounds_check =
          if upgrade_opam && lower_bounds then
            let action =
              let spec = lower_bounds_spec ~platform ~upgrade_opam pkg ~after:image in
              Build.v ocluster ~label:"lower-bounds" ~base ~spec ~master source
            in
            let+ action = Node.action `Built action in
            [Node.leaf ~label:"lower-bounds" action]
          else
            Current.return []
        and+ revdeps =
          if revdeps then test_revdeps ~ocluster ~upgrade_opam ~master ~base ~platform ~pkgopt source ~after:image
          else Current.return []
        in
        let label = OpamPackage.to_string pkg in
        Node.actioned_branch ~label build (
          Node.leaf ~label:"tests" tests ::
          lower_bounds_check @
          revdeps
        )
      )
    |> Current.map (Node.branch ~label)
    |> Current.collapse ~key:"platform" ~value:label ~input:analysis
  in
  let compilers ~upgrade_opam =
    Current.list_seq begin
      let master_distro = Dockerfile_distro.tag_of_distro master_distro in
      (Ocaml_version.Releases.recent @ Ocaml_version.Releases.dev) |>
      List.map (fun v ->
        let v = Ocaml_version.with_just_major_and_minor v in
        let revdeps = Ocaml_version.equal v default_compiler in (* TODO: Remove this when the cluster is ready *)
        let v = Ocaml_version.to_string v in
        let variant = Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:(v, None) in
        build ~upgrade_opam ~lower_bounds:true ~revdeps v variant
      )
    end
  in
  let distributions ~upgrade_opam =
    Current.list_seq begin
      let default_compiler = Ocaml_version.to_string default_compiler in
      Dockerfile_distro.active_distros `X86_64 |>
      List.fold_left (fun acc distro ->
        if Dockerfile_distro.compare distro master_distro = 0 (* TODO: Add Dockerfile_distro.equal *)
        || Dockerfile_distro.os_family_of_distro distro <> `Linux then (* TODO: Unlock this when Windows is ready *)
          acc
        else
          let distro = Dockerfile_distro.tag_of_distro distro in
          let variant = Variant.v ~arch:`X86_64 ~distro ~compiler:(default_compiler, None) in
          build ~upgrade_opam ~lower_bounds:false ~revdeps:false distro variant :: acc
      ) []
    end
  in
  let+ analysis = Node.action `Analysed analysis
  and+ lint = Node.action `Linted lint
  and+ compilers_2_0 = compilers ~upgrade_opam:false
  and+ compilers_2_1 = compilers ~upgrade_opam:true
  and+ distributions_2_0 = distributions ~upgrade_opam:false
  and+ distributions_2_1 = distributions ~upgrade_opam:true
  and+ extras =
    let master_distro = Dockerfile_distro.tag_of_distro master_distro in
    let default_compiler = Ocaml_version.to_string default_compiler in
    let flambda = Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:(default_compiler, Some "flambda") in
    let nnpchecker = Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:(default_compiler, Some "nnpchecker") in
    Current.list_seq (
      build ~upgrade_opam:true ~lower_bounds:false ~revdeps:false "flambda" flambda ::
      build ~upgrade_opam:true ~lower_bounds:false ~revdeps:false "nnpchecker" nnpchecker ::
      List.fold_left (fun acc arch ->
        if arch = `X86_64 then
          acc
        else
          let label = Ocaml_version.to_opam_arch arch in
          let variant = Variant.v ~arch ~distro:master_distro ~compiler:(default_compiler, None) in
          build ~upgrade_opam:true ~lower_bounds:false ~revdeps:false label variant :: acc
      ) [] Ocaml_version.arches
    )
  in
  let opam_2_0 =
    [
      Node.branch ~label:"compilers" compilers_2_0;
      Node.branch ~label:"distributions" distributions_2_0;
    ]
  in
  let opam_2_1 =
    [
      Node.branch ~label:"compilers" compilers_2_1;
      Node.branch ~label:"distributions" distributions_2_1;
      Node.branch ~label:"extras" extras;
    ]
  in
  Node.root [
    Node.leaf ~label:"(analysis)" analysis;
    Node.leaf ~label:"(lint)" lint;
    Node.branch ~label:"opam-2.0" opam_2_0;
    Node.branch ~label:"opam-2.1" opam_2_1;
  ]

let summarise results =
  results
  |> Node.flatten (fun ~label ~job_id:_ ~result -> (label, result))
  |> List.fold_left (fun (ok, pending, err, skip, lint, waiting) -> function
      | _, Ok `Analysed -> (ok, pending, err, skip, lint, waiting)
      | _, Ok `Linted -> (ok, pending, err, skip, lint + 1, waiting)
      | _, Ok `Built -> (ok + 1, pending, err, skip, lint, waiting)
      | _, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, skip + 1, lint, waiting)
      | _, Error `Msg _ -> (ok, pending, err + 1, skip, lint, waiting)
      | _, Error `Active (`Ready | `Running) -> (ok, pending + 1, err, skip, lint, waiting)
      | _, Error `Active `Waiting_for_confirmation -> (ok, pending, err, skip, lint, waiting + 1)
      (* TODO: Find a way to use the labels and error messages to display something more useful *)
    ) (0, 0, 0, 0, 0, 0)
  |> fun (ok, pending, err, skip, lint, waiting) ->
  let lint = if lint > 0 then "ok" else "failed" in
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip, waiting with
    | 0, 0, 0, 0 -> Ok "Passed - No build was necessary"
    | 0, 0, _skip, 0 -> Error (`Msg "Failed - Everything was skipped")
    | ok, 0, 0, 0 -> Ok (Fmt.str "Passed - %d jobs passed" ok)
    | ok, 0, skip, 0 -> Ok (Fmt.str "Passed - %d jobs passed, %d jobs skipped" ok skip)
    | ok, err, skip, 0 -> Error (`Msg (Fmt.str "Failed - %d jobs failed, lint: %s, %d jobs skipped, %d jobs passed" err lint skip ok))
    | ok, err, _skip, waiting -> Error (`Msg (Fmt.str "Waiting for maintainers approval (%d) - %d jobs failed, lint: %s, %d jobs passed" waiting err lint ok))

(* An in-memory-only latch of the last successful value. *)
let latch ~label x =
  let prev = ref (Error (`Active `Ready), None) in
  Current.component "latch %s" label |>
  let> x = Current.state ~hidden:true x in
  Result.iter (fun x -> prev := Ok x, None) x;
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
    let+ refs = refs in
    Github.Api.Ref_map.fold begin fun key head acc ->
      match key with
      | `Ref _ -> acc (* Skip branches, only check PRs *)
      | `PR _ -> head :: acc
    end (Github.Api.all_refs refs) []
  in
  master, prs

let test_repo ~ocluster ~push_status repo =
  let master, prs = get_prs repo in
  let master = latch ~label:"master" master in  (* Don't cancel builds while fetching updates to this *)
  let prs = set_active_refs ~repo prs in
  prs |> Current.list_iter ~collapse_key:"pr" (module Github.Api.Commit) @@ fun head ->
  let commit_id = Current.map Github.Api.Commit.id head in
  let src = Git.fetch commit_id in
  let analysis = Analyse.examine ~master src in
  let lint =
    let packages = Current.map Analyse.Analysis.packages analysis in
    Lint.check ~master ~packages src
  in
  let builds = build_with_cluster ~ocluster ~analysis ~lint ~master commit_id in
  let summary = Current.map summarise builds in
  let status =
    let+ summary = summary in
    match summary with
    | Ok _ -> `Passed
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
  and set_github_status =
    summary
    |> github_status_of_state ~head
    |> (if push_status then Github.Api.Commit.set_status head "opam-ci"
        else Current.ignore_value)
  in
  Current.all [index; set_github_status]

let local_test ~ocluster repo () =
  let { Github.Repo_id.owner; name = _ } = Github.Api.Repo.id repo in
  Index.set_active_accounts @@ Index.Account_set.singleton owner;
  let ocluster = Build.config ~timeout:Conf.build_timeout ocluster in
  test_repo ~ocluster ~push_status:false (Current.return repo)

let v ~ocluster ~app () =
  let ocluster = Build.config ~timeout:Conf.build_timeout ocluster in
  let installations = Github.App.installations app |> set_active_installations in
  installations |> Current.list_iter (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter (module Github.Api.Repo) @@ fun repo ->
  test_repo ~ocluster ~push_status:(Conf.profile = `Production) repo
