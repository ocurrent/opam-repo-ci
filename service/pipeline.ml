open Current.Syntax
open Opam_repo_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Common = Opam_repo_ci_api.Common

let master_distro = (Dockerfile_distro.resolve_alias Dockerfile_distro.master_distro :> Dockerfile_distro.t)
let default_compiler_full = Ocaml_version.Releases.latest
let default_compiler = Ocaml_version.with_just_major_and_minor default_compiler_full

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
  | Ok m              -> Github.Api.Status.v ~url `Success ~description:("Passed - "^m)
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:("Failed - "^m)

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

let build_spec ~platform ~opam_version pkg =
  let+ pkg = pkg in
  Build.Spec.opam ~platform ~lower_bounds:false ~with_tests:false ~opam_version pkg

let test_spec ~platform ~opam_version pkg =
  let+ pkg = pkg in
  Build.Spec.opam ~platform ~lower_bounds:false ~with_tests:true ~opam_version pkg

let lower_bounds_spec ~platform ~opam_version pkg =
  let+ pkg = pkg in
  Build.Spec.opam ~platform ~lower_bounds:true ~with_tests:false ~opam_version pkg

let revdep_spec ~platform ~opam_version ~revdep pkg =
  let+ revdep = revdep
  and+ pkg = pkg
  in
  Build.Spec.opam ~platform ~lower_bounds:false ~with_tests:true ~revdep ~opam_version pkg

(* List the revdeps of [pkg] (using [builder] and [image]) and test each one
   (using [spec] and [base], merging [source] into [master]). *)
let test_revdeps ~ocluster ~opam_version ~master ~base ~platform ~pkgopt ~after source =
  let revdeps =
    Build.list_revdeps ~base ocluster ~platform ~pkgopt ~master ~after source |>
    Current.map OpamPackage.Set.elements
  in
  let pkg = Current.map (fun {PackageOpt.pkg = pkg; urgent = _} -> pkg) pkgopt in
  let urgent = Current.map (fun {PackageOpt.pkg = _; urgent} -> urgent) pkgopt in
  let+ tests =
    revdeps
    |> dep_list_map (module OpamPackage) (fun revdep ->
        let image =
          let spec = revdep_spec ~platform ~opam_version ~revdep pkg in
          Build.v ocluster ~label:"build" ~base ~spec ~master ~urgent source
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
  | pkg, Analyse.Analysis.New -> Some {PackageOpt.pkg; urgent = None}
  | pkg, Analyse.Analysis.SignificantlyChanged -> Some {PackageOpt.pkg; urgent = Some (fun (`High | `Low) -> false)}
  | _, Analyse.Analysis.(Deleted | UnsignificantlyChanged) -> None

let build_with_cluster ~ocluster ~analysis ~lint ~master source =
  let pkgs = Current.map Analyse.Analysis.packages analysis in
  let pkgs = Current.map (List.filter_map get_significant_available_pkg) pkgs in
  let build ~opam_version ~lower_bounds ~revdeps label variant =
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
        let pkg = Current.map (fun {PackageOpt.pkg; urgent = _} -> pkg) pkgopt in
        let urgent = Current.return None in
        let base =
          let+ repo_id =
            Docker.peek ~schedule:weekly ~arch:(Ocaml_version.to_docker_arch arch)
              ("ocaml/opam:" ^ Variant.docker_tag variant)
          in
          Current_docker.Raw.Image.of_hash repo_id
        in
        let image =
          let spec = build_spec ~platform ~opam_version pkg in
          Build.v ocluster ~label:"build" ~base ~spec ~master ~urgent source in
        let tests =
          let spec = test_spec ~platform ~opam_version pkg in
          Build.v ocluster ~label:"test" ~base ~spec ~master ~urgent source
        in
        let+ pkg = pkg
        and+ build = Node.action `Built image
        and+ tests = Node.action `Built tests
        and+ lower_bounds_check =
          match opam_version, lower_bounds with
          | `V2_1, true ->
            let action =
              let spec = lower_bounds_spec ~platform ~opam_version pkg in
              Build.v ocluster ~label:"lower-bounds" ~base ~spec ~master ~urgent source
            in
            let+ action = Node.action `Built action in
            [Node.leaf ~label:"lower-bounds" action]
          | `V2_0, true
          | (`V2_1 | `V2_0), false ->
            Current.return []
        and+ revdeps =
          if revdeps then test_revdeps ~ocluster ~opam_version ~master ~base ~platform ~pkgopt source ~after:image
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
  let compilers ~opam_version =
    Current.list_seq begin
      let master_distro = Dockerfile_distro.tag_of_distro master_distro in
      (Ocaml_version.Releases.recent @ Ocaml_version.Releases.unreleased_betas) |>
      List.map (fun v ->
        let v = Ocaml_version.with_just_major_and_minor v in
        let revdeps = Ocaml_version.equal v default_compiler in (* TODO: Remove this when the cluster is ready *)
        let v = Ocaml_version.to_string v in
        let variant = Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:(v, None) in
        build ~opam_version ~lower_bounds:true ~revdeps v variant
      )
    end
  in
  let distributions ~opam_version =
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
          build ~opam_version ~lower_bounds:false ~revdeps:false distro variant :: acc
      ) []
    end
  in
  let+ analysis = Node.action `Analysed analysis
  and+ lint = Node.action `Linted lint
  and+ compilers_2_0 = compilers ~opam_version:`V2_0
  and+ compilers_2_1 = compilers ~opam_version:`V2_1
  and+ distributions_2_0 = distributions ~opam_version:`V2_0
  and+ distributions_2_1 = distributions ~opam_version:`V2_1
  and+ extras =
    let master_distro = Dockerfile_distro.tag_of_distro master_distro in
    let default_comp = Ocaml_version.to_string default_compiler in
    Current.list_seq (
      (* TODO: Remove this when OCaml 5.00 is merged and available in a docker image *)
      build ~opam_version:`V2_1 ~lower_bounds:false ~revdeps:false "domains" (Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:("4.12", Some "domains")) ::
      build ~opam_version:`V2_1 ~lower_bounds:false ~revdeps:false "domains-effects" (Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:("4.12", Some "domains-effects")) ::
      List.filter_map (fun v ->
        match Ocaml_version.extra v with
        | None -> None
        | Some label ->
            (* TODO: This should be in ocaml-version or ocaml-dockerfile *)
            (* TODO: The same code is used in docker-base-images *)
            let label = String.map (function '+' -> '-' | c -> c) label in
            let variant = Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:(default_comp, Some label) in
            Some (build ~opam_version:`V2_1 ~lower_bounds:false ~revdeps:false label variant)
      ) (Ocaml_version.Opam.V2.switches `X86_64 default_compiler_full) @
      List.filter_map (function
        | `X86_64 -> None
        | arch ->
            let label = Ocaml_version.to_opam_arch arch in
            let variant = Variant.v ~arch ~distro:master_distro ~compiler:(default_comp, None) in
            Some (build ~opam_version:`V2_1 ~lower_bounds:false ~revdeps:false label variant)
      ) Ocaml_version.arches
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
  let results = Node.flatten (fun ~job_id:_ ~result -> result) results in
  let (ok, pending, err, skip, lint) =
    Index.Job_map.fold (fun _ result (ok, pending, err, skip, lint) ->
      match result with
      | Ok `Analysed -> (ok, pending, err, skip, lint)
      | Ok `Linted -> (ok, pending, err, skip, lint + 1)
      | Ok `Built -> (ok + 1, pending, err, skip, lint)
      | Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, skip + 1, lint)
      | Error `Msg _ -> (ok, pending, err + 1, skip, lint)
      | Error `Active _ -> (ok, pending + 1, err, skip, lint)
      (* TODO: Find a way to use the labels and error messages to display something more useful *)
    ) results (0, 0, 0, 0, 0)
  in
  let lint = if lint > 0 then "ok" else "failed" in
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, 0, 0 -> Ok "No build was necessary"
    | 0, 0, _skip -> Error (`Msg "Everything was skipped")
    | ok, 0, 0 -> Ok (Fmt.str "%d jobs passed" ok)
    | ok, 0, skip -> Ok (Fmt.str "%d jobs passed, %d jobs skipped" ok skip)
    | ok, err, skip -> Error (`Msg (Fmt.str "%d jobs failed, lint: %s, %d jobs skipped, %d jobs passed" err lint skip ok))

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
    and+ jobs = Current.map (Node.flatten (fun ~job_id ~result:_ -> job_id)) builds
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
