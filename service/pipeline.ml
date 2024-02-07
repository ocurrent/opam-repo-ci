open Current.Syntax
open Opam_repo_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Common = Opam_repo_ci_api.Common
module Distro = Dockerfile_opam.Distro

let master_distro = (Distro.resolve_alias Distro.master_distro :> Distro.t)
let default_compilers_full = Ocaml_version.Releases.[v4_14; Ocaml_version.Releases.latest] (* NOTE: Should probably stay with list length 2 *)
let default_compilers = List.map Ocaml_version.with_just_major_and_minor default_compilers_full
let opam_version = `Dev

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

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

module OpamPackage = struct
  include OpamPackage
  let pp = Fmt.of_to_string to_string
end

let with_label l t =
  Current.component "%s" l |>
  let> v = t in
  Current.Primitive.const v

let build_spec ~platform ~opam_version pkg =
  let+ pkg = pkg in
  Spec.opam ~platform ~lower_bounds:false ~with_tests:false ~opam_version pkg

let test_spec ~platform ~opam_version pkg =
  let+ pkg = pkg in
  Spec.opam ~platform ~lower_bounds:false ~with_tests:true ~opam_version pkg

let lower_bounds_spec ~platform ~opam_version pkg =
  let+ pkg = pkg in
  Spec.opam ~platform ~lower_bounds:true ~with_tests:false ~opam_version pkg

let revdep_spec ~platform ~opam_version ~revdep pkg =
  let+ revdep = revdep
  and+ pkg = pkg
  in
  Spec.opam ~platform ~lower_bounds:false ~with_tests:true ~revdep ~opam_version pkg

(* List the revdeps of [pkg] (using [builder] and [image]) and test each one
   (using [spec] and [base], merging [source] into [master]). *)
let test_revdeps ~ocluster ~opam_version ~master ~base ~platform ~pkgopt ~after source =
  let revdeps =
    Cluster_build.list_revdeps ~opam_version ~base ocluster ~platform ~pkgopt ~master ~after source
    |> Current.map OpamPackage.Set.elements
  in
  let pkg = Current.map (fun {PackageOpt.pkg = pkg; urgent = _; has_tests = _} -> pkg) pkgopt in
  let urgent = Current.map (fun {PackageOpt.pkg = _; urgent; has_tests = _} -> urgent) pkgopt in
  let tests =
    revdeps
    |> Node.list_map (module OpamPackage) (fun revdep ->
        let image =
          let spec = revdep_spec ~platform ~opam_version ~revdep pkg in
          Cluster_build.v ocluster ~label:"build" ~base ~spec ~master ~urgent source
        in
        let label = Current.map OpamPackage.to_string revdep
        and build = Node.action `Built image
        in
        Node.leaf_dyn ~label build
      )
  and list_revdeps = Node.action `Analysed revdeps
  in
  Node.actioned_branch ~label:"revdeps" list_revdeps [tests]

let get_significant_available_pkg = function
  | pkg, {Analyse.Analysis.kind = New; has_tests} -> Some {PackageOpt.pkg; urgent = None; has_tests}
  | pkg, {Analyse.Analysis.kind = SignificantlyChanged; has_tests} -> Some {PackageOpt.pkg; urgent = Some (fun (`High | `Low) -> false); has_tests}
  | _, {Analyse.Analysis.kind = Deleted | Unavailable | UnsignificantlyChanged; _} -> None

module Build_with = struct
  let compilers ~arch ~build =
    let master_distro = Distro.tag_of_distro master_distro in
    (* The last eight releases of OCaml plus latest beta / release-candidate for each unreleased version. *)
    (* 4.02 -> 5.0 plus 5.1~alpha *)
    (Ocaml_version.Releases.recent @ Ocaml_version.Releases.unreleased_betas) |>
    List.map (fun v ->
      let v = Ocaml_version.with_just_major_and_minor v in
      let revdeps = List.exists (Ocaml_version.equal v) default_compilers in (* TODO: Remove this when the cluster is ready *)
      let v = Ocaml_version.to_string v in
      let variant = Variant.v ~arch ~distro:master_distro ~compiler:(v, None) in
      build ~opam_version ~lower_bounds:true ~revdeps v variant
    )

  let linux_distributions ~arch ~build =
    let build ~distro ~arch ~compiler =
      let variant = Variant.v ~arch ~distro ~compiler in
      let label = Fmt.str "%s-ocaml-%s" distro (Variant.pp_ocaml_version variant) in
      build ~opam_version ~lower_bounds:false ~revdeps:false label variant
    in
    List.fold_left (fun acc comp ->
      let comp = Ocaml_version.to_string comp in
      List.fold_left (fun acc distro ->
        match distro with
        | `CentOS `V7 | `OracleLinux _ (* TODO: Too annoying to support. Remove when it has been removed in ocaml-dockerfile *)
        | _ when Distro.compare distro master_distro = 0 (* TODO: Add Distro.equal *)
              || Distro.os_family_of_distro distro <> `Linux -> (* TODO: Unlock this when Windows is ready *)
            acc
        | _ ->
            let distro = Distro.tag_of_distro distro in
            build ~arch ~distro ~compiler:(comp, None) :: acc
      ) acc (Distro.active_distros arch)
    ) [] default_compilers

  let macos ~build =
    let build ~distro ~arch ~compiler =
      let variant = Variant.v ~arch ~distro ~compiler in
      let label = Fmt.str "%s-%s" (Variant.docker_tag variant) (Ocaml_version.string_of_arch arch) in
      build ~opam_version ~lower_bounds:false ~revdeps:false label variant
    in
    let homebrew = Variant.macos_homebrew in
    List.fold_left (fun acc comp ->
      let comp = Ocaml_version.to_string comp in
      List.fold_left (fun acc arch ->
        build ~distro:homebrew ~arch ~compiler:(comp, None) :: acc
      ) acc [`Aarch64; `X86_64]
    ) [] default_compilers

  let freebsd ~build =
    let build ~distro ~arch ~compiler =
      let variant = Variant.v ~arch ~distro ~compiler in
      let label = Fmt.str "%s-%s" (Variant.docker_tag variant) (Ocaml_version.string_of_arch arch) in
      build ~opam_version ~lower_bounds:false ~revdeps:false label variant
    in
    let freebsd = Variant.freebsd in
    List.fold_left (fun acc comp ->
      let comp = Ocaml_version.to_string comp in
      List.fold_left (fun acc arch ->
        build ~distro:freebsd ~arch ~compiler:(comp, None) :: acc
      ) acc [`X86_64]
    ) [] default_compilers

  (** Non-linux-x86_64 compiler variants. eg ls390x, arm64, flambda, afl etc *)
  let extras ~build =
    let build ~opam_version ~distro ~arch ~compiler label =
      let variant = Variant.v ~arch ~distro ~compiler in
      let label = if String.equal label "" then "" else label^"-" in
      let label = Fmt.str "%socaml-%s" label (Variant.pp_ocaml_version variant) in
      build ~opam_version ~lower_bounds:false ~revdeps:false label variant
    in
    let master_distro = Distro.tag_of_distro master_distro in
    List.fold_left (fun acc comp_full ->
      let comp = Ocaml_version.to_string (Ocaml_version.with_just_major_and_minor comp_full) in
      build ~opam_version:`V2_0 ~arch:`X86_64 ~distro:master_distro ~compiler:(comp, None) "opam-2.0" ::
      build ~opam_version:`V2_1 ~arch:`X86_64 ~distro:master_distro ~compiler:(comp, None) "opam-2.1" ::
      List.filter_map (fun v ->
        match Ocaml_version.extra v with
        | None -> None
        | Some label ->
            (* TODO: This should be in ocaml-version or ocaml-dockerfile *)
            (* TODO: The same code is used in docker-base-images *)
            let label = String.map (function '+' -> '-' | c -> c) label in
            Some (build ~opam_version ~arch:`X86_64 ~distro:master_distro ~compiler:(comp, Some label) "")
      ) (Ocaml_version.Opam.V2.switches `X86_64 comp_full) @
      List.filter_map (function
        | `X86_64 -> None
        | `Riscv64 -> None (* TODO: unlock this one when more machines are available *)
        | arch ->
            let label = Ocaml_version.to_opam_arch arch in
            Some (build ~opam_version ~arch ~distro:master_distro ~compiler:(comp, None) label)
      ) Ocaml_version.arches @
      acc
    ) [] default_compilers_full

  let build_with_cluster ~ocluster ~analysis ~pkgs ~master ~source ~opam_version ~lower_bounds ~revdeps label variant =
    let arch = Variant.arch variant in
    let pool = Conf.pool_of_arch variant in (* ocluster-pool eg linux-x86_64 *)
    let platform = {Platform.label; pool; variant} in
    let analysis = with_label label analysis in
    let pkgs =
      (* Add fake dependency from pkgs to analysis so that the package being tested appears
        below the platform, to make the diagram look nicer. Ideally, the pulls of the
        base images should be moved to the top (not be per-package at all). *)
      let+ _ = analysis
      and+ pkgs in
      pkgs
    in
    pkgs |> Node.list_map ~collapse_key:"pkg" (module PackageOpt) (fun pkgopt ->
        let pkg = Current.map (fun {PackageOpt.pkg; urgent = _; has_tests = _} -> pkg) pkgopt in
        let urgent = Current.return None in
        let has_tests = Current.map (fun {PackageOpt.pkg = _; urgent = _; has_tests} -> has_tests) pkgopt in
        let base =
          match Variant.os variant with
          | `macOS ->
              Current.return (Spec.MacOS (Variant.docker_tag variant))
          | `FreeBSD ->
              Current.return (Spec.FreeBSD (Variant.docker_tag variant))
          | `linux -> (* TODO: Use docker images as base for both macOS and linux *)
              let+ repo_id =
                Docker.peek ~schedule:weekly ~arch:(Ocaml_version.to_docker_arch arch)
                  ("ocaml/opam:" ^ Variant.docker_tag variant)
              in
              Spec.Docker (Current_docker.Raw.Image.of_hash repo_id)
        in
        let image =
          let spec = build_spec ~platform ~opam_version pkg in
          Cluster_build.v ocluster ~label:"build" ~base ~spec ~master ~urgent source
        in
        let build = Node.action `Built image
        and tests =
          Node.bool_map (fun () ->
            let action =
              let spec = test_spec ~platform ~opam_version pkg in
              Cluster_build.v ocluster ~label:"test" ~base ~spec ~master ~urgent source
            in
            let action = Node.action `Built action in
            Node.leaf ~label:"tests" action
          ) has_tests
        and lower_bounds_check =
          if lower_bounds then
            let action =
              let spec = lower_bounds_spec ~platform ~opam_version pkg in
              Cluster_build.v ocluster ~label:"lower-bounds" ~base ~spec ~master ~urgent source
            in
            let action = Node.action `Built action in
            Node.leaf ~label:"lower-bounds" action
          else
            Node.empty
        and revdeps =
          if revdeps then test_revdeps ~ocluster ~opam_version ~master ~base ~platform ~pkgopt source ~after:image
          else Node.empty
        in
        let label = Current.map OpamPackage.to_string pkg in
        ignore revdeps;
        ignore test_revdeps;
        Node.actioned_branch_dyn ~label build [
          tests;
          lower_bounds_check;
          revdeps;
        ]
      )
    |> (fun x -> Node.branch ~label [x])
    |> Node.collapse ~key:"platform" ~value:label ~input:analysis

  let build_with_docker ~analysis ~pkgs ~master ~source ~opam_version ~lower_bounds ~revdeps label variant =
    ignore revdeps;
    let arch = Variant.arch variant in
    let pool = Conf.pool_of_arch variant in (* ocluster-pool eg linux-x86_64 *)
    let platform = {Platform.label; pool; variant} in
    let analysis = with_label label analysis in
    let pkgs =
      (* Add fake dependency from pkgs to analysis so that the package being tested appears
        below the platform, to make the diagram look nicer. Ideally, the pulls of the
        base images should be moved to the top (not be per-package at all). *)
      let+ _ = analysis
      and+ pkgs in
      pkgs
    in
    pkgs |> Node.list_map ~collapse_key:"pkg" (module PackageOpt) (fun pkgopt ->
        let pkg = Current.map (fun {PackageOpt.pkg; urgent = _; has_tests = _} -> pkg) pkgopt in
        let urgent = Current.return None in
        let has_tests = Current.map (fun {PackageOpt.pkg = _; urgent = _; has_tests} -> has_tests) pkgopt in
        let base =
          match Variant.os variant with
          | `macOS ->
              Current.return (Spec.MacOS (Variant.docker_tag variant))
          | `FreeBSD ->
              Current.return (Spec.FreeBSD (Variant.docker_tag variant))
          | `linux -> (* TODO: Use docker images as base for both macOS and linux *)
              let+ repo_id =
                Docker.peek ~schedule:weekly ~arch:(Ocaml_version.to_docker_arch arch)
                  ("ocaml/opam:" ^ Variant.docker_tag variant)
              in
              Spec.Docker (Current_docker.Raw.Image.of_hash repo_id)
        in
        let image =
          let spec = build_spec ~platform ~opam_version pkg in
          Local_build.v ~label:"build" ~base ~spec ~master ~urgent source
        in
        let build = Node.action `Built image
        and tests =
          Node.bool_map (fun () ->
            let action =
              let spec = test_spec ~platform ~opam_version pkg in
              Local_build.v ~label:"test" ~base ~spec ~master ~urgent source
            in
            let action = Node.action `Built action in
            Node.leaf ~label:"tests" action
          ) has_tests
        and lower_bounds_check =
          if lower_bounds then
            let action =
              let spec = lower_bounds_spec ~platform ~opam_version pkg in
              Local_build.v ~label:"lower-bounds" ~base ~spec ~master ~urgent source
            in
            let action = Node.action `Built action in
            Node.leaf ~label:"lower-bounds" action
          else
            Node.empty
        (* and revdeps =
          if revdeps then test_revdeps ~ocluster ~opam_version ~master ~base ~platform ~pkgopt source ~after:image
          else Node.empty *)
        in
        let label = Current.map OpamPackage.to_string pkg in
        Node.actioned_branch_dyn ~label build [
          tests;
          lower_bounds_check;
          (* revdeps; *)
        ]
      )
    |> (fun x -> Node.branch ~label [x])
    |> Node.collapse ~key:"platform" ~value:label ~input:analysis

  (** Build jobs on a cluster *)
  let cluster ~ocluster ~analysis ~lint ~master source =
    ignore (linux_distributions, macos, freebsd, extras);
    let pkgs = Current.map (fun x -> Analyse.Analysis.packages x
      |> List.filter_map get_significant_available_pkg) analysis in
    let build = build_with_cluster ~ocluster ~analysis ~pkgs ~master ~source in
    [
      Node.leaf ~label:"(lint)" (Node.action `Linted lint);
      Node.branch ~label:"compilers" (compilers ~arch:`X86_64 ~build);
      Node.branch ~label:"distributions" (linux_distributions ~arch:`X86_64 ~build);
      Node.branch ~label:"macos" (macos ~build);
      Node.branch ~label:"freebsd" (freebsd ~build);
      Node.branch ~label:"extras" (extras ~build);
    ]

  (** Build jobs locally with Docker *)
  let docker ~analysis ~lint ~master source =
    let pkgs = Current.map (fun x -> Analyse.Analysis.packages x
      |> List.filter_map get_significant_available_pkg) analysis in
    let build = build_with_docker ~analysis ~pkgs ~master ~source in
    [
      Node.leaf ~label:"(lint)" (Node.action `Linted lint);
      Node.branch ~label:"compilers" (compilers ~arch:Conf.host_arch ~build);
      Node.branch ~label:"distributions" (linux_distributions ~arch:Conf.host_arch ~build);
    ]
end

module Summary = struct
  type t = { ok: int; pending: int; err: int; skip: int; lint: int }

  let merge a b =
    {
      ok = a.ok + b.ok;
      pending = a.pending + b.pending;
      err = a.err + b.err;
      skip = a.skip + b.skip;
      lint = a.lint + b.lint;
    }

  let empty = { ok = 0; pending = 0; err = 0; skip = 0; lint = 0 }
  let ok = { empty with ok = 1 }
  let pending = { empty with pending = 1 }
  let err = { empty with err = 1 }
  let skip = { empty with skip = 1 }
  let lint = { empty with lint = 1 }

  let of_current t =
    let+ result = Current.state ~hidden:true t in
    match result with
    | Ok `Analysed -> empty
    | Ok `Linted -> lint
    | Ok `Built -> ok
    | Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> skip
    | Error `Msg _ -> err
    | Error `Active _ -> pending

  let to_string { ok; pending; err; skip; lint } =
    let lint = lint > 0 in
    let main_jobs =
      if pending > 0 then Error (`Active `Running)
      else match ok, err, skip with
        | 0, 0, 0 -> Ok "No build was necessary"
        | 0, 0, _skip -> Error (`Msg "Everything was skipped")
        | ok, 0, 0 -> Ok (Fmt.str "%d jobs passed" ok)
        | ok, 0, skip -> Ok (Fmt.str "%d jobs passed, %d jobs skipped" ok skip)
        | ok, err, skip -> Error (`Msg (Fmt.str "%d jobs failed, %d jobs skipped, %d jobs passed" err skip ok))
    in
    (lint, main_jobs)

  let n_jobs { ok; pending; err; skip; lint } = ok + pending + err + skip + lint
end

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
    let+ refs = refs in
    Github.Api.Ref_map.fold begin fun key head acc ->
      match key with
      | `Ref _ -> acc (* Skip branches, only check PRs *)
      | `PR _ -> head :: acc
    end (Github.Api.all_refs refs) []
  in
  master, prs

let analyze ~master src =
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
    let n_jobs = Summary.n_jobs summary in
    let summary = Summary.to_string summary in
    let status =
      match summary with
      | _, Ok _ -> `Passed
      | _, Error (`Active `Running) -> `Pending
      | _, Error (`Msg _) -> `Failed
    in
    Index.set_status ~owner ~name ~hash (status, n_jobs);
    summary
  in
  summary

let test_pr ~ocluster ~master ~head =
  let repo = Current.map Current_github.Api.Commit.repo_id head in
  let commit_id = Current.map Github.Api.Commit.id head in
  let hash = Current.map Git.Commit_id.hash commit_id in
  let src = Git.fetch commit_id in
  let latest_analysis = analyze ~master src in
  let analysis = latest_analysis |> latch ~label:"analysis" (* ignore errors from a rerun *) in
  let lint =
    let packages =
      Current.map (fun x ->
        List.map (fun (pkg, {Analyse.Analysis.kind; has_tests = _}) ->
          (pkg, kind))
          (Analyse.Analysis.packages x))
        analysis
    in
    Lint.check ~host_os:Conf.host_os ~master ~packages src
  in
  let builds =
    Node.root
      (Node.leaf ~label:"(analysis)" (Node.action `Analysed latest_analysis)
      :: Build_with.cluster ~ocluster ~analysis ~lint ~master commit_id)
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
    test_pr ~ocluster ~master ~head
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

let local_test_pr repo pr_branch () =
  let master = Git.Local.commit_of_ref repo "refs/heads/master" in
  let pr_gref = Printf.sprintf "refs/heads/%s" pr_branch in
  let pr_branch = Git.Local.commit_of_ref repo pr_gref in
  let analysis = analyze ~master pr_branch in
  let lint =
    let packages =
      Current.map (fun x ->
        List.map (fun (pkg, {Analyse.Analysis.kind; has_tests = _}) ->
          (pkg, kind))
          (Analyse.Analysis.packages x))
        analysis
    in
    Lint.check ~host_os:Conf.host_os ~master ~packages pr_branch
  in
  let builds =
    Node.root
      (Node.leaf ~label:"(analysis)" (Node.action `Analysed analysis)
      :: Build_with.docker ~analysis ~lint ~master pr_branch)
  in
  let dummy_repo =
    Current.return { Github.Repo_id.owner = "local-owner"; name = "local-repo" }
  in
  let pr_hash = Current.map Git.Commit.hash pr_branch in
  (let+ _ = set_index_local ~repo:dummy_repo pr_gref pr_hash
  and+ result = summarise ~repo:dummy_repo ~hash:pr_hash builds in
  result)
  |> Current.ignore_value
