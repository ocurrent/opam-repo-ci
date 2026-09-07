open Current.Syntax
open Capnp_rpc_lwt
open Lwt.Infix

module Variant = Opam_ci_check.Variant
module Spec = Opam_ci_check.Spec
module Opam_build = Opam_ci_check.Opam_build
module Git = Current_git

let ( >>!= ) = Lwt_result.bind

type t = {
  connection : Current_ocluster.Connection.t;
  timeout : Duration.t;
  enable_day10 : bool;        (* Add riscv64 day10 shadow builds (OPAM_REPO_CI_USE_DAY10). *)
}

let tail ?buffer ~job build_job =
  let rec aux start =
    Cluster_api.Job.log build_job start >>= function
    | Error (`Capnp e) -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
    | Ok ("", _) -> Lwt_result.return ()
    | Ok (data, next) ->
      Stdlib.Option.iter (fun b -> Buffer.add_string b data) buffer;
      Current.Job.write job data;
      aux next
  in aux 0L

let run_job ?buffer ~job build_job =
  let on_cancel _ =
    Cluster_api.Job.cancel build_job >|= function
    | Ok () -> ()
    | Error (`Capnp e) -> Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
  in
  Current.Job.with_handler job ~on_cancel @@ fun () ->
  let result = Cluster_api.Job.result build_job in
  tail ?buffer ~job build_job >>!= fun () ->
  result >>= function
  | Error (`Capnp e) -> Lwt_result.fail (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp e))
  | Ok _ as x -> Lwt.return x

let pool_of_variant v =
  let os = match Variant.os v with
    | `Macos -> "macos"
    | `Freebsd -> "freebsd"
    | `Linux -> "linux"
  in
  let arch = match Variant.arch v with
    | `X86_64 | `I386 -> "x86_64"
    | `Aarch32 | `Aarch64 -> "arm64"
    | `Ppc64le -> "ppc64"
    | `S390x -> "s390x"
    | `Riscv64 -> "riscv64"
  in
  os^"-"^arch

(* ---- day10 routing --------------------------------------------------------

   day10 tests a package by solving and building it (and its dependencies as
   content-addressed layers) straight from the opam-repository, reusing shared
   dependency layers across jobs. For opam-repo-ci we use the [health-check]
   verb: it reads the repository from the worker's mirror, so no source checkout
   is shipped. A PR is represented by overlaying two commits (see
   [day10_action]); the OBuilder path is left untouched for everything else. *)

let day10_enabled () =
  match Sys.getenv_opt "OPAM_REPO_CI_USE_DAY10" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

(* day10 shadow builds run on this pool (the day10-capable workers, e.g.
   carpenter). Hardcoded like the freebsd distro rather than configured. *)
let day10_pool = "test"

(* day10 constrains the compiler with [= version] (main.ml), so it needs an
   exact [ocaml.X.Y.Z]; the variant only carries the major.minor (e.g. "5.3").
   Resolve it to the matching supported release. *)
let day10_ocaml_version variant =
  let mm = Variant.ocaml_version_to_string variant in
  match
    List.find_opt
      (fun r -> String.equal (Ocaml_version.to_string (Ocaml_version.with_just_major_and_minor r)) mm)
      Opam_ci_check.Compiler_version.all_supported
  with
  | Some r -> "ocaml." ^ Ocaml_version.to_string r
  | None -> "ocaml." ^ mm

(* Build a Custom kind="day10" job that health-checks [pkg]. [overlay_base] is
   the master commit for a PR overlay (head wins), or [None] for the head alone
   (a PR that removes a package: its head tree already omits it). *)
let day10_action ~variant ~pkg ~with_test ~overlay_base ~commit =
  let os = match Variant.os variant with `Linux -> "linux" | `Macos -> "macos" | `Freebsd -> "freebsd" in
  let arch = Ocaml_version.to_opam_arch (Variant.arch variant) in
  let os_distribution, os_version =
    let d = Variant.distribution variant in
    match String.rindex_opt d '-' with
    | Some i -> (String.sub d 0 i, String.sub d (i + 1) (String.length d - i - 1))
    | None -> (d, "")
  in
  let payload builder =
    let module B = Cluster_api.Raw.Builder.Day10 in
    let d = B.init_pointer builder in
    B.verb_set d "health-check";
    (* The PR's commits (head + base) live on the tested repository, which may
       be a fork (e.g. a test repo), not ocaml/opam-repository. Point the worker
       at it so both commits are fetchable — mirrors the OBuilder [src] repo. *)
    B.opam_repository_set d (Git.Commit_id.repo commit);
    B.opam_repository_commit_set d (Git.Commit_id.hash commit);
    (match overlay_base with Some h -> B.opam_repository_base_set d h | None -> ());
    B.package_set d (OpamPackage.to_string pkg);
    B.ocaml_version_set d (day10_ocaml_version variant);
    B.with_test_set d with_test;
    B.os_set d os;
    B.arch_set d arch;
    B.os_distribution_set d os_distribution;
    B.os_version_set d os_version
  in
  Cluster_api.Submission.custom_build (Cluster_api.Custom.v ~kind:"day10" payload)

(* day10 health-check always exits 0; the real outcome is the terminal marker
   in the log. Map it to the job result so a failing package fails the PR. *)
let day10_classify log =
  let has affix = Astring.String.is_infix ~affix log in
  if has "[NOTE] success" then Lwt_result.return ""
  else if has "[NOTE] accept_failures" then
    (* The build genuinely failed, but the maintainer declared this platform in
       the package's x-ci-accept-failures field, so day10 tags the failure as
       ignorable. Match OBuilder, whose log-matcher reports [SKIP] Failure
       ignored for the same case (non-gating). Checked after [NOTE] success (so
       a real success stays Ok) and before the failure markers (so an accepted
       failure wins over [ERROR] failure / dependency_failed). *)
    Lwt_result.fail (`Msg "[SKIP] Failure ignored")
  else if has "[WARNING] no_solution" then
    (* No solution on this variant = the package is not available/installable
       here (e.g. an [ocaml >= 5.2] constraint on a 4.14 variant). OBuilder
       treats this as an accepted skip, not a failure; match that by using a
       [SKIP]-prefixed error, which summary.ml counts as [skip] (non-gating)
       rather than [err]. See mtelvers/day10#3. *)
    Lwt_result.fail (`Msg "[SKIP] Package not available (day10: no solution on this variant)")
  else if has "[WARNING] dependency_failed" then
    Lwt_result.fail (`Msg "day10: a dependency failed to build")
  else if has "[ERROR] failure" then
    Lwt_result.fail (`Msg "day10: build failed")
  else
    Lwt_result.fail (`Msg "day10: no result marker in log")

module Op = struct
  type nonrec t = {
    config : t;
    master : Current_git.Commit.t;
    urgent : ([`High | `Low] -> bool) option;
    base : Spec.base;
  }

  let id = "ci-ocluster-build"

  module Key = struct
    type t = {
      pool : string;                            (* The build pool to use (e.g. "linux-arm64") *)
      commit : Current_git.Commit_id.t;         (* The source code to build and test *)
      variant : Variant.t;                      (* Added as a comment in the Dockerfile and selects personality *)
      ty : Spec.ty;
    }

    let to_json { pool; commit; variant; ty } =
      `Assoc [
        "pool", `String pool;
        "commit", `String (Current_git.Commit_id.hash commit);
        "variant", Variant.to_yojson variant;
        "ty", Spec.ty_to_yojson ty;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = Current.String

  let parse_output ~day10 ty job build_job =
    let buffer =
      match day10, ty with
      | true, _ -> Some (Buffer.create 4096)                (* classify by log marker *)
      | false, `Opam (`List_revdeps _, _) -> Some (Buffer.create 1024)
      | false, _ -> None
    in
    Capability.with_ref build_job (run_job ?buffer ~job) >>!= fun (_ : string) ->
    if day10 then
      match buffer with
      | Some b -> day10_classify (Buffer.contents b)
      | None -> Lwt_result.return ""
    else
      match buffer with
      | None -> Lwt_result.return ""
      | Some buffer ->
        match Astring.String.cuts ~sep:"\n@@@OUTPUT\n" (Buffer.contents buffer) with
        | [_; output; _] -> Lwt_result.return output
        | [_; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest -> Lwt_result.return ""
        | _ -> Lwt_result.fail (`Msg "Missing output from command")

  let build { config; master; urgent; base } job
      { Key.pool; commit; variant; ty } =
    let { connection; timeout; _ } = config in
    let master = Current_git.Commit.hash master in
    let timeout = match Variant.arch variant with
      | `Riscv64 -> Int64.mul timeout 2L
      | _ -> timeout in
    (* A job runs on day10 iff it was submitted to the day10 pool (set in [v]
       via ~use_day10) — such jobs only exist when day10 is enabled, so the pool
       alone is the signal. Only plain package builds/tests (incl. revdep
       builds) are eligible; List_revdeps (output-parsed) and lower-bounds
       (day10 has no --lower-bound) fall through to OBuilder even on that pool. *)
    let day10 =
      match ty with
      | `Opam (`Build { lower_bounds = false; revdep; with_tests; _ }, pkg)
        when String.equal pool day10_pool ->
          let target = match revdep with Some r -> r | None -> pkg in
          Some (target, with_tests)
      | _ -> None
    in
    match day10 with
    | Some (target, with_tests) ->
        (* Overlay the base (master) so the PR is tested against current master
           (head wins per package version). Deleted packages generate no build
           jobs, so the overlay is exact for every job we create here; the only
           gap is a PR that removes a package a *modified* package still depends
           on (self-inconsistent) — accepted for now. *)
        let overlay_base = Some master in
        let action = day10_action ~variant ~pkg:target ~with_test:with_tests ~overlay_base ~commit in
        let cache_hint =
          Fmt.str "day10-%s-%s" (OpamPackage.to_string target) (Git.Commit_id.hash commit)
        in
        Current.Job.log job
          "Building with day10 (variant %a, package %s, overlay-base %b)"
          Variant.pp variant (OpamPackage.to_string target) (Option.is_some overlay_base);
        (* No source checkout: day10 reads the repository from the worker mirror.
           [pool] is already the day10 pool (from the Key). *)
        let src = (Git.Commit_id.repo commit, []) in
        let build_pool =
          Current_ocluster.Connection.pool ?urgent ~job ~pool ~action ~cache_hint ~src connection
        in
        Current.Job.start_with ~pool:build_pool job ~timeout ~level:Current.Level.Average >>=
        parse_output ~day10:true ty job
    | None ->
        let os = match Variant.os variant with
          | `Macos | `Linux | `Freebsd -> `Unix
        in
        let build_config = {Spec.variant; ty} in
        Current.Job.write job
          (Fmt.str "@.\
                    To reproduce locally:@.@.\
                    cd $(mktemp -d)@.\
                    %a@.\
                    git fetch origin master@.\
                    git merge --no-edit %s@.\
                    cat > ../Dockerfile <<'END-OF-DOCKERFILE'@.\
                    \o033[34m%s\o033[0m@.\
                    END-OF-DOCKERFILE@.\
                    docker build -f ../Dockerfile .@.@."
             Current_git.Commit_id.pp_user_clone commit
             master
             (Obuilder_spec.Docker.dockerfile_of_spec ~os ~buildkit:false (Opam_build.build_spec ~for_docker:true ~base build_config)));
        let spec_str = Fmt.to_to_string Obuilder_spec.pp (Opam_build.build_spec ~for_docker:false ~base build_config) in
        let action = Cluster_api.Submission.obuilder_build spec_str in
        let src = (Git.Commit_id.repo commit, [master; Git.Commit_id.hash commit]) in
        let cache_hint =
          let pkg =
            match ty with
            | `Opam (`Build { revdep = Some revdep; _ }, pkg) -> Fmt.str "%s-%s" (OpamPackage.to_string pkg) (OpamPackage.to_string revdep)
            | `Opam (`List_revdeps _, pkg)
            | `Opam (`Build _, pkg) -> OpamPackage.to_string pkg
          in
          Fmt.str "%s-%s-%s" (Spec.base_to_string base) pkg (Git.Commit_id.hash commit)
        in
        Current.Job.log job "Using cache hint %S" cache_hint;
        Current.Job.log job "Using OBuilder spec:@.%s@." spec_str;
        let build_pool = Current_ocluster.Connection.pool ?urgent ~job ~pool ~action ~cache_hint ~src connection in
        Current.Job.start_with ~pool:build_pool job ~timeout ~level:Current.Level.Average >>=
        parse_output ~day10:false ty job

  let pp f { Key.pool = _; commit; variant; ty } =
    Fmt.pf f "@[<v>%a@,from %a@,on %a@]"
      Spec.pp_ty ty
      Current_git.Commit_id.pp commit
      Variant.pp variant

  let auto_cancel = true
end

module BC = Current_cache.Make(Op)

let config ~timeout sr =
  let connection = Current_ocluster.Connection.create sr in
  { connection; timeout; enable_day10 = day10_enabled () }

(* Whether to add the riscv64 day10 shadow branch (OPAM_REPO_CI_USE_DAY10). *)
let day10_available t = t.enable_day10

let v t ?(use_day10 = false) ~label ~spec ~base ~master ~urgent commit =
  Current.component "%s" label |>
  let> { Spec.variant; ty } = spec
  and> base
  and> commit
  and> master
  and> urgent in
  (* Routing to day10 is carried by the pool: a day10 job goes to the day10
     pool, which — being part of the cache Key — also keeps its result distinct
     from the sibling OBuilder job at the same variant. *)
  let pool = if use_day10 then day10_pool else pool_of_variant variant in
  let t = { Op.config = t; master; urgent; base } in
  BC.get t { Op.Key.pool; commit; variant; ty }
  |> Current.Primitive.map_result (Result.map ignore) (* TODO: Create a separate type of cache that doesn't parse the output *)

let list_revdeps t ~variant ~opam_version ~pkgopt ~new_pkgs ~base ~master ~after commit =
  Current.component "list revdeps" |>
  let> {Package_opt.pkg; urgent; has_tests = _} = pkgopt
  and> new_pkgs
  and> base
  and> commit
  and> master
  and> () = after in
  let pool = pool_of_variant variant in
  let t = { Op.config = t; master; urgent; base } in
  let ty = `Opam (`List_revdeps {Spec.opam_version}, pkg) in
  BC.get t { Op.Key.pool; commit; variant; ty }
  |> Current.Primitive.map_result (Result.map (Common.revdeps ~pkg ~new_pkgs))
