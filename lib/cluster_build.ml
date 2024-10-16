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

  let parse_output ty job build_job =
    let buffer =
      match ty with
      | `Opam (`List_revdeps _, _) -> Some (Buffer.create 1024)
      | _ -> None
    in
    Capability.with_ref build_job (run_job ?buffer ~job) >>!= fun (_ : string) ->
    match buffer with
    | None -> Lwt_result.return ""
    | Some buffer ->
      match Astring.String.cuts ~sep:"\n@@@OUTPUT\n" (Buffer.contents buffer) with
      | [_; output; _] -> Lwt_result.return output
      | [_; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest -> Lwt_result.return ""
      | _ -> Lwt_result.fail (`Msg "Missing output from command")

  let build { config; master; urgent; base } job
      { Key.pool; commit; variant; ty } =
    let { connection; timeout } = config in
    let master = Current_git.Commit.hash master in
    let timeout = match Variant.arch variant with
      | `Riscv64 -> Int64.mul timeout 2L
      | _ -> timeout in
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
    parse_output ty job

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
  { connection; timeout }

let v t ~label ~spec ~base ~master ~urgent commit =
  Current.component "%s" label |>
  let> { Spec.variant; ty } = spec
  and> base
  and> commit
  and> master
  and> urgent in
  let pool = pool_of_variant variant in
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
