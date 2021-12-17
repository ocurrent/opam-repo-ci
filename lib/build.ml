open Current.Syntax
open Capnp_rpc_lwt
open Lwt.Infix

module Git = Current_git
module Image = Current_docker.Raw.Image

let ( >>!= ) = Lwt_result.bind

module Spec = struct
  type package = OpamPackage.t

  let package_to_yojson x = `String (OpamPackage.to_string x)

  type opam_build = {
    revdep : package option;
    with_tests : bool;
    lower_bounds : bool;
    opam_version : [`V2_0 | `V2_1];
  } [@@deriving to_yojson]

  type ty = [
    | `Opam of [ `Build of opam_build | `List_revdeps ] * package
  ] [@@deriving to_yojson]

  type t = {
    platform : Platform.t;
    ty : ty;
  }

  let opam ?revdep ~platform ~lower_bounds ~with_tests ~opam_version pkg =
    let ty = `Opam (`Build { revdep; lower_bounds; with_tests; opam_version }, pkg) in
    { platform; ty }

  let pp_pkg ?revdep f pkg =
    match revdep with
    | Some revdep -> Fmt.pf f "%s with %s" (OpamPackage.to_string revdep) (OpamPackage.to_string pkg)
    | None -> Fmt.string f (OpamPackage.to_string pkg)

  let pp_ty f = function
    | `Opam (`List_revdeps, pkg) ->
        Fmt.pf f "list revdeps of %s" (OpamPackage.to_string pkg)
    | `Opam (`Build { revdep; lower_bounds; with_tests; opam_version }, pkg) ->
      let action = if with_tests then "test" else "build" in
      Fmt.pf f "%s %a%s, using opam %s" action (pp_pkg ?revdep) pkg
        (if lower_bounds then ", lower-bounds" else "")
        (match opam_version with
         | `V2_0 -> "2.0"
         | `V2_1 -> "2.1"
        )
end

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
      Option.iter (fun b -> Buffer.add_string b data) buffer;
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

module Op = struct
  type nonrec t = {
    config : t;
    master : Current_git.Commit.t;
    urgent : ([`High | `Low] -> bool) option;
    base : Image.t;                           (* The image with the OCaml compiler to use. *)
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

  module Value = struct
    type t = unit

    let to_json () =
      `Assoc [
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.String

  let lwt_pool = Lwt_pool.create 1000 (fun () -> Lwt.return_unit)

  let run { config = { connection; timeout }; master; urgent; base } job { Key.pool; commit; variant; ty } () =
    Lwt_pool.use lwt_pool @@ fun () ->
    let master = Current_git.Commit.hash master in
    let build_spec ~for_docker =
      let base = Image.hash base in
      match ty with
      | `Opam (`List_revdeps, pkg) -> Opam_build.revdeps ~for_docker ~base ~variant ~pkg
      | `Opam (`Build { revdep; lower_bounds; with_tests; opam_version }, pkg) -> Opam_build.spec ~for_docker ~opam_version ~base ~variant ~revdep ~lower_bounds ~with_tests ~pkg
    in
    Current.Job.write job
      (Fmt.str "@.\
                To reproduce locally:@.@.\
                %a@.\
                git fetch origin master@.\
                git merge %s@.\
                cat > ../Dockerfile <<'END-OF-DOCKERFILE'@.\
                \o033[34m%s\o033[0m@.\
                END-OF-DOCKERFILE@.\
                docker build -f ../Dockerfile .@.@."
         Current_git.Commit_id.pp_user_clone commit
         master
         (Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false (build_spec ~for_docker:true)));
    let spec_str = Fmt.to_to_string Obuilder_spec.pp (build_spec ~for_docker:false) in
    let action = Cluster_api.Submission.obuilder_build spec_str in
    let src = (Git.Commit_id.repo commit, [master; Git.Commit_id.hash commit]) in
    let cache_hint =
      let pkg =
        match ty with
        | `Opam (`Build { revdep = Some revdep; _ }, pkg) -> Printf.sprintf "%s-%s" (OpamPackage.to_string pkg) (OpamPackage.to_string revdep)
        | `Opam (`List_revdeps, pkg)
        | `Opam (`Build _, pkg) -> OpamPackage.to_string pkg
      in
      Printf.sprintf "%s-%s-%s" (Image.hash base) pkg (Git.Commit_id.hash commit)
    in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.log job "Using OBuilder spec:@.%s@." spec_str;
    let build_pool = Current_ocluster.Connection.pool ?urgent ~job ~pool ~action ~cache_hint ~src connection in
    let is_nnpchecker = Option.equal String.equal variant.Variant.ocaml_variant (Some "nnpchecker") in
    let buffer =
      match ty with
      | `Opam (`List_revdeps, _) -> Some (Buffer.create 1024)
      | _ when is_nnpchecker -> Some (Buffer.create 50_000)
      | _ -> None
    in
    Current.Job.start_with ~pool:build_pool job ~timeout ~level:Current.Level.Average >>= fun build_job ->
    Capability.with_ref build_job (run_job ?buffer ~job) >>!= fun (_ : string) ->
    match buffer with
    | None -> Lwt_result.return ""
    | Some buffer when is_nnpchecker ->
        (* TODO: Remove this hack when we switch to OCaml 4.13. See https://github.com/ocaml/ocaml/pull/10171 *)
        begin match Astring.String.find_sub ~rev:true ~sub:"Out-of-heap pointer at " (Buffer.contents buffer) with
        | None -> Lwt_result.return ""
        | Some _ -> Lwt_result.fail (`Msg "Naked pointers detected")
        end
    | Some buffer ->
      match Astring.String.cuts ~sep:"\n@@@OUTPUT\n" (Buffer.contents buffer) with
      | [_; output; _] -> Lwt_result.return output
      | [_; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest -> Lwt_result.return ""
      | _ -> Lwt_result.fail (`Msg "Missing output from command")

  let pp f ({ Key.pool = _; commit; variant; ty }, _) =
    Fmt.pf f "@[<v>%a@,from %a@,on %a@]"
      Spec.pp_ty ty
      Current_git.Commit_id.pp commit
      Variant.pp variant

  let auto_cancel = true
  let latched = true
end

module BC = Current_cache.Generic(Op)

let config ~timeout sr =
  let connection = Current_ocluster.Connection.create sr in
  { connection; timeout }

let v t ~label ~spec ~base ~master ~urgent commit =
  Current.component "%s" label |>
  let> { Spec.platform; ty } = spec
  and> base = base
  and> commit = commit
  and> master = master
  and> urgent = urgent in
  let t = { Op.config = t; master; urgent; base } in
  let { Platform.pool; variant; label = _ } = platform in
  BC.run t { Op.Key.pool; commit; variant; ty } ()
  |> Current.Primitive.map_result (Result.map ignore) (* TODO: Create a separate type of cache that doesn't parse the output *)

let list_revdeps t ~platform ~pkgopt ~base ~master commit =
  Current.component "list revdeps" |>
  let> {PackageOpt.pkg; urgent} = pkgopt
  and> base = base
  and> commit = commit
  and> master = master in
  let t = { Op.config = t; master; urgent; base } in
  let { Platform.pool; variant; label = _ } = platform in
  let ty = `Opam (`List_revdeps, pkg) in
  BC.run t { Op.Key.pool; commit; variant; ty } ()
  |> Current.Primitive.map_result (Result.map (fun output ->
      String.split_on_char '\n' output |>
      List.filter_map (function
          | "" -> None
          | revdep ->
              let revdep = OpamPackage.of_string revdep in
              if OpamPackage.equal pkg revdep then
                None (* NOTE: opam list --recursive --depends-on <pkg> also returns <pkg> itself *)
              else
                Some revdep
        )
    ))
