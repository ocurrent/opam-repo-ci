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
    with_tests:bool;
  } [@@deriving to_yojson]

  type opam_list = {
    list_with_tests : bool;
  } [@@deriving to_yojson]

  type ty = [
    | `Opam_fmt of Analyse_ocamlformat.source option
    | `Opam of [ `Build of opam_build | `List_revdeps of opam_list ] * package
  ] [@@deriving to_yojson]

  type t = {
    platform : Platform.t;
    ty : ty;
  }

  let opam ?revdep ~platform ~with_tests pkg =
    let ty = `Opam (`Build { revdep; with_tests }, pkg) in
    { platform; ty }

  let pp_pkg ?revdep f pkg =
    match revdep with
    | Some revdep -> Fmt.pf f "%s with %s" (OpamPackage.to_string revdep) (OpamPackage.to_string pkg)
    | None -> Fmt.string f (OpamPackage.to_string pkg)

  let pp_ty f = function
    | `Opam_fmt _ -> Fmt.pf f "ocamlformat"
    | `Opam (`List_revdeps { list_with_tests }, pkg) ->
        let with_tests = if list_with_tests then "with tests" else "without tests" in
        Fmt.pf f "list revdeps of %s %s" (OpamPackage.to_string pkg) with_tests
    | `Opam (`Build { revdep; with_tests }, pkg) ->
      let action = if with_tests then "test" else "build" in
      Fmt.pf f "%s %a" action (pp_pkg ?revdep) pkg
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
  type nonrec t = t

  let id = "ci-ocluster-build"

  module Key = struct
    type t = {
      pool : string;                            (* The build pool to use (e.g. "linux-arm64") *)
      commit : Current_git.Commit_id.t;         (* The source code to build and test *)
      variant : string;                         (* Added as a comment in the Dockerfile *)
      ty : Spec.ty;
    }

    let to_json { pool; commit; variant; ty } =
      `Assoc [
        "pool", `String pool;
        "commit", `String (Current_git.Commit_id.hash commit);
        "variant", `String variant;
        "ty", Spec.ty_to_yojson ty;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = {
      base : Image.t;                           (* The image with the OCaml compiler to use. *)
      master : Current_git.Commit.t;
      urgent : bool;
    }

    let to_json { base; master; urgent } =
      `Assoc [
        "base", `String (Image.digest base);
        "master", `String (Current_git.Commit.hash master);
        "urgent", `Bool urgent;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.String

  let run { connection; timeout} job { Key.pool; commit; variant; ty } { Value.base; master; urgent } =
    let master = Current_git.Commit.hash master in
    let build_spec =
      let base = Image.hash base in
      match ty with
      | `Opam (`List_revdeps { list_with_tests = with_tests }, pkg) -> Opam_build.revdeps ~with_tests ~base ~variant ~pkg
      | `Opam (`Build { revdep; with_tests }, pkg) -> Opam_build.spec ~base ~variant ~revdep ~with_tests ~pkg
      | `Opam_fmt ocamlformat_source -> Lint.fmt_dockerfile ~base ~ocamlformat_source ~variant
    in
    Current.Job.write job
      (Fmt.strf "@.\
                 To reproduce locally:@.@.\
                 %a@.\
                 git fetch origin master@.\
                 git merge %s@.\
                 cat > Dockerfile <<'END-OF-DOCKERFILE'@.\
                 \o033[34m%a\o033[0m@.\
                 END-OF-DOCKERFILE@.\
                 docker build .@.@."
         Current_git.Commit_id.pp_user_clone commit
         master
         Dockerfile.pp (Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false build_spec));
    let spec_sexp = Obuilder_spec.sexp_of_stage build_spec in
    let action = Cluster_api.Submission.obuilder_build (Sexplib.Sexp.to_string_hum spec_sexp) in
    let src = (Git.Commit_id.repo commit, [master; Git.Commit_id.hash commit]) in
    let cache_hint =
      let pkg =
        match ty with
        | `Opam (`List_revdeps _, pkg)
        | `Opam (`Build _, pkg) -> OpamPackage.to_string pkg
        | `Opam_fmt _ -> "ocamlformat"
      in
      Printf.sprintf "%s-%s" (Image.hash base) pkg
    in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.log job "Using OBuilder spec:@.%a@." Sexplib.Sexp.pp_hum spec_sexp;
    let urgent = function
      | `High -> urgent
      | `Low -> false
    in
    let build_pool = Current_ocluster.Connection.pool ~job ~pool ~action ~cache_hint ~src ~urgent connection in
    let buffer =
      match ty with
      | `Opam (`List_revdeps _, _) -> Some (Buffer.create 1024)
      | _ -> None
    in
    Current.Job.start_with ~pool:build_pool job ~timeout ~level:Current.Level.Average >>= fun build_job ->
    Capability.with_ref build_job (run_job ?buffer ~job) >>!= fun (_ : string) ->
    match buffer with
    | None -> Lwt_result.return ""
    | Some buffer ->
      match Astring.String.cuts ~sep:"\n@@@OUTPUT\n" (Buffer.contents buffer) with
      | [_; output; _] -> Lwt_result.return output
      | [_; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest -> Lwt_result.return ""
      | _ -> Lwt_result.fail (`Msg "Missing output from command")

  let pp f ({ Key.pool = _; commit; variant; ty }, _) =
    Fmt.pf f "@[<v>%a@,from %a@,on %s@]"
      Spec.pp_ty ty
      Current_git.Commit_id.pp commit
      variant

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
  and> master = master in
  let { Platform.pool; variant; label = _ } = platform in
  BC.run t { Op.Key.pool; commit; variant; ty } { Op.Value.base; master; urgent }
  |> Current.Primitive.map_result (Result.map ignore)

let list_revdeps t ~with_tests ~platform ~pkg ~base ~master ~urgent commit =
  Current.component "list revdeps" |>
  let> pkg = pkg
  and> base = base
  and> commit = commit
  and> master = master in
  let { Platform.pool; variant; label = _ } = platform in
  let ty = `Opam (`List_revdeps {Spec.list_with_tests = with_tests}, pkg) in
  BC.run t
    { Op.Key.pool; commit; variant; ty }
    { Op.Value.base; master; urgent }
  |> Current.Primitive.map_result (Result.map (fun output ->
      String.split_on_char '\n' output |>
      List.filter_map (function
          | "" -> None
          | pkg -> Some (OpamPackage.of_string pkg)
        )
    ))
