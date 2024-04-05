open Current.Syntax
open Capnp_rpc_lwt
open Lwt.Infix

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

module type S = sig
  include Common.S

  val cache_hint : Spec.base -> Git.Commit_id.t -> OpamPackage.t -> spec -> string
end

module Op (S : S) = struct
  type nonrec t = {
    config : t;
    master : Current_git.Commit.t;
    urgent : ([`High | `Low] -> bool) option;
    base : Spec.base;
  }

  let id = "ci-ocluster-build"

  module Key = struct
    type t = {
      pool : string; (* The build pool to use (e.g. "linux-arm64") *)
      commit : Current_git.Commit_id.t; (* The source code to build and test *)
      variant : Variant.t; (* Added as a comment in the Dockerfile and selects personality *)
      spec : S.spec;
      pkg : Opam_package.t
    }

    let to_json { pool; commit; variant; spec; pkg } =
      `Assoc [
        "pool", `String pool;
        "commit", `String (Current_git.Commit_id.hash commit);
        "op", S.spec_to_yojson spec;
        "pkg", Opam_package.to_yojson pkg;
        "variant", Variant.to_yojson variant;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = S.Value

  let build { config; master; urgent; base } job
      { Key.pool; commit; variant; spec; pkg } =
    let { connection; timeout } = config in
    let master = Current_git.Commit.hash master in
    let os = match Variant.os variant with
      | `Macos | `Linux | `Freebsd -> `Unix
    in
    let build_spec ~for_docker =
      let base = Spec.base_to_string base in
      S.build_spec ~for_docker ~base ~variant spec pkg
    in
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
         (Obuilder_spec.Docker.dockerfile_of_spec ~os ~buildkit:false (build_spec ~for_docker:true)));
    let spec_str = Fmt.to_to_string Obuilder_spec.pp (build_spec ~for_docker:false) in
    let action = Cluster_api.Submission.obuilder_build spec_str in
    let src = (Git.Commit_id.repo commit, [master; Git.Commit_id.hash commit]) in
    let cache_hint = S.cache_hint base commit pkg spec in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.log job "Using OBuilder spec:@.%s@." spec_str;
    let build_pool = Current_ocluster.Connection.pool ?urgent ~job ~pool ~action ~cache_hint ~src connection in
    Current.Job.start_with ~pool:build_pool job ~timeout ~level:Current.Level.Average
    >>= fun build_pool -> S.parse_output job (Some build_pool)

  let pp f { Key.pool = _; commit; variant; spec; pkg } =
    Fmt.pf f "@[<v>%a@,from %a@,on %a@]"
      S.pp_ty (spec, pkg)
      Current_git.Commit_id.pp commit
      Variant.pp variant

  let auto_cancel = true
end

module B = Op (struct
  type spec = Spec.opam_build [@@deriving to_yojson]

  module Value = Current.Unit

  let pp_ty f (spec, pkg) = Spec.pp_ty f (`Opam (`Build spec, pkg))
  let pp_summary f (spec, pkg) = Spec.pp_summary f (`Opam (`Build spec, pkg))

  let build_spec ~for_docker ~base ~variant (spec : spec) pkg =
    let { Spec.revdep; with_tests; lower_bounds; opam_version } = spec in
    Opam_build.spec ~for_docker ~opam_version ~base ~variant ~revdep ~lower_bounds ~with_tests ~pkg

  let parse_output _ _ = Lwt_result.return ()

  let cache_hint base commit pkg spec =
    let pkg =
      match spec.Spec.revdep with
      | Some revdep -> Fmt.str "%s-%s" (OpamPackage.to_string pkg) (OpamPackage.to_string revdep)
      | None -> OpamPackage.to_string pkg
    in
    Fmt.str "%s-%s-%s" (Spec.base_to_string base) pkg (Git.Commit_id.hash commit)
end)

module BC = Current_cache.Make(B)

let config ~timeout sr =
  let connection = Current_ocluster.Connection.create sr in
  { connection; timeout }

let v t ~label ~spec ~base ~master ~urgent commit =
  Current.component "%s" label |>
  let> { Spec.variant; spec; pkg } = spec
  and> base
  and> commit
  and> master
  and> urgent in
  let pool = pool_of_variant variant in
  let t = { B.config = t; master; urgent; base } in
  BC.get t { B.Key.pool; commit; variant; spec; pkg }

module R = Op (struct
  type spec = Spec.list_revdeps [@@deriving to_yojson]

  module Value = Current.String

  let pp_ty f (spec, pkg) = Spec.pp_ty f (`Opam (`List_revdeps spec, pkg))
  let pp_summary f (spec, pkg) = Spec.pp_summary f (`Opam (`List_revdeps spec, pkg))

  let build_spec ~for_docker ~base ~variant (spec : spec) pkg =
    Opam_build.revdeps ~for_docker ~opam_version:spec.opam_version ~base ~variant ~pkg

  let parse_output job = function
  | None -> Lwt_result.return ""
  | Some build_job ->
    let buffer = Buffer.create 1024 in
    Capability.with_ref build_job (run_job ~buffer ~job) >>!= fun (_ : string) ->
    match Astring.String.cuts ~sep:"\n@@@OUTPUT\n" (Buffer.contents buffer) with
    | [_; output; _] -> Lwt_result.return output
    | [_; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest -> Lwt_result.return ""
    | _ -> Lwt_result.fail (`Msg "Missing output from command")

  let cache_hint base commit pkg _ =
    let pkg = OpamPackage.to_string pkg in
    Fmt.str "%s-%s-%s" (Spec.base_to_string base) pkg (Git.Commit_id.hash commit)
end)

module RC = Current_cache.Make(R)

let list_revdeps t ~variant ~opam_version ~pkgopt ~new_pkgs ~base ~master ~after commit =
  Current.component "list revdeps" |>
  let> {Package_opt.pkg; urgent; has_tests = _} = pkgopt
  and> new_pkgs
  and> base
  and> commit
  and> master
  and> () = after in
  let pool = pool_of_variant variant in
  let t = { R.config = t; master; urgent; base } in
  let spec = { Spec.opam_version } in
  RC.get t { R.Key.pool; commit; variant; spec; pkg }
  |> Current.Primitive.map_result (Result.map (Common.revdeps ~pkg ~new_pkgs))
