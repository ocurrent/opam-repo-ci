open Current.Syntax
open Lwt.Infix

module Git = Current_git
module Raw = Current_docker.Raw

let ( >>!= ) = Lwt_result.bind

let checkout_pool = Current.Pool.create ~label:"git-clone" 1

module Commit_lock = struct
  let locks = Hashtbl.create 1000

  let rec with_lock ~job commit variant fn =
    let key = (Current_git.Commit.hash commit, variant) in
    match Hashtbl.find_opt locks key with
    | Some lock ->
        Current.Job.log job "Waiting for a similar build to finish...";
        lock >>= fun () -> with_lock ~job commit variant fn
    | None ->
        let finished, set_finished = Lwt.wait () in
        Hashtbl.add locks key finished;
        Lwt.finalize fn (fun () ->
            Hashtbl.remove locks key;
            Lwt.wakeup set_finished ();
            Lwt.return_unit)
end

module Builder = struct
  type t = {
    docker_context : string option;
    pool : unit Current.Pool.t;
    build_timeout : Duration.t;
  }

  let local =
    let pool = Current.Pool.create ~label:"docker" 4 in
    (* Maximum time for one Docker build. *)
    let build_timeout = Duration.of_hour 1 in
    { docker_context = None; pool; build_timeout }

  let build { docker_context; pool; build_timeout } ~dockerfile source =
    Current_docker.Raw.build (`Git source) ~dockerfile ~docker_context ~pool
      ~timeout:build_timeout ~pull:false

  let pull { docker_context; pool = _; build_timeout = _ } ~arch tag =
    let arch =
      if Ocaml_version.arch_is_32bit arch then
        Some (Ocaml_version.to_docker_arch arch)
      else None
    in
    Current_docker.Raw.pull ?arch tag ~docker_context

  let run { docker_context; pool; build_timeout = _ } ~args img =
    Current_docker.Raw.run img ~docker_context ~pool ~args
end

module Op = struct
  type nonrec t = {
    config : Builder.t;
    master : Current_git.Commit.t;
    urgent : ([`High | `Low] -> bool) option;
    base : Spec.base;
  }

  let id = "ci-build"
  let dockerignore = ".git"

  module Key = struct
    type t = {
      commit : Current_git.Commit.t; (* The source code to build and test *)
      label : string; (* A unique ID for this build within the commit *)
      ty : Spec.ty;
      variant : Variant.t; (* Added as a comment in the Dockerfile *)
    }

    let to_json { commit; label; ty; variant } =
      `Assoc
        [
          ("commit", `String (Current_git.Commit.hash commit));
          ("label", `String label);
          ("op", Spec.ty_to_yojson ty);
          ("variant", Variant.to_yojson variant);
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = Current.Unit

  let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

  let build { config; master = _; urgent = _; base } job
      { Key.commit; label = _; ty; variant } =
    let { Builder.docker_context; pool; build_timeout } = config in
    let build_spec =
      let base = Spec.base_to_string base in
      match ty with
      | `Opam (`List_revdeps { opam_version }, pkg) ->
          Opam_build.revdeps ~for_docker:true ~opam_version ~base ~variant ~pkg
      | `Opam (`Build { revdep; lower_bounds; with_tests; opam_version }, pkg) ->
          Opam_build.spec ~for_docker:true ~opam_version ~base ~variant ~revdep ~lower_bounds ~with_tests ~pkg
    in
    match base with
    | MacOS _s -> failwith "local macos docker not supported"
    | FreeBSD _s -> failwith "local freebsd docker not supported"
    | Docker base ->
        let make_dockerfile ~for_user =
          (if for_user then "" else Buildkit_syntax.add (Variant.arch variant))
          ^ Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:(not for_user)
              ~os:`Unix build_spec
        in
        Current.Job.write job
          (Fmt.str "@[<v>Base: %a@,%a@]@." Raw.Image.pp base Spec.pp_summary ty);
        Current.Job.write job
          (Fmt.str
             "@.To reproduce locally:@.@.cd $(mktemp -d)@.%a@.cat > Dockerfile \
              <<'END-OF-DOCKERFILE'@.\o033[34m%s\o033[0mEND-OF-DOCKERFILE@.docker \
              build .@.END-REPRO-BLOCK@.@."
             Current_git.Commit_id.pp_user_clone
             (Current_git.Commit.id commit)
             (make_dockerfile ~for_user:true));
        let dockerfile = make_dockerfile ~for_user:false in
        Current.Job.start ~timeout:build_timeout ~pool job
          ~level:Current.Level.Average
        >>= fun () ->
        Commit_lock.with_lock ~job commit variant @@ fun () ->
        Current_git.with_checkout ~pool:checkout_pool ~job commit @@ fun dir ->
        Current.Job.write job
          (Fmt.str "Writing BuildKit Dockerfile:@.%s@." dockerfile);
        Bos.OS.File.write Fpath.(dir / "Dockerfile") (dockerfile ^ "\n")
        |> or_raise;
        Bos.OS.File.write Fpath.(dir / ".dockerignore") dockerignore |> or_raise;
        let cmd =
          Raw.Cmd.docker ~docker_context
          @@ [ "build"; "--"; Fpath.to_string dir ]
        in
        let pp_error_command f = Fmt.string f "Docker build" in
        Current.Process.exec ~cancellable:true ~pp_error_command ~job cmd

  let pp f { Key.commit; label; ty = _; variant } =
    Fmt.pf f "test %a %a (%s)" Current_git.Commit.pp commit Variant.pp variant label

  let auto_cancel = true
end

module BC = Current_cache.Make(Op)

let v ~label ~spec ~master ~urgent ~base commit =
  Current.component "%s" label |>
  let> {Spec.platform; ty} = spec
  and> base
  and> commit
  and> master
  and> urgent in
  let t = { Op.config = Builder.local; master; urgent; base } in
  BC.get t { commit; label; ty; variant = platform.variant }
