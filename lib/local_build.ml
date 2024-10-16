open Current.Syntax
open Lwt.Infix

module Spec = Opam_ci_check.Spec
module Variant = Opam_ci_check.Variant
module Opam_build = Opam_ci_check.Opam_build
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

(** Activate BuildKit experimental syntax. *)
module Buildkit_syntax = struct
  (* From `docker manifest inspect docker/dockerfile:experimental` *)
  let hash_for = function
    | `X86_64 ->
        "sha256:8c69d118cfcd040a222bea7f7d57c6156faa938cb61b47657cd65343babc3664"
    | `I386 ->
        "sha256:8c69d118cfcd040a222bea7f7d57c6156faa938cb61b47657cd65343babc3664"
    | `Aarch64 ->
        "sha256:d9ced99b409ddb781c245c7c11f72566f940424fc3883ac0b5c5165f402e5a09"
    | `Aarch32 ->
        "sha256:5f502d5a34f8cd1780fde9301b69488e9c0cfcecde2d673b6bff19aa4979fdfc"
    | `Ppc64le ->
        "sha256:c0fe20821d527e147784f7e782513880bf31b0060b2a7da7a94582ecde81c85f"
    | `S390x ->
        "sha256:e2b9c21cc1d0067116c572db562f80de9b0c7a654ac41f094651a724408beafc"

  (** [add arch] will activate BuildKit experimental syntax with a hash that will
    work for that architecture. Defaults to x86_64 if no arch is specified. *)
  let add arch =
    let hash =
      hash_for
        (match arch with
        | `X86_64 | `I386 -> `X86_64
        | `Aarch64 | `Aarch32 -> `Aarch64
        | `Ppc64le -> `Ppc64le
        | `S390x -> `S390x
        | `Riscv64 ->
            failwith "No support for riscv64 in docker/dockerfile:experimental.")
    in
    Printf.sprintf "# syntax = docker/dockerfile:experimental@%s\n" hash
end

type t = {
  docker_context : string option;
  pool : unit Current.Pool.t;
  build_timeout : Duration.t;
}

let local_builder =
  let pool = Current.Pool.create ~label:"docker" 1 in
  (* Maximum time for one Docker build. *)
  let build_timeout = Duration.of_hour 1 in
  { docker_context = None; pool; build_timeout }

module Op = struct
  type nonrec t = {
    config : t;
    master : Current_git.Commit.t;
    urgent : ([`High | `Low] -> bool) option;
    base : Spec.base;
  }

  let id = "ci-build"
  let dockerignore = ".git"

  module Key = struct
    type t = {
      commit : Current_git.Commit.t; (* The source code to build and test *)
      ty : Spec.ty;
      variant : Variant.t; (* Added as a comment in the Dockerfile *)
    }

    let to_json { commit; ty; variant } =
      `Assoc
        [
          ("commit", `String (Current_git.Commit.hash commit));
          ("op", Spec.ty_to_yojson ty);
          ("variant", Variant.to_yojson variant);
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = Current.String

  let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

  (* Docker output lines are of the form:

    #NN TTTTT OUTPUT

    Where NN is the number of the command run, TTTTT is a timestamp,
    and OUTPUT is a single line of output *)
  let parse_docker_lines s =
    Astring.String.cuts ~sep:"\n" s
    |> List.filter_map (fun s ->
      let split = Astring.String.cuts ~sep:" " s in
      List.nth_opt split 2)
    |> Astring.String.concat ~sep:"\n"

  let parse_output ty job () =
    let f () =
      let log_path = Lwt.return @@ Current.Job.(log_path @@ id job) in
      let read_log path =
        Fpath.to_string path |>
        Lwt_io.open_file ~flags:[Unix.O_RDONLY] ~mode:Input >>=
        Lwt_io.read >>= fun s ->
        Lwt.return @@ Result.ok s
      in
      Lwt_result.bind log_path read_log
    in
    match ty with
    | `Opam (`List_revdeps _, _) -> begin
        f () >>!= fun s ->
        match Astring.String.cuts ~sep:" @@@OUTPUT\n" s with
        | [_; output; _] -> Lwt_result.return @@ parse_docker_lines output
        | [_; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest -> Lwt_result.return ""
        | _ -> Lwt_result.fail (`Msg "Missing output from command")
      end
    | _ -> Lwt_result.return ""

  let build { config; master = _; urgent = _; base } job
      { Key.commit; ty; variant } =
    let { docker_context; pool; build_timeout } = config in
    let os = match Variant.os variant with
      | `Macos | `Linux | `Freebsd -> `Unix
    in
    let build_config = {Spec.variant; ty}
    in
    let image =
      match base with
      | Macos _s -> failwith "Local MacOS OBuilder worker not supported"
      | Freebsd _s -> failwith "Local FreeBSD OBuilder worker not supported"
      | Docker image -> image
    in
    let make_dockerfile ~for_user =
      (if for_user then "" else Buildkit_syntax.add (Variant.arch variant))
      ^ Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:(not for_user)
          ~os (Opam_build.build_spec ~base ~local:true ~for_docker:true build_config)
    in
    Current.Job.write job
      (Fmt.str "@[<v>Base: %s@,%a@]@." image Spec.pp_summary ty);
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
    (* Don't cache Docker steps as revdeps requires the output of commands,
       and the OCurrent cache deals with this anyway *)
    let cmd =
      Raw.Cmd.docker ~docker_context
      @@ [ "build"; "--no-cache"; "--progress=plain"; "--"; Fpath.to_string dir ]
    in
    let pp_error_command f = Fmt.string f "Docker build" in
    Current.Process.exec ~cancellable:true ~pp_error_command ~job cmd >>!=
    parse_output ty job

  let pp f { Key.commit; ty = _; variant } =
    Fmt.pf f "test %a %a" Current_git.Commit.pp commit Variant.pp variant

  let auto_cancel = true
end

module BC = Current_cache.Make(Op)

let v ~label ~spec ~base ~master ~urgent commit =
  Current.component "%s" label |>
  let> {Spec.variant; ty} = spec
  and> base
  and> commit = Git.fetch commit
  and> master
  and> urgent in
  let t = { Op.config = local_builder; master; urgent; base } in
  BC.get t { commit; ty; variant }
  |> Current.Primitive.map_result (Result.map ignore) (* TODO: Create a separate type of cache that doesn't parse the output *)

let list_revdeps ~variant ~opam_version ~pkgopt ~new_pkgs ~base ~master ~after commit =
  let label = "list revdeps" in
  Current.component "%s" label |>
  let> {Package_opt.pkg; urgent; has_tests = _} = pkgopt
  and> new_pkgs
  and> base
  and> commit = Git.fetch commit
  and> master
  and> () = after in
  let t = { Op.config = local_builder; master; urgent; base } in
  let ty = `Opam (`List_revdeps {Spec.opam_version}, pkg) in
  BC.get t { commit; ty; variant }
  |> Current.Primitive.map_result (Result.map (Common.revdeps ~pkg ~new_pkgs))
