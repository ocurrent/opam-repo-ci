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

module Op (S : Common.S) = struct
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
      spec : S.spec;
      pkg : OpamPackage.t;
      variant : Variant.t; (* Added as a comment in the Dockerfile *)
    }

    let to_json { commit; spec; pkg; variant } =
      `Assoc
        [
          "commit", `String (Current_git.Commit.hash commit);
          "op", S.spec_to_yojson spec;
          "pkg", Opam_package.to_yojson pkg;
          "variant", Variant.to_yojson variant;
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = S.Value

  let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

  let build { config; master = _; urgent = _; base } job
      { Key.commit; spec; pkg; variant } =
    let { docker_context; pool; build_timeout } = config in
    let os = match Variant.os variant with
      | `Macos | `Linux | `Freebsd -> `Unix
    in
    let build_spec =
      let base = Spec.base_to_string base in
      S.build_spec ~for_docker:true ~base ~variant spec pkg
    in
    let base =
      match base with
      | Macos _s -> failwith "Local MacOS OBuilder worker not supported"
      | Freebsd _s -> failwith "Local FreeBSD OBuilder worker not supported"
      | Docker base -> base
    in
    let make_dockerfile ~for_user =
      (if for_user then "" else Buildkit_syntax.add (Variant.arch variant))
      ^ Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:(not for_user)
          ~os build_spec
    in
    Current.Job.write job
      (Fmt.str "@[<v>Base: %a@,%a@]@." Raw.Image.pp base S.pp_summary (spec, pkg));
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
    Current.Process.exec ~cancellable:true ~pp_error_command ~job cmd
    >>!= fun () -> S.parse_output job None

  let pp f { Key.commit; spec = _; pkg = _; variant } =
    Fmt.pf f "test %a %a" Current_git.Commit.pp commit Variant.pp variant

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
end)

module BC = Current_cache.Make(B)

let v ~label ~spec ~base ~master ~urgent commit =
  Current.component "%s" label |>
  let> { Spec.variant; spec; pkg } = spec
  and> base
  and> commit = Git.fetch commit
  and> master
  and> urgent in
  let t = { B.config = local_builder; master; urgent; base } in
  BC.get t { commit; spec; pkg; variant }
  |> Current.Primitive.map_result (Result.map ignore) (* TODO: Create a separate type of cache that doesn't parse the output *)

module R = Op (struct
  type spec = Spec.list_revdeps [@@deriving to_yojson]

  module Value = Current.String

  let pp_ty f (spec, pkg) = Spec.pp_ty f (`Opam (`List_revdeps spec, pkg))
  let pp_summary f (spec, pkg) = Spec.pp_summary f (`Opam (`List_revdeps spec, pkg))

  let build_spec ~for_docker ~base ~variant (spec : spec) pkg =
    Opam_build.revdeps ~for_docker ~opam_version:spec.opam_version ~base ~variant ~pkg

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

  let parse_output job _ =
    let log_path = Lwt.return @@ Current.Job.(log_path @@ id job) in
    let read_log path =
      Fpath.to_string path |>
      Lwt_io.open_file ~flags:[Unix.O_RDONLY] ~mode:Input >>=
      Lwt_io.read >>= fun s ->
      Lwt.return @@ Result.ok s
    in
    log_path >>!= read_log >>!= fun s ->
    match Astring.String.cuts ~sep:" @@@OUTPUT\n" s with
    | [_; output; _] -> Lwt_result.return @@ parse_docker_lines output
    | [_; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest -> Lwt_result.return ""
    | _ -> Lwt_result.fail (`Msg "Missing output from command")
end)

module RC = Current_cache.Make(R)

let list_revdeps ~variant ~opam_version ~pkgopt ~new_pkgs ~base ~master ~after commit =
  let label = "list revdeps" in
  Current.component "%s" label |>
  let> {Package_opt.pkg; urgent; has_tests = _} = pkgopt
  and> new_pkgs
  and> base
  and> commit = Git.fetch commit
  and> master
  and> () = after in
  let t = { R.config = local_builder; master; urgent; base } in
  let spec = { Spec.opam_version } in
  RC.get t { commit; spec; pkg; variant }
  |> Current.Primitive.map_result (Result.map (Common.revdeps ~pkg ~new_pkgs))
