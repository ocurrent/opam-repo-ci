open Current.Syntax
open Lwt.Infix

module Git = Current_git
module Raw = Current_docker.Raw

(* TODO: Make macOS use docker images *)
type base =
  | Docker of Current_docker.Raw.Image.t
  | MacOS of string
  | FreeBSD of string

let base_to_string = function
  | Docker img -> Current_docker.Raw.Image.hash img
  | MacOS base -> base
  | FreeBSD base -> base

let ( >>!= ) = Lwt_result.bind

module Spec = struct
  type package = OpamPackage.t

  let package_to_yojson x = `String (OpamPackage.to_string x)

  type opam_build = {
    revdep : package option;
    with_tests : bool;
    lower_bounds : bool;
    opam_version : [`V2_0 | `V2_1 | `Dev];
  } [@@deriving to_yojson]

  type list_revdeps = {
    opam_version : [`V2_0 | `V2_1 | `Dev];
  } [@@deriving to_yojson]

  type ty = [
    | `Opam of [ `Build of opam_build | `List_revdeps of list_revdeps ] * package
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

  let pp_opam_version = function
    | `V2_0 -> "2.0"
    | `V2_1 -> "2.1"
    | `Dev -> "dev"

  let pp_ty f = function
    | `Opam (`List_revdeps {opam_version}, pkg) ->
        Fmt.pf f "list revdeps of %s, using opam %s" (OpamPackage.to_string pkg)
          (pp_opam_version opam_version)
    | `Opam (`Build { revdep; lower_bounds; with_tests; opam_version }, pkg) ->
      let action = if with_tests then "test" else "build" in
      Fmt.pf f "%s %a%s, using opam %s" action (pp_pkg ?revdep) pkg
        (if lower_bounds then ", lower-bounds" else "")
        (pp_opam_version opam_version)

  let pp_summary f = function
    | `Opam (`List_revdeps _, _) -> Fmt.string f "Opam list revdeps"
    | `Opam (`Build _, _) -> Fmt.string f "Opam project build"
end

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
    base : base;
  }

  let id = "ci-build"
  let dockerignore = ".git"

  module Key = struct
    type t = {
      commit : Current_git.Commit.t; (* The source code to build and test *)
      branch : string;
      label : string; (* A unique ID for this build within the commit *)
    }

    let to_json { commit; branch; label } =
      `Assoc
        [
          ("commit", `String (Current_git.Commit.hash commit));
          ("branch", `String branch);
          ("label", `String label);
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = {
      ty : Spec.ty;
      variant : Variant.t; (* Added as a comment in the Dockerfile *)
    }

    let to_json { ty; variant } =
      `Assoc
        [
          ("op", Spec.ty_to_yojson ty);
          ("variant", Variant.to_yojson variant);
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.Unit

  let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

  let run { config; master = _; urgent = _; base } job
      { Key.commit; label = _; branch = _ } { Value.ty; variant } =
    let { Builder.docker_context; pool; build_timeout } = config in
    let build_spec =
      let base = base_to_string base in
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

  let pp f ({ Key.commit; label; branch }, _) =
    Fmt.pf f "test %s %a (%s)" branch Current_git.Commit.pp commit label

  let auto_cancel = true
  let latched = true
end

module BC = Current_cache.Generic(Op)

(* let v t ~label ~spec ~base ~master ~urgent commit =
  Current.component "%s" label |>
  let> { Spec.platform; ty } = spec
  and> base = base
  and> commit = commit
  and> master = master
  and> urgent = urgent in
  let t = { Op.config = t; master; urgent; base } in
  let { Platform.pool; variant; label = _ } = platform in
  BC.run t { Op.Key.pool; commit; variant; ty } ()
  |> Current.Primitive.map_result (Result.map ignore) *)

let v ~label ~spec ~master ~urgent ~base ~docker_context ~pool ~build_timeout commit branch =
  Current.component "%s" label |>
  let> {Spec.platform; ty} = spec
  and> base in
  let t = {Op.config = { Builder.docker_context; pool; build_timeout }; master; urgent; base } in
  BC.run t {commit; branch; label} { ty; variant = platform.variant }
  |> Current.Primitive.map_result (Result.map ignore)

(* let build ~platforms ~spec ~repo commit =
  Current.component "build"
  |> let> { Spec.variant; ty; label } = spec
      and> commit
      and> platforms
      and> repo in
      match
        List.find_opt
          (fun p -> Variant.equal p.Platform.variant variant)
          platforms
      with
      | Some { Platform.builder; variant; base; _ } ->
          BC.run builder
            { Op.Key.commit; repo; label }
            { Op.Value.base; ty; variant }
      | None ->
          (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
          let msg =
            Fmt.str "BUG: variant %a is not a supported platform" Variant.pp
              variant
          in
          Current_incr.const (Error (`Msg msg), None) *)

(* let list_revdeps t ~platform ~opam_version ~pkgopt ~base ~master ~after commit =
  Current.component "list revdeps" |>
  let> {PackageOpt.pkg; urgent; has_tests = _} = pkgopt
  and> base = base
  and> commit = commit
  and> master = master
  and> () = after in
  let t = { Op.config = t; master; urgent; base } in
  let { Platform.pool; variant; label = _ } = platform in
  let ty = `Opam (`List_revdeps {Spec.opam_version}, pkg) in
  BC.run t { Op.Key.pool; commit; variant; ty } ()
  |> Current.Primitive.map_result (Result.map (fun output ->
      String.split_on_char '\n' output |>
      List.fold_left (fun acc -> function
          | "" -> acc
          | revdep ->
              let revdep = OpamPackage.of_string revdep in
              if OpamPackage.equal pkg revdep then
                acc (* NOTE: opam list --recursive --depends-on <pkg> also returns <pkg> itself *)
              else
                OpamPackage.Set.add revdep acc
        ) OpamPackage.Set.empty
    )) *)

(* let build_with_docker ~analysis ty =
  Current.with_context analysis @@ fun () ->
   *)