open Lwt.Infix
open Current.Syntax

let pool_size = 4
let pool = Current.Pool.create ~label:"lint" pool_size
let () = Lwt_preemptive.init 0 1 (fun _errlog -> ()) (* NOTE: Lwt_preemptive is used to wrap
                                                        long opam calls, and as of today (opam 2.1.0)
                                                        opam uses Unix.chdir to normalize paths
                                                        which isn't thread-safe. *)

let ( // ) = Filename.concat
let ( >>/= ) x f = x >>= fun x -> f (Result.get_ok x)
let exec ~cwd ~job cmd = Current.Process.exec ~cwd ~cancellable:true ~job ("", cmd)

type error =
  | UnnecessaryField of string
  | UnmatchedName of OpamPackage.Name.t
  | UnmatchedVersion of OpamPackage.Version.t
  | DubiousDuneSubst
  | DuneProjectMissing
  | DuneConstraintMissing
  | DuneIsBuild
  | BadDuneConstraint of string * string
  | UnexpectedFile of string
  | ForbiddenPerm of string
  | OpamLint of (int * [`Warning | `Error] * string)
  | FailedToDownload of string
  | NameCollision of string

type host_os = Macos | Other [@@deriving to_yojson]

module Check = struct
  type t = unit

  let marshal () = Yojson.Safe.to_string `Null
  let unmarshal _ = ()

  let path_from_pkg pkg =
    "packages" //
    (OpamPackage.Name.to_string (OpamPackage.name pkg)) //
    (OpamPackage.to_string pkg)

  let get_opam ~cwd pkg =
    Analyse.Analysis.get_opam ~cwd (path_from_pkg pkg // "opam") >>/= fun opam ->
    Lwt.return (OpamFile.OPAM.read_from_string opam)

  let is_perm_correct file =
    Lwt_unix.stat file >|= function
    | {st_kind = S_REG; st_perm = 0o644; _} -> true
    | {st_kind = S_REG; st_perm = 0o664; _} -> true
    | _ -> false

  let get_files dirname =
    Lwt_unix.opendir dirname >>= fun dir ->
    let rec aux files =
      Lwt.catch begin fun () ->
        Lwt_unix.readdir dir >>= fun file ->
        if Fpath.is_rel_seg file then
          aux files
        else
          aux (file :: files)
      end begin function
      | End_of_file -> Lwt.return files
      | exn -> Lwt.fail exn
      end
    in
    Lwt.finalize
      (fun () -> aux [])
      (fun () -> Lwt_unix.closedir dir)

  let scan_dir ~cwd errors pkg =
    let dir = Fpath.to_string cwd // path_from_pkg pkg in
    get_files dir >>= fun files ->
    let rec aux errors extra_files = function
      | [] -> Lwt.return (errors, extra_files)
      | "opam"::files ->
          is_perm_correct (dir // "opam") >|= begin function
          | true -> errors
          | false -> ((pkg, ForbiddenPerm (dir // "opam")) :: errors)
          end >>= fun errors ->
          aux errors extra_files files
      | "files"::files ->
          get_files (dir // "files") >>= fun extra_files ->
          Lwt_list.fold_left_s (fun errors file ->
            is_perm_correct (dir // "files" // file) >|= function
            | true -> errors
            | false -> ((pkg, ForbiddenPerm ("files" // file)) :: errors)
          ) errors extra_files >>= fun errors ->
          let check_hash file hash = try OpamHash.check_file file hash with _ -> false in
          let extra_files =
            List.map (fun file ->
              (OpamFilename.Base.of_string file, check_hash (dir // "files" // file))
            ) extra_files
          in
          aux errors extra_files files
      | file::files ->
          aux ((pkg, UnexpectedFile file) :: errors) extra_files files
    in
    aux errors [] files

  let get_dune_project_version ~pkg url =
    Lwt_io.with_temp_dir @@ fun dir ->
    Lwt_preemptive.detach begin fun () ->
      OpamProcess.Job.run @@
      OpamRepository.pull_tree (OpamPackage.to_string pkg)
        (OpamFilename.Dir.of_string dir)
        (OpamFile.URL.checksum url)
        [OpamFile.URL.url url]
    end () >>= function
    | OpamTypes.Not_available (_, msg) ->
        Lwt.return (Error msg)
    | Up_to_date _ | Result _ ->
        let dune_project = Filename.concat dir "dune-project" in
        Lwt.catch begin fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Input dune_project (fun ch -> Lwt_io.read ch >|= Sexplib.Sexp.parse) >>= begin function
          | Sexplib.Sexp.(Done (List [Atom "lang"; Atom "dune"; Atom version], _)) -> Lwt.return (Ok (Some version))
          | Done _ -> Lwt.fail_with "(lang dune ...) is not the first construct"
          | Cont _ -> Lwt.fail_with "Failed to parse the dune-project file"
          end
        end begin function
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok None)
        | exn -> Lwt.fail exn
        end

  (** On MacOS [get_dune_project_version] does not work and causes a hang
      https://github.com/ocurrent/opam-repo-ci/issues/260 *)
  let get_dune_project_version_portable ~pkg url =
    Lwt_io.with_temp_dir @@ fun dir ->
    let res = OpamProcess.Job.run @@
      OpamRepository.pull_tree (OpamPackage.to_string pkg)
        (OpamFilename.Dir.of_string dir)
        (OpamFile.URL.checksum url)
        [OpamFile.URL.url url] in
    match res with
    | OpamTypes.Not_available (_, msg) ->
        Lwt.return (Error msg)
    | Up_to_date _ | Result _ ->
        let dune_project = Filename.concat dir "dune-project" in
        Lwt.catch begin fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Input dune_project (fun ch -> Lwt_io.read ch >|= Sexplib.Sexp.parse) >>= begin function
          | Sexplib.Sexp.(Done (List [Atom "lang"; Atom "dune"; Atom version], _)) -> Lwt.return (Ok (Some version))
          | Done _ -> Lwt.fail_with "(lang dune ...) is not the first construct"
          | Cont _ -> Lwt.fail_with "Failed to parse the dune-project file"
          end
        end begin function
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok None)
        | exn -> Lwt.fail exn
        end

  let is_dune name =
    OpamPackage.Name.equal name (OpamPackage.Name.of_string "dune")

  let get_dune_constraint opam =
    let get_max = function
      | None, None -> None
      | Some x, None -> Some x
      | None, Some x -> Some x
      | Some x, Some y when OpamVersionCompare.compare x y >= 0 -> Some x
      | Some _, Some y -> Some y
    in
    let get_min = function
      | None, None | Some _, None | None, Some _ -> None
      | Some x, Some y when OpamVersionCompare.compare x y >= 0 -> Some y
      | Some x, Some _ -> Some x
    in
    let is_build = ref false in
    let rec get_lower_bound = function
      | OpamFormula.Atom (OpamTypes.Constraint ((`Gt | `Geq | `Eq), OpamTypes.FString version)) -> Some version
      | Atom (Filter (FIdent (_, var, _))) when String.equal (OpamVariable.to_string var) "build" -> is_build := true; None (* TODO: remove this hack *)
      | Empty | Atom (Filter _) | Atom (Constraint _) -> None
      | Block x -> get_lower_bound x
      | And (x, y) -> get_max (get_lower_bound x, get_lower_bound y)
      | Or (x, y) -> get_min (get_lower_bound x, get_lower_bound y)
    in
    let rec aux = function
      | OpamFormula.Atom (pkg, constr) ->
          if is_dune pkg then
            let v = get_lower_bound constr in
            Some (Option.value ~default:"1.0" v)
          else
            None
      | Empty -> None
      | Block x -> aux x
      | And (x, y) -> get_max (aux x, aux y)
      | Or (x, y) -> get_min (aux x, aux y)
    in
    (!is_build, aux opam.OpamFile.OPAM.depends)

  let check_dune_constraints ~host_os ~errors ~pkg opam =
    match opam.OpamFile.OPAM.url with
    | Some url ->
        let get_dune_project_version =
          match host_os with
          | Macos -> get_dune_project_version_portable
          | Other -> get_dune_project_version
        in
        get_dune_project_version ~pkg url >|= fun dune_version ->
        let is_build, dune_constraint = get_dune_constraint opam in
        let errors =
          match dune_constraint, dune_version with
          | _, Error msg -> (pkg, FailedToDownload msg) :: errors
          | None, Ok None -> errors
          | Some _, Ok None -> (pkg, DuneProjectMissing) :: errors
          | None, Ok (Some _) ->
              if is_dune (OpamPackage.name pkg) then
                errors
              else
                (pkg, DuneConstraintMissing) :: errors
          | Some dep, Ok (Some ver) ->
              if OpamVersionCompare.compare dep ver >= 0 then
                errors
              else
                (pkg, BadDuneConstraint (dep, ver)) :: errors
        in
        if is_build then (pkg, DuneIsBuild) :: errors else errors
    | None ->
        Lwt.return errors

  (** [package_name_collision p0 p1] returns true if [p0] is similar to [p1].
    Similarity is defined to be case-insensitive string equality
    considering '_' and '-' to be equal. As examples, by this relation:

    - "lru-cache" and "lru_cache" collide
    - "lru-cache" and "LRU-cache" collide
    - "lru-cache" and "cache-lru" do not collide *)
  let package_name_collision p0 p1 =
    let f = function
      | '_' -> '-'
      | c -> c
    in
    let p0 = String.map f @@ String.lowercase_ascii p0 in
    let p1 = String.map f @@ String.lowercase_ascii p1 in
    String.equal p0 p1

  let check_name_collisions ~cwd ~errors ~pkg =
    let pkg_name = pkg.OpamPackage.name |> OpamPackage.Name.to_string in
    let repository_path = Fpath.to_string cwd // "packages" in
    get_files repository_path >|= fun packages ->
    let packages = List.filter (fun s -> not @@ String.equal s pkg_name) packages in
    List.fold_left
      (fun errors other_pkg ->
        if package_name_collision pkg_name other_pkg then
          (pkg, NameCollision other_pkg) :: errors
        else
          errors)
      errors packages

  let check_name_field ~errors ~pkg opam =
    match OpamFile.OPAM.name_opt opam with
    | None -> errors
    | Some name ->
        if OpamPackage.Name.equal name (OpamPackage.name pkg) then
          (pkg, UnnecessaryField "name") :: errors
        else
          (pkg, UnmatchedName name) :: errors

  let check_version_field ~errors ~pkg opam =
    match OpamFile.OPAM.version_opt opam with
    | None -> errors
    | Some version ->
        if OpamPackage.Version.equal version (OpamPackage.version pkg) then
          (pkg, UnnecessaryField "version") :: errors
        else
          (pkg, UnmatchedVersion version) :: errors

  let check_dune_subst ~errors ~pkg opam =
    List.fold_left
      (fun errors -> function
          | OpamTypes.([(CString "dune", None); (CString "subst", None)], filter) ->
              begin match filter with
              | Some (OpamTypes.FIdent ([], var, None)) when
                  String.equal (OpamVariable.to_string var) "dev" -> errors
              | _ -> (pkg, DubiousDuneSubst) :: errors
              end
          | _ -> errors
      )
      errors
      opam.OpamFile.OPAM.build

  let opam_lint ~check_extra_files ~errors ~pkg opam =
    Lwt_preemptive.detach begin fun () ->
      OpamFileTools.lint ~check_extra_files ~check_upstream:true opam |>
      List.fold_left begin fun errors -> function
        | (67, _, _) -> errors (* TODO: Disable error 67 (Checksum specified with a non archive url) while discussing a fix:
                                  - https://github.com/ocaml/opam/pull/4834
                                  - https://github.com/ocaml/opam/pull/4960 *)
        | x -> (pkg, OpamLint x) :: errors
      end errors
    end ()

  let of_dir ~host_os ~master ~job ~packages cwd =
    let master = Current_git.Commit.hash master in
    exec ~cwd ~job [|"git"; "merge"; "-q"; "--"; master|] >>/= fun () ->
    Lwt_list.fold_left_s (fun errors (pkg, kind) ->
      match kind with
      | Analyse.Analysis.Deleted ->
          Lwt.return errors (* TODO *)
      | Analyse.Analysis.(New | Unavailable | SignificantlyChanged | UnsignificantlyChanged) ->
          get_opam ~cwd pkg >>= fun opam ->
          let errors = check_name_field ~errors ~pkg opam in
          let errors = check_version_field ~errors ~pkg opam in
          let errors = check_dune_subst ~errors ~pkg opam in
          check_dune_constraints ~host_os ~errors ~pkg opam >>= fun errors ->
          check_name_collisions ~cwd ~errors ~pkg >>= fun errors ->
          (* Check directory structure correctness *)
          scan_dir ~cwd errors pkg >>= fun (errors, check_extra_files) ->
          opam_lint ~check_extra_files ~errors ~pkg opam >>= fun errors ->
          Lwt.return errors
    ) [] packages
end

module Lint = struct
  type t = {
    master : Current_git.Commit.t;
  }

  module Key = struct
    type t = {
      src : Current_git.Commit.t;
      packages : (OpamPackage.t * Analyse.Analysis.kind) list
    }

    let digest {src; packages} =
      Yojson.Safe.to_string (`Assoc [
        "src", `String (Current_git.Commit.hash src);
        "packages", `List (List.map (fun (pkg, kind) ->
          `Assoc [
            "pkg", `String (OpamPackage.to_string pkg);
            "kind", Analyse.Analysis.kind_to_yojson kind;
          ]) packages);
      ])
  end

  module Value = struct
    type t = {
      host_os : host_os
    } [@@deriving to_yojson]

    let digest t = Yojson.Safe.to_string @@ to_yojson t
  end

  module Outcome = Check

  let id = "opam-ci-lint"

  let msg_of_errors =
    List.map (fun (package, err) ->
      let pkg = OpamPackage.to_string package in
      match err with
      | UnnecessaryField field ->
          Fmt.str "Warning in %s: Unnecessary field '%s'. It is suggested to remove it." pkg field
      | UnmatchedName value ->
          Fmt.str "Error in %s: The field 'name' that doesn't match its context. \
                   Field 'name' has value '%s' but was expected of value '%s'."
            pkg
            (OpamPackage.Name.to_string value)
            (OpamPackage.Name.to_string (OpamPackage.name package))
      | UnmatchedVersion value ->
          Fmt.str "Error in %s: The field 'version' that doesn't match its context. \
                   Field 'version' has value '%s' but was expected of value '%s'."
            pkg
            (OpamPackage.Version.to_string value)
            (OpamPackage.Version.to_string (OpamPackage.version package))
      | DubiousDuneSubst ->
          Fmt.str "Warning in %s: Dubious use of 'dune subst'. \
                   'dune subst' should always only be called with {dev} (i.e. [\"dune\" \"subst\"] {dev}) \
                   If your opam file has been autogenerated by dune, you need to upgrade your dune-project \
                   to at least (lang dune 2.7)."
            pkg
      | DuneProjectMissing ->
          Fmt.str "Warning in %s: The package seems to use dune but the dune-project file is missing." pkg
      | DuneConstraintMissing ->
          Fmt.str "Warning in %s: The package has a dune-project file but no explicit dependency on dune was found." pkg
      | DuneIsBuild ->
          Fmt.str "Warning in %s: The package tagged dune as a build dependency. \
                   Due to a bug in dune (https://github.com/ocaml/dune/issues/2147) this should never be the case. \
                   Please remove the {build} tag from its filter."
            pkg
      | BadDuneConstraint (dep, ver) ->
          Fmt.str "Error in %s: Your dune-project file indicates that this package requires at least dune %s \
                   but your opam file only requires dune >= %s. Please check which requirement is the right one, and fix the other."
            pkg ver dep
      | UnexpectedFile file ->
          Fmt.str "Error in %s: Unexpected file in %s/files/%s" pkg (Check.path_from_pkg package) file
      | ForbiddenPerm file ->
          Fmt.str
            "Error in %s: Forbidden permission for file %s/%s. All files should have permissions 644."
            pkg (Check.path_from_pkg package) file
      | OpamLint warn ->
          let warn = OpamFileTools.warns_to_string [warn] in
          Fmt.str "Error in %s: %s" pkg warn
      | FailedToDownload msg ->
          Fmt.str "Error in %s: Failed to download the archive. Details: %s" pkg msg
      | NameCollision other_pkg ->
          Fmt.str "Warning in %s: Possible name collision with package '%s'" pkg other_pkg
    )

  let run { master } job { Key.src; packages } { Value.host_os } =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun dir ->
    Check.of_dir ~host_os ~master ~job ~packages dir >|= fun errors ->
    let errors = msg_of_errors errors in
    List.iter (Current.Job.log job "%s") errors;
    match errors with
    | [] -> Ok ()
    | [msg] -> Error (`Msg msg)
    | _::_ -> Error (`Msg (Fmt.str "%d errors" (List.length errors)))

  let pp f _ = Fmt.string f "Lint"

  let auto_cancel = true
  let latched = true
end

module Lint_cache = Current_cache.Generic(Lint)

let get_packages_kind =
  Current.map (fun packages ->
    List.map (fun (pkg, {Analyse.Analysis.kind; has_tests = _}) ->
      (pkg, kind))
      packages)

let check ?test_config ~host_os ~master ~packages src =
  Current.component "Lint" |>
  let> src
  and> packages = get_packages_kind packages
  and> master in
  let host_os = if String.equal host_os "macos" then Macos else Other in
  Lint_cache.run { master } { src; packages } { host_os }
  |> Current.Primitive.map_result @@ Integration_test.check_lint ?test_config
