(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

module D = Dir_helpers
module O = Opam_helpers

let ( // ) = Filename.concat
let get_files dir = dir |> Sys.readdir |> Array.to_list

include Lint_error

module Checks = struct
  module Prefix = struct
    (* For context, see https://github.com/ocurrent/opam-repo-ci/pull/316#issuecomment-2160069803 *)
    let prefix_conflict_class_map =
      [
        ("mysys2-", "msys2-env");
        ("arch-", "ocaml-arch");
        ("ocaml-env-mingw", "ocaml-env-mingw");
        ("ocaml-env-msvc", "ocaml-env-msvc");
        ("host-arch-", "ocaml-host-arch");
        ("host-system-", "ocaml-host-system");
        ("system-", "ocaml-system");
      ]

    let conflict_class_prefix_map =
      List.map (fun (a, b) -> (b, a)) prefix_conflict_class_map

    let prefixes = List.map fst prefix_conflict_class_map

    let check_name_restricted_prefix ~pkg _opam =
      let name = OpamPackage.name_to_string pkg in
      List.filter_map
        (fun prefix ->
          if String.starts_with ~prefix name then
            Some (pkg, RestrictedPrefix prefix)
          else None)
        prefixes

    let check_prefix_without_conflict_class ~pkg name conflict_classes =
      let prefix =
        List.find_opt (fun prefix -> String.starts_with ~prefix name) prefixes
      in
      match prefix with
      | None -> []
      | Some prefix -> (
          match List.assoc_opt prefix prefix_conflict_class_map with
          | Some required_conflict_class ->
              if List.mem required_conflict_class conflict_classes then []
              else
                [
                  ( pkg,
                    PrefixConflictClassMismatch
                      (WrongConflictClass { prefix; required_conflict_class })
                  );
                ]
          | None ->
              (* NOTE: We should ideally never reach here. It would be a logic
                 error, if we do. *)
              failwith
              @@ Printf.sprintf
                   "BUG: prefix '%s' not found in conflict class map" prefix)

    let check_conflict_class_without_prefix ~pkg name conflict_classes =
      List.filter_map
        (fun conflict_class ->
          match List.assoc_opt conflict_class conflict_class_prefix_map with
          | Some prefix when not (String.starts_with ~prefix name) ->
              Some
                ( pkg,
                  PrefixConflictClassMismatch
                    (WrongPrefix { conflict_class; required_prefix = prefix })
                )
          | _ -> None)
        conflict_classes

    let check_prefix_conflict_class_mismatch ~pkg opam =
      let conflict_classes =
        OpamFile.OPAM.conflict_class opam |> List.map OpamPackage.Name.to_string
      in
      let name = OpamPackage.name_to_string pkg in
      check_prefix_without_conflict_class ~pkg name conflict_classes
      @ check_conflict_class_without_prefix ~pkg name conflict_classes
  end

  let check_name_field ~pkg opam =
    match OpamFile.OPAM.name_opt opam with
    | None -> []
    | Some name ->
        if OpamPackage.Name.equal name (OpamPackage.name pkg) then
          [ (pkg, UnnecessaryField "name") ]
        else [ (pkg, UnmatchedName name) ]

  let check_version_field ~pkg opam =
    match OpamFile.OPAM.version_opt opam with
    | None -> []
    | Some version ->
        if OpamPackage.Version.equal version (OpamPackage.version pkg) then
          [ (pkg, UnnecessaryField "version") ]
        else [ (pkg, UnmatchedVersion version) ]

  let check_dune_subst ~pkg opam =
    let errors =
      List.filter_map
        (function
          | OpamTypes.(
              [ (CString "dune", None); (CString "subst", None) ], filter) -> (
              match filter with
              | Some (OpamTypes.FIdent ([], var, None))
                when String.equal (OpamVariable.to_string var) "dev" ->
                  None
              | _ -> Some (pkg, DubiousDuneSubst))
          | _ -> None)
        opam.OpamFile.OPAM.build
    in
    errors

  let check_checksums ~pkg opam =
    let err ~ctx ~filename msg =
      let err =
        Printf.sprintf "opam field %s contains %s for %s" ctx msg filename
      in
      (pkg, WeakChecksum err)
    in
    let check_one_url ~ctx url =
      let filename =
        Filename.basename @@ OpamUrl.to_string (OpamFile.URL.url url)
      in
      let checksums = OpamFile.URL.checksum url in
      match checksums with
      | [] -> [ err ~ctx ~filename "no checksum" ]
      | _ ->
          if
            List.for_all
              (fun hash ->
                match OpamHash.kind hash with `MD5 -> true | _ -> false)
              checksums
          then [ err ~ctx ~filename "only MD5 as checksum" ]
          else []
    in
    let check_extra_file (basename, hash) =
      match OpamHash.kind hash with
      | `MD5 ->
          let filename = OpamFilename.Base.to_string basename in
          [ err ~ctx:"extra-files" ~filename "only MD5 as checksum" ]
      | _ -> []
    in
    let extra_src_errs =
      List.concat
        (List.map
           (fun (_, url) -> check_one_url ~ctx:"extra-sources" url)
           (OpamFile.OPAM.extra_sources opam))
    and url_errs =
      Option.default []
        (Option.map (check_one_url ~ctx:"url") (OpamFile.OPAM.url opam))
    and extra_file_errs =
      Option.default []
        (Option.map
           (fun efs -> List.concat (List.map check_extra_file efs))
           (OpamFile.OPAM.extra_files opam))
    in
    url_errs @ extra_file_errs @ extra_src_errs

  let check_no_pin_depends ~pkg opam =
    match OpamFile.OPAM.pin_depends opam with
    | [] -> []
    | _ -> [ (pkg, PinDepends) ]

  let check_no_extra_files ~pkg opam =
    match OpamFile.OPAM.extra_files opam with
    | None | Some [] -> []
    | Some _ -> [ (pkg, ExtraFiles) ]

  let get_dune_project_version ~pkg url =
    D.with_temp_dir "lint-dune-project-version-" @@ fun dir ->
    let res =
      OpamProcess.Job.run
      @@ OpamRepository.pull_tree
           (OpamPackage.to_string pkg)
           (OpamFilename.Dir.of_string dir)
           (OpamFile.URL.checksum url)
           [ OpamFile.URL.url url ]
    in
    match res with
    | OpamTypes.Not_available (_, msg) -> Error msg
    | Up_to_date _ | Result _ -> (
        let dune_project = Filename.concat dir "dune-project" in
        match
          In_channel.input_all
          |> In_channel.with_open_text dune_project
          |> Sexplib.Sexp.parse
        with
        | exception Sys_error _ -> Ok None
        | Sexplib.Sexp.Done (List [ Atom "lang"; Atom "dune"; Atom version ], _)
          ->
            Ok (Some version)
        | Done _ -> Error "(lang dune ...) is not the first construct"
        | Cont _ -> Error "Failed to parse the dune-project file")

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
      | OpamFormula.Atom
          (OpamTypes.Constraint ((`Gt | `Geq | `Eq), OpamTypes.FString version))
        ->
          Some version
      | Atom (Filter (FIdent (_, var, _)))
        when String.equal (OpamVariable.to_string var) "build" ->
          (* TODO: remove this hack-ish side-effect change setting is_build, in
             function to get version lower bound *)
          is_build := true;
          None
      | Empty | Atom (Filter _) | Atom (Constraint _) -> None
      | Block x -> get_lower_bound x
      | And (x, y) -> get_max (get_lower_bound x, get_lower_bound y)
      | Or (x, y) -> get_min (get_lower_bound x, get_lower_bound y)
    in
    let rec aux = function
      | OpamFormula.Atom (pkg, constr) ->
          if is_dune pkg then
            let v = get_lower_bound constr in
            Some (Option.default "" v)
          else None
      | Empty -> None
      | Block x -> aux x
      | And (x, y) -> get_max (aux x, aux y)
      | Or (x, y) -> get_min (aux x, aux y)
    in
    (!is_build, aux opam.OpamFile.OPAM.depends)

  let check_dune_constraints ~pkg opam =
    match opam.OpamFile.OPAM.url with
    | Some url ->
        let dune_version = get_dune_project_version ~pkg url in
        let is_build, dune_constraint = get_dune_constraint opam in
        let errors =
          match (dune_constraint, dune_version) with
          | _, Error msg -> [ (pkg, FailedToDownload msg) ]
          | None, Ok None -> []
          | Some "", _ -> [ (pkg, DuneLowerBoundMissing) ]
          | Some _, Ok None -> [ (pkg, DuneProjectMissing) ]
          | None, Ok (Some _) ->
              if is_dune (OpamPackage.name pkg) then []
              else [ (pkg, DuneDependencyMissing) ]
          | Some dep, Ok (Some ver) ->
              if OpamVersionCompare.compare dep ver >= 0 then []
              else [ (pkg, BadDuneConstraint (dep, ver)) ]
        in
        if is_build then (pkg, DuneIsBuild) :: errors else errors
    | None -> []

  let check_maintainer_email ~pkg opam =
    let maintainers = OpamFile.OPAM.maintainer opam in
    List.filter_map
      (fun m ->
        if Str.string_match (Str.regexp ".*<?.*@.*>?") m 0 then None
        else Some (pkg, MaintainerEmailMissing m))
      maintainers

  let check_tags ~pkg opam =
    (* Check if any of the default tags are present *)
    let tags = OpamFile.OPAM.tags opam in
    let default_tags =
      [ "add topics"; "topics"; "to describe"; "your"; "project" ]
    in
    let default_tags_present =
      List.filter_map
        (fun tag -> if List.mem tag tags then Some tag else None)
        default_tags
    in
    match default_tags_present with
    | [] -> []
    | _ -> [ (pkg, DefaultTagsPresent default_tags_present) ]

  let opam_lint ~pkg opam =
    OpamFileTools.lint ~check_upstream:true opam
    |> List.map (fun x -> (pkg, OpamLint x))

  let is_perm_correct file =
    Unix.stat file |> function
    | { st_kind = S_REG; st_perm = 0o644; _ } -> true
    | { st_kind = S_REG; st_perm = 0o664; _ } -> true
    | _ -> false

  let check_package_dir ~repo_dir ~pkg _opam =
    let dir = O.path_from_pkg ~repo_dir pkg in
    let check_file = function
      | "opam" ->
          let path = dir // "opam" in
          if is_perm_correct path then [] else [ (pkg, ForbiddenPerm path) ]
      | file -> [ (pkg, UnexpectedFile file) ]
    in
    get_files dir |> List.map check_file |> List.concat

  (** [package_name_collision p0 p1] returns true if [p0] is similar to [p1].
    Similarity is defined to be either:

    - Case-insensitive string equality considering underscores ([_])
      and dashes ([-]) to be equal
    - A Levenshtein distance within 1/6 of the length of the string (rounding up),
      with names of three characters or less ignored as a special case

    As examples, by this relation:

    - [lru-cache] and [lru_cache] collide
    - [lru-cache] and [LRU-cache] collide
    - [lru-cache] and [cache-lru] do not collide
    - [ocaml] and [pcaml] collide
    - [ocamlfind] and [ocamlbind] do not collide *)
  let package_name_collision p0 p1 =
    let dash_underscore p0 p1 =
      let f = function '_' -> '-' | c -> c in
      let p0 = String.map f p0 in
      let p1 = String.map f p1 in
      String.equal p0 p1
    in
    let levenstein_distance p0 p1 =
      let l = String.length p0 in
      if l <= 3 then false
      else
        let k = ((l - 1) / 16) + 2 in
        (* Ignore distances of 1, too many false positives:
           https://github.com/ocaml/opam-repository/pull/25678 *)
        match Mula.Strings.Lev.get_distance ~k p0 p1 with
        | None -> false
        | Some n when n <= 1 -> false
        | Some _ -> true
    in
    dash_underscore p0 p1 || levenstein_distance p0 p1

  let check_name_collisions ~pkg packages _opam =
    let pkg_name = pkg.OpamPackage.name |> OpamPackage.Name.to_string in
    let pkg_name_lower = String.lowercase_ascii pkg_name in
    let other_pkgs =
      List.filter (fun s -> not @@ String.equal s pkg_name) packages
    in
    List.filter_map
      (fun other_pkg ->
        let other_pkg_lower = String.lowercase_ascii other_pkg in
        if package_name_collision pkg_name_lower other_pkg_lower then
          Some (pkg, NameCollision other_pkg)
        else None)
      other_pkgs

  let checks ~newly_published ~repo_dir packages =
    let newly_published_checks =
      [ check_name_collisions packages; Prefix.check_name_restricted_prefix ]
    in
    let checks =
      [
        check_name_field;
        check_version_field;
        check_dune_subst;
        check_dune_constraints;
        check_checksums;
        check_package_dir ~repo_dir;
        opam_lint;
        check_maintainer_email;
        check_tags;
        check_no_pin_depends;
        check_no_extra_files;
        Prefix.check_prefix_conflict_class_mismatch;
      ]
    in
    if newly_published then checks @ newly_published_checks else checks

  let run_checks ~repo_dir ~pkg ~packages ?(newly_published = false) opam =
    checks ~newly_published packages ~repo_dir
    |> List.map (fun f -> f ~pkg opam)
    |> List.concat

  let parse_error pkg = (pkg, ParseError)
end

let get_packages repo_dir =
  get_files (repo_dir // "packages") |> List.sort String.compare

let run_package_lint ~newly_published ~repo_dir pkg =
  let pkg = OpamPackage.of_string pkg in
  let opam_path = O.path_from_pkg ~repo_dir pkg // "opam" in
  (* NOTE: We use OpamFile.OPAM.read_from_channel instead of OpamFile.OPAM.file
     to prevent the name and version fields being automatically added *)
  In_channel.with_open_text opam_path (fun ic ->
      let opam =
        try Ok (OpamFile.OPAM.read_from_channel ic)
        with OpamPp.Bad_format e | OpamPp.Bad_version (e, _) -> Error e
      in
      match opam with
      | Ok opam ->
          let packages = get_packages repo_dir in
          Checks.run_checks ~repo_dir ~pkg ~packages ~newly_published opam
      | Error _ -> [ Checks.parse_error pkg ])

let check ~new_pkgs ~changed_pkgs repo_dir =
  let changed_errors =
    List.map (run_package_lint ~newly_published:false ~repo_dir) changed_pkgs
    |> List.concat
  in
  let new_pkg_errors =
    List.map (run_package_lint ~newly_published:true ~repo_dir) new_pkgs
    |> List.concat
  in
  new_pkg_errors @ changed_errors
