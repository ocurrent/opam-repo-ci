(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

module Opam_helpers = Opam_helpers


let ( // ) = Filename.concat
let get_files dir = dir |> Sys.readdir |> Array.to_list
let some_if v cond = if cond then Some v else None

(* List.is_empty is not available until ocaml 5.1 *)
let list_is_empty = function
  | [] -> true
  | _ :: _ -> false

include Lint_error

module Checks = struct
  (* A lint check takes an opam package (given by its parsed name and package data) to a
     list of linting errors. An empty list of errors signals a passing check. *)
  type lint_check = pkg:OpamPackage.t -> OpamFile.OPAM.t -> (OpamPackage.t * error) list

  type kind =
    | General_opam_file
    | Opam_repo_publication
    | Opam_repo_archive

  (* The kinds of checks that need to take account of whether a package is newly published *)
  let needs_newness : kind list -> bool
    = List.mem Opam_repo_publication

  (* The kinds of checks that want to inspect package source (tho it may not be available) *)
  let wants_source : kind list -> bool
    = List.mem Opam_repo_publication

  (* [is_conf_package opam] is true iff [opam] has ["conf"] in its [flags].

     See https://opam.ocaml.org/doc/Manual.html#opamflag-conf *)
  let is_conf_package opam =
    OpamFile.OPAM.flags opam
    |> List.mem OpamTypes.Pkgflag_Conf

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
        let expected = OpamPackage.name pkg in
        if OpamPackage.Name.equal name expected then
          [ (pkg, UnnecessaryField "name") ]
        else [ (pkg, UnmatchedName (name, expected)) ]

  let check_version_field ~pkg opam =
    match OpamFile.OPAM.version_opt opam with
    | None -> []
    | Some version ->
        let expected = OpamPackage.version pkg in
        if OpamPackage.Version.equal version expected then
          [ (pkg, UnnecessaryField "version") ]
        else [ (pkg, UnmatchedVersion (version, expected)) ]

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
      Stdlib.Option.value ~default:[]
        (Option.map (check_one_url ~ctx:"url") (OpamFile.OPAM.url opam))
    and extra_file_errs =
      Stdlib.Option.value ~default:[]
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

  let is_dune name =
    OpamPackage.Name.equal name (OpamPackage.Name.of_string "dune")

  let is_dune_build opam =
    let rec is_build_constraint = function
      | OpamFormula.Atom (OpamTypes.Filter (FIdent (_, var, _)))
        when String.equal (OpamVariable.to_string var) "build" -> true
      | Empty | Atom (Filter _) | Atom (Constraint _) -> false
      | Block x -> is_build_constraint x
      | And (x, y) | Or (x, y) -> is_build_constraint x || is_build_constraint y
    in
    let rec aux = function
      | OpamFormula.Atom (pkg, constr) ->
          if is_dune pkg then is_build_constraint constr else false
      | Empty -> false
      | Block x -> aux x
      | And (x, y) | Or (x, y) -> aux x || aux y
    in
    aux opam.OpamFile.OPAM.depends

  let check_dune_build_dep ~pkg opam =
    let is_build = is_dune_build opam in
    if is_build then [(pkg, DuneIsBuild)] else []

  let check_maintainer_contact ~pkg opam =
    let is_present bug_reports = bug_reports <> [] in
    let includes_an_email maintainers =
      List.exists
        (fun m -> Str.string_match (Str.regexp ".*<?.*@.*>?") m 0)
        maintainers
    in
    let bug_reports = OpamFile.OPAM.bug_reports opam in
    let maintainers = OpamFile.OPAM.maintainer opam in
    if is_present bug_reports || includes_an_email maintainers then []
    else [ (pkg, MaintainerWithoutContact maintainers) ]

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

  let check_package_dir ~opam_repo_dir ~pkg _opam =
    let dir = Opam_helpers.path_from_pkg ~opam_repo_dir pkg in
    let relative_dir = Opam_helpers.path_from_pkg ~opam_repo_dir:"" pkg in
    let check_file = function
      | "opam" ->
          let path = dir // "opam" in
          let relative_path = relative_dir // "opam" in
          if is_perm_correct path then []
          else [ (pkg, ForbiddenPerm relative_path) ]
      | file ->
          let relative_path = relative_dir // file in
          [ (pkg, UnexpectedFile relative_path) ]
    in
    (* FIXME: Would it be better to make skipping this check more explicit? *)
    if Sys.file_exists dir then
      get_files dir |> List.map check_file |> List.concat
    else (
      print_endline
      @@ Printf.sprintf
           "Skipped check_package_dir since package dir %s doesn't exist" dir;
      [])

  (* Check that package names have the "conf-" perfix iff they have the "conf"
     flag iff they have a non-empty "depext". As per
     https://opam.ocaml.org/doc/Manual.html#opamflag-conf, which stipulates that

     > [conf] packages should have a name starting with conf-, and include the
       appropriate depexts: field. *)
  let check_conf_flag
    : lint_check
    = fun ~pkg opam ->
      let is_conf_package = is_conf_package opam in
      let pkg_has_conf_prefix =
        OpamPackage.Name.to_string pkg.name |> String.starts_with ~prefix:"conf-"
      in
      let has_depext = not @@ list_is_empty @@ OpamFile.OPAM.depexts opam
      in
      let properties =
        [ some_if `Conf_flag is_conf_package
        ; some_if `Conf_prefix pkg_has_conf_prefix
        ; some_if `Depext has_depext ]
        |> List.filter_map Fun.id
      in
      if list_is_empty properties || List.length properties = 3  then
        (* Either all are true or all false, and we are all good *)
        []
      else
        [(pkg, InvalidConfPackage properties)]


  let check_package_source ~pkg_src_dir
    : lint_check =
    fun ~pkg opam ->
    if Option.is_some pkg_src_dir ||
       (* conf packages generally don't have sources.
          See https://opam.ocaml.org/doc/Manual.html#opamflag-conf *)
       is_conf_package opam
    then
      []
    else
      [ (pkg, NoPackageSources) ]

  (** [package_name_collision p0 p1] returns true if [p0] is similar to [p1].
    Similarity is defined to be:

    - Case-insensitive string equality considering underscores ([_])
      dashes ([-]), and the empty string to be equal *)
  let package_name_collision p0 p1 =
    let dash_underscore p0 p1 =
      let f = function
        | '_' | '-' -> None
        | c -> Some (Char.lowercase_ascii c)
      in
      let p0 = p0 |> String.to_seq |> Seq.filter_map f in
      let p1 = p1 |> String.to_seq |> Seq.filter_map f in
      Seq.equal Char.equal p0 p1
    in
    dash_underscore p0 p1

  let check_name_collisions ~pkg package_names _opam =
    let pkg_name = pkg.OpamPackage.name |> OpamPackage.Name.to_string in
    let pkg_name_lower = String.lowercase_ascii pkg_name in
    let other_pkgs =
      List.filter (fun s -> not @@ String.equal s pkg_name) package_names
    in
    List.filter_map
      (fun other_pkg ->
        let other_pkg_lower = String.lowercase_ascii other_pkg in
        if package_name_collision pkg_name_lower other_pkg_lower then
          Some (pkg, NameCollision other_pkg)
        else None)
      other_pkgs

  let check_deps_have_upper_bounds ~pkg opam =
    (* See https://github.com/ocaml/opam-repository/blob/master/governance/policies/archiving.md#archiving-a-package *)
    let is_upper_bound_constraint
      : OpamTypes.filter OpamTypes.filter_or_constraint -> bool
      = function
        | Constraint ((`Eq | `Leq | `Lt), _) -> true
        | _ -> false
    in
    OpamFile.OPAM.depends opam
    |> OpamFormula.fold_left (fun acc ((name, condition) : OpamTypes.name * OpamTypes.condition) ->
        if OpamPackage.Name.to_string name = "ocaml" (* The compiler is special *)
        || OpamFormula.exists is_upper_bound_constraint condition
        then
          acc
        else
          (pkg, MissingUpperBound (OpamPackage.Name.to_string name)) :: acc
      )
      []

  let check_x_reason_for_archival ~pkg opam =
    let is_valid_reason (item : OpamParserTypes.FullPos.value) =
      match item.pelem with
      | String reason -> List.mem reason x_reason_for_archiving_valid_reasons
      | _ -> false (* Must be a string *)
    in
    opam
    |> OpamFile.OPAM.extensions
    |> OpamStd.String.Map.find_opt x_reason_for_archiving_field
    |> function
    | None ->
      [(pkg, InvalidReasonForArchiving)] (* Field must be present *)
    | Some field ->
      match field.pelem with
      (* Must be a non-empty list of valid reasons *)
      | List {pelem = (_::_ as reasons); _} when List.for_all is_valid_reason reasons -> []
      | _ -> [(pkg, InvalidReasonForArchiving)]

  let x_opam_repository_commit_hash_at_time_of_archival ~pkg  opam  =
    opam
    |> OpamFile.OPAM.extensions
    |> OpamStd.String.Map.find_opt x_opam_repository_commit_hash_at_time_of_archiving_field
    |> function
    | Some {pelem = String _; _} -> []
    | _ -> [(pkg, InvalidOpamRepositoryCommitHash)]

  let checks kinds ~newly_published ~opam_repo_dir ~pkg_src_dir repo_package_names =
    let general_opam_file_checks () =
      [
        opam_lint;
      ]
    in
    let opam_repo_publication_checks () =
      [
        check_dune_subst;
        check_name_field;
        check_version_field;
        check_checksums;
        check_package_dir ~opam_repo_dir;
        check_package_source ~pkg_src_dir;
        check_maintainer_contact;
        check_tags;
        check_no_pin_depends;
        check_no_extra_files;
        check_conf_flag;
        Prefix.check_prefix_conflict_class_mismatch;
        check_dune_build_dep;
      ] @
      if newly_published then
        [
          check_name_collisions repo_package_names;
          Prefix.check_name_restricted_prefix;
        ]
      else
        []
    in
    let opam_repo_archive_checks () =
      [
        check_deps_have_upper_bounds;
        check_x_reason_for_archival;
        x_opam_repository_commit_hash_at_time_of_archival;
      ]
    in
    List.concat_map
      (function
        | General_opam_file -> general_opam_file_checks ()
        | Opam_repo_publication -> opam_repo_publication_checks ()
        | Opam_repo_archive -> opam_repo_archive_checks ())
      kinds


  let lint_package
      ~kinds
      ~opam_repo_dir ~pkg ~pkg_src_dir ~repo_package_names
      ~newly_published opam =
    checks kinds ~newly_published ~opam_repo_dir ~pkg_src_dir repo_package_names
    |> List.concat_map (fun f -> f ~pkg opam)
end

type t = {
  pkg : OpamPackage.t;
  newly_published : bool option;
  pkg_src_dir : string option;
  opam : OpamFile.OPAM.t;
}

let v ~pkg ?(newly_published = None) ~pkg_src_dir opam =
  { pkg; newly_published; pkg_src_dir; opam }

(** A package is considered newly published if the repository doesn't have any
    other versions of the package, other than the current one.

    NOTE: If two versions of a package are published in the same PR, this
    inference would fail to detect the package as new.*)
let is_newly_published ~opam_repo_dir pkg =
  let pkg_name = OpamPackage.(pkg |> name |> Name.to_string) in
  let pkg_str = OpamPackage.to_string pkg in
  match get_files (opam_repo_dir // "packages" // pkg_name) with
  | exception Sys_error _ -> true
  | [] -> true
  | [ v ] -> v = pkg_str
  | _ -> false

let get_package_names repo_dir =
  get_files (repo_dir // "packages") |> List.sort String.compare

let lint_packages
    ?(checks=Checks.[General_opam_file; Opam_repo_publication])
    ~opam_repo_dir
    metas
  =
  if Sys.file_exists (opam_repo_dir // "packages") then
    let repo_package_names = get_package_names opam_repo_dir in
    metas
    |> List.map (fun { pkg; newly_published; pkg_src_dir; opam } ->
           let newly_published =
             match newly_published with
             | Some v -> v
             | None ->
               if Checks.needs_newness checks then
                 is_newly_published ~opam_repo_dir pkg
               else
                 false
           in
           Checks.lint_package ~kinds:checks ~opam_repo_dir ~pkg ~pkg_src_dir
             ~repo_package_names ~newly_published opam)
    |> List.concat |> Result.ok
  else Error (Printf.sprintf "Invalid opam repository: %s" opam_repo_dir)
