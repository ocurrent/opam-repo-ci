module D = Dir_helpers

let ( // ) = Filename.concat

let path_from_pkg ~repo_dir pkg =
  repo_dir // "packages"
  // OpamPackage.Name.to_string (OpamPackage.name pkg)
  // OpamPackage.to_string pkg

let get_files dir = dir |> Sys.readdir |> Array.to_list

module Checks = struct
  (** If either a restricted prefix or conflict class exists,
    then the corresponding other must also exist. *)
  type prefix_conflict_class_mismatch =
    | WrongPrefix of { conflict_class : string; required_prefix : string }
    | WrongConflictClass of {
        prefix : string;
        required_conflict_class : string;
      }

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
    | OpamLint of (int * [ `Warning | `Error ] * string)
    | FailedToDownload of string
    | NameCollision of string
    | WeakChecksum of string
    | PinDepends
    | ExtraFiles
    | RestrictedPrefix of string
    | PrefixConflictClassMismatch of prefix_conflict_class_mismatch
    | ParseError

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

    let msg_of_prefix_conflict_class_mismatch ~pkg = function
      | WrongPrefix { conflict_class; required_prefix } ->
          Printf.sprintf
            "Error in %s: package with conflict class '%s' requires name \
             prefix '%s'"
            pkg conflict_class required_prefix
      | WrongConflictClass { prefix; required_conflict_class } ->
          Printf.sprintf
            "Error in %s: package with prefix '%s' requires conflict class '%s'"
            pkg prefix required_conflict_class
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
      let filename = OpamUrl.to_string (OpamFile.URL.url url) in
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
            Some (Option.default "1.0" v)
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
          | Some _, Ok None -> [ (pkg, DuneProjectMissing) ]
          | None, Ok (Some _) ->
              if is_dune (OpamPackage.name pkg) then []
              else [ (pkg, DuneConstraintMissing) ]
          | Some dep, Ok (Some ver) ->
              if OpamVersionCompare.compare dep ver >= 0 then []
              else [ (pkg, BadDuneConstraint (dep, ver)) ]
        in
        if is_build then [ (pkg, DuneIsBuild) ] else errors
    | None -> []

  let opam_lint ~pkg opam =
    OpamFileTools.lint ~check_upstream:true opam
    |> List.map (fun x -> (pkg, OpamLint x))

  let is_perm_correct file =
    Unix.stat file |> function
    | { st_kind = S_REG; st_perm = 0o644; _ } -> true
    | { st_kind = S_REG; st_perm = 0o664; _ } -> true
    | _ -> false

  let check_package_dir ~repo_dir ~pkg _opam =
    let dir = path_from_pkg ~repo_dir pkg in
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

let msg_of_error (package, (err : Checks.error)) =
  let pkg = OpamPackage.to_string package in
  match err with
  | UnnecessaryField field ->
      Printf.sprintf
        "Warning in %s: Unnecessary field '%s'. It is suggested to remove it."
        pkg field
  | UnmatchedName value ->
      Printf.sprintf
        "Error in %s: The field 'name' that doesn't match its context. Field \
         'name' has value '%s' but was expected of value '%s'."
        pkg
        (OpamPackage.Name.to_string value)
        (OpamPackage.Name.to_string (OpamPackage.name package))
  | UnmatchedVersion value ->
      Printf.sprintf
        "Error in %s: The field 'version' that doesn't match its context. \
         Field 'version' has value '%s' but was expected of value '%s'."
        pkg
        (OpamPackage.Version.to_string value)
        (OpamPackage.Version.to_string (OpamPackage.version package))
  | DubiousDuneSubst ->
      Printf.sprintf
        "Warning in %s: Dubious use of 'dune subst'. 'dune subst' should \
         always only be called with {dev} (i.e. [\"dune\" \"subst\"] {dev}) If \
         your opam file has been autogenerated by dune, you need to upgrade \
         your dune-project to at least (lang dune 2.7)."
        pkg
  | WeakChecksum msg ->
      Printf.sprintf
        "Error in %s: Weak checksum algorithm(s) provided. Please use SHA-256 \
         or SHA-512. Details: %s"
        pkg msg
  | PinDepends ->
      Printf.sprintf
        "Error in %s: pin-depends present. This is not allowed in the \
         opam-repository."
        pkg
  | ExtraFiles ->
      Printf.sprintf
        "Error in %s: extra-files present. This is not allowed in the \
         opam-repository. Please use extra-source instead."
        pkg
  | FailedToDownload msg ->
      Printf.sprintf "Error in %s: Failed to download the archive. Details: %s"
        pkg msg
  | DuneProjectMissing ->
      Printf.sprintf
        "Warning in %s: The package seems to use dune but the dune-project \
         file is missing."
        pkg
  | DuneConstraintMissing ->
      Printf.sprintf
        "Warning in %s: The package has a dune-project file but no explicit \
         dependency on dune was found."
        pkg
  | BadDuneConstraint (dep, ver) ->
      Printf.sprintf
        "Error in %s: Your dune-project file indicates that this package \
         requires at least dune %s but your opam file only requires dune >= \
         %s. Please check which requirement is the right one, and fix the \
         other."
        pkg ver dep
  | DuneIsBuild ->
      Printf.sprintf
        "Warning in %s: The package tagged dune as a build dependency. Due to \
         a bug in dune (https://github.com/ocaml/dune/issues/2147) this should \
         never be the case. Please remove the {build} tag from its filter."
        pkg
  | RestrictedPrefix prefix ->
      Printf.sprintf "Warning in %s: package name has restricted prefix '%s'"
        pkg prefix
  | PrefixConflictClassMismatch mismatch ->
      Checks.Prefix.msg_of_prefix_conflict_class_mismatch ~pkg mismatch
  | NameCollision other_pkg ->
      Printf.sprintf "Warning in %s: Possible name collision with package '%s'"
        pkg other_pkg
  | UnexpectedFile file ->
      Printf.sprintf "Error in %s: Unexpected file in %s/%s" pkg
        (path_from_pkg ~repo_dir:"" package)
        file
  | ForbiddenPerm file ->
      Printf.sprintf
        "Error in %s: Forbidden permission for file %s/%s. All files should \
         have permissions 644."
        pkg
        (path_from_pkg ~repo_dir:"" package)
        file
  | OpamLint warn ->
      let warn = OpamFileTools.warns_to_string [ warn ] in
      Printf.sprintf "Error in %s: %s" pkg warn
  | ParseError ->
      Printf.sprintf "Error in %s: Failed to parse the opam file" pkg

let get_packages repo_dir = get_files (repo_dir // "packages")

let run_lint pkg newly_published repo_dir =
  let pkg = OpamPackage.of_string pkg in
  let opam_path = path_from_pkg ~repo_dir pkg // "opam" in
  (* NOTE: We use OpamFile.OPAM.read_from_channel instead of OpamFile.OPAM.file
     to prevent the name and version fields being automatically added *)
  In_channel.with_open_text opam_path (fun ic ->
      let opam =
        try Ok (OpamFile.OPAM.read_from_channel ic)
        with OpamPp.Bad_format e | OpamPp.Bad_version e -> Error e
      in
      let errors =
        match opam with
        | Ok opam ->
            let packages = get_packages repo_dir in
            Checks.run_checks ~repo_dir ~pkg ~packages ~newly_published opam
        | Error _ -> [ Checks.parse_error pkg ]
      in
      match errors with
      | [] -> print_endline "No errors"
      | _ ->
          errors |> List.iter (fun e -> e |> msg_of_error |> print_endline);
          exit 1)
