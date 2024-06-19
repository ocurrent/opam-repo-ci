module D = Dir_helpers

module Checks = struct
  type error =
    | UnnecessaryField of string
    | UnmatchedName of OpamPackage.Name.t
    | UnmatchedVersion of OpamPackage.Version.t
    | DubiousDuneSubst
    | DuneProjectMissing
    | DuneConstraintMissing
    | DuneIsBuild
    | BadDuneConstraint of string * string
    | FailedToDownload of string
    | WeakChecksum of string
    | PinDepends
    | ExtraFiles

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

  let checks =
    [
      check_name_field;
      check_version_field;
      check_dune_subst;
      check_dune_constraints;
      check_checksums;
      check_no_pin_depends;
      check_no_extra_files;
    ]

  let run_checks ~pkg opam =
    checks |> List.map (fun f -> f ~pkg opam) |> List.concat
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

let opam_file_path_from_pkg ~repo_dir pkg =
  let ( // ) = Filename.concat in
  repo_dir // "packages"
  // OpamPackage.Name.to_string (OpamPackage.name pkg)
  // OpamPackage.to_string pkg // "opam"

let run_lint pkg repo_dir =
  let pkg = OpamPackage.of_string pkg in
  let opam_path = opam_file_path_from_pkg ~repo_dir pkg in
  (* NOTE: We use OpamFile.OPAM.read_from_channel instead of OpamFile.OPAM.file
     to prevent the name and version fields being automatically added *)
  In_channel.with_open_text opam_path (fun ic ->
      let opam = OpamFile.OPAM.read_from_channel ic in
      let errors = Checks.run_checks ~pkg opam in
      match errors with
      | [] -> print_endline "No errors"
      | _ ->
          errors |> List.iter (fun e -> e |> msg_of_error |> print_endline);
          exit 1)
