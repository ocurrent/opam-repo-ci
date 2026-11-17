



(** If either a restricted prefix or conflict class exists,
    then the corresponding other must also exist. *)
type prefix_conflict_class_mismatch =
  | WrongPrefix of { conflict_class : string; required_prefix : string }

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







    in


    in

  let get_dune_project_version ~pkg url =
    let res =
      OpamProcess.Job.run
      @@ OpamRepository.pull_tree
           (OpamPackage.to_string pkg)
           (OpamFilename.Dir.of_string dir)
           (OpamFile.URL.checksum url)
           [ OpamFile.URL.url url ]
    in
    match res with
        let dune_project = Filename.concat dir "dune-project" in

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
          is_build := true;
      | Empty | Atom (Filter _) | Atom (Constraint _) -> None
      | Block x -> get_lower_bound x
      | And (x, y) -> get_max (get_lower_bound x, get_lower_bound y)
      | Or (x, y) -> get_min (get_lower_bound x, get_lower_bound y)
    in
    let rec aux = function
      | OpamFormula.Atom (pkg, constr) ->
          if is_dune pkg then
            let v = get_lower_bound constr in
          else None
      | Empty -> None
      | Block x -> aux x
      | And (x, y) -> get_max (aux x, aux y)
      | Or (x, y) -> get_min (aux x, aux y)
    in
    (!is_build, aux opam.OpamFile.OPAM.depends)

    match opam.OpamFile.OPAM.url with
    | Some url ->
        let is_build, dune_constraint = get_dune_constraint opam in
        let errors =
          match (dune_constraint, dune_version) with
          | None, Ok (Some _) ->
          | Some dep, Ok (Some ver) ->
        in

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

    let pkg_name = pkg.OpamPackage.name |> OpamPackage.Name.to_string in
    let pkg_name_lower = String.lowercase_ascii pkg_name in
    let other_pkgs =
      List.filter (fun s -> not @@ String.equal s pkg_name) packages
    in
        let other_pkg_lower = String.lowercase_ascii other_pkg in
        if package_name_collision pkg_name_lower other_pkg_lower then


  end

    let pkg = OpamPackage.to_string package in
    match err with
    | UnnecessaryField field ->
          "Warning in %s: Unnecessary field '%s'. It is suggested to remove it."
          pkg field
    | UnmatchedName value ->
          "Error in %s: The field 'name' that doesn't match its context. Field \
           'name' has value '%s' but was expected of value '%s'."
          pkg
          (OpamPackage.Name.to_string value)
          (OpamPackage.Name.to_string (OpamPackage.name package))
    | UnmatchedVersion value ->
          "Error in %s: The field 'version' that doesn't match its context. \
           Field 'version' has value '%s' but was expected of value '%s'."
          pkg
          (OpamPackage.Version.to_string value)
          (OpamPackage.Version.to_string (OpamPackage.version package))
    | DubiousDuneSubst ->
          "Warning in %s: Dubious use of 'dune subst'. 'dune subst' should \
          pkg
    | DuneProjectMissing ->
          "Warning in %s: The package seems to use dune but the dune-project \
           file is missing."
          pkg
    | DuneConstraintMissing ->
          "Warning in %s: The package has a dune-project file but no explicit \
           dependency on dune was found."
          pkg
    | BadDuneConstraint (dep, ver) ->
          "Error in %s: Your dune-project file indicates that this package \
           requires at least dune %s but your opam file only requires dune >= \
           %s. Please check which requirement is the right one, and fix the \
           other."
          pkg ver dep
    | UnexpectedFile file ->
          file
    | ForbiddenPerm file ->
          "Error in %s: Forbidden permission for file %s/%s. All files should \
           have permissions 644."
          pkg
          file
    | OpamLint warn ->
        let warn = OpamFileTools.warns_to_string [ warn ] in

    match errors with
