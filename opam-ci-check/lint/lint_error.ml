(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

(** Some package name prefixes must be used along with specific conflict classes

    If either a restricted prefix or conflict class exists, then the
    corresponding other must also exist. *)
type prefix_conflict_class_mismatch =
  | WrongPrefix of { conflict_class : string; required_prefix : string }
  | WrongConflictClass of { prefix : string; required_conflict_class : string }

(** Errors detected during linting.

    Use {!string_of_error} to produce descriptions the errors *)
type error =
  | UnnecessaryField of string
      (** A field that is not needed and should be removed *)
  | UnmatchedName of (OpamPackage.Name.t * OpamPackage.Name.t)
      (** The [name] field does not match the package name *)
  | UnmatchedVersion of (OpamPackage.Version.t * OpamPackage.Version.t)
      (** The [version] field does not match the package version *)
  | DubiousDuneSubst
      (** The package uses [dune subst] without the {dev} flag *)
  | DuneIsBuild
      (** The package tags [dune] as a build dependency, which is not allowed *)
  | NoPackageSources
      (** The package's source directory could not be found, when one is required *)
  | UnexpectedFile of string
      (** An unexpected file was found in the package's source directory *)
  | ForbiddenPerm of string
      (** opam file in the package's source directory has forbidden permissions *)
  | OpamLint of (int * [ `Warning | `Error ] * string)
      (** An error or warning produced by [opam lint] *)
  | MaintainerWithoutContact of string list
      (** The package has maintainers but no way to contact them *)
  | NameCollision of string
      (** The package name is too similar to another package name in the repo *)
  | WeakChecksum of string
      (** The package uses a weak checksum algorithm (MD5) or has no checksum *)
  | PinDepends
      (** The package uses [pin-depends], which is not allowed in the opam-repository *)
  | ExtraFiles
      (** The package uses [extra-files], which is not allowed in the opam-repository *)
  | RestrictedPrefix of string
      (** The package name has a restricted prefix (e.g. "arch-", "system-") *)
  | PrefixConflictClassMismatch of prefix_conflict_class_mismatch
      (** The package has a restricted prefix without the corresponding conflict class, or vice versa *)
  | DefaultTagsPresent of string list
      (** The package has not replaced the default example tags *)
  | MissingUpperBound of string
      (** A dependency is missing an upper bound constraint *)
  | InvalidReasonForArchiving
      (** The opam file is missing the field or has an invalid [x-reason-for-archiving] field *)
  | InvalidOpamRepositoryCommitHash
      (** The opam file is missing the [x-opam-repository-commit-hash-at-time-of-archiving] field *)
  | InvalidConfPackage of [ `Conf_prefix | `Depext | `Conf_flag ] list
      (** A conf package is missing one of the required properties:
            - the 'conf-' name prefix
            - a non-empty 'depext' field
            - the 'conf' flag
        *)

(**/**)

let is_warning = function
  | UnnecessaryField _ | DubiousDuneSubst | DuneIsBuild | NameCollision _
  | DefaultTagsPresent _ | RestrictedPrefix _ ->
      true
  | _ -> false

(* These x_ fields are used in the opam repo archive *)
let x_reason_for_archiving_field = "x-reason-for-archiving"

let x_reason_for_archiving_valid_reasons =
  [
    "ocaml-version"; "source-unavailable"; "maintenance-intent"; "uninstallable";
  ]

let x_opam_repository_commit_hash_at_time_of_archiving_field =
  "x-opam-repository-commit-hash-at-time-of-archiving"

let msg_of_prefix_conflict_class_mismatch = function
  | WrongPrefix { conflict_class; required_prefix } ->
      Printf.sprintf
        "package with conflict class '%s' requires name prefix '%s'"
        conflict_class required_prefix
  | WrongConflictClass { prefix; required_conflict_class } ->
      Printf.sprintf "package with prefix '%s' requires conflict class '%s'"
        prefix required_conflict_class

(**/**)

(** [string_of_error] returns a string description of an error*)
let string_of_error = function
  | UnnecessaryField field ->
      Printf.sprintf "Unnecessary field '%s'. It is suggested to remove it."
        field
  | UnmatchedName (value, expected) ->
      Printf.sprintf
        "The field 'name' that doesn't match its context. Field 'name' has \
         value '%s' but was expected of value '%s'."
        (OpamPackage.Name.to_string value)
        (OpamPackage.Name.to_string expected)
  | UnmatchedVersion (value, expected) ->
      Printf.sprintf
        "The field 'version' that doesn't match its context. Field 'version' \
         has value '%s' but was expected of value '%s'."
        (OpamPackage.Version.to_string value)
        (OpamPackage.Version.to_string expected)
  | DubiousDuneSubst ->
      Printf.sprintf
        "Dubious use of 'dune subst'. 'dune subst' should always only be \
         called with {dev} (i.e. [\"dune\" \"subst\"] {dev}) If your opam file \
         has been autogenerated by dune, you need to upgrade your dune-project \
         to at least (lang dune 2.7)."
  | WeakChecksum msg ->
      Printf.sprintf
        "Weak checksum algorithm(s) provided. Please use SHA-256 or SHA-512. \
         Details: %s"
        msg
  | PinDepends ->
      Printf.sprintf
        "pin-depends present. This is not allowed in the opam-repository."
  | ExtraFiles ->
      Printf.sprintf
        "extra-files present. This is not allowed in the opam-repository. \
         Please use extra-source instead."
  | NoPackageSources -> Printf.sprintf "No package source directory provided."
  | DuneIsBuild ->
      Printf.sprintf
        "The package tagged dune as a build dependency. Due to a bug in dune \
         (https://github.com/ocaml/dune/issues/2147) this should never be the \
         case. Please remove the {build} tag from its filter."
  | RestrictedPrefix prefix ->
      Printf.sprintf "package name has restricted prefix '%s'" prefix
  | PrefixConflictClassMismatch mismatch ->
      msg_of_prefix_conflict_class_mismatch mismatch
  | NameCollision other_ ->
      Printf.sprintf "Possible name collision with package '%s'" other_
  | UnexpectedFile file -> Printf.sprintf "Unexpected file in %s" file
  | ForbiddenPerm file ->
      Printf.sprintf
        "Forbidden permission for file %s. All files should have permissions \
         644."
        file
  | OpamLint warn ->
      let warn = OpamFileTools.warns_to_string [ warn ] in
      String.trim warn |> Printf.sprintf "Opam lint %s"
  | MaintainerWithoutContact maintainer ->
      Printf.sprintf
        "There is no way to contact the maintainer(s) '%s'. A package must \
         either specify a url for 'bug-reports' or provide an email address in \
         the 'maintainer' field."
        (String.concat ", " maintainer)
  | DefaultTagsPresent tags ->
      Printf.sprintf
        "The package has not replaced the following default, example tags: %s"
        (String.concat ", " tags)
  | MissingUpperBound dep_name ->
      Printf.sprintf "An upper bound constraint is missing on dependency '%s'"
        dep_name
  | InvalidReasonForArchiving ->
      Printf.sprintf
        "The field '%s' must be present and hold a nonempty list of one or \
         more of the valid reasons %s"
        x_reason_for_archiving_field
        (String.concat ", " x_reason_for_archiving_valid_reasons)
  | InvalidOpamRepositoryCommitHash ->
      Printf.sprintf
        "The field '%s' must be present and hold a string recording the commit \
         hash of the primary repo at the time the package version is archived."
        x_opam_repository_commit_hash_at_time_of_archiving_field
  | InvalidConfPackage properties ->
      let property_descriptions =
        properties
        |> List.map (function
             | `Conf_flag -> "the 'conf' flag"
             | `Conf_prefix -> "the 'conf-' name prefix"
             | `Depext -> "a non-empty 'depext' field")
        |> String.concat " and "
      in
      Printf.sprintf
        "conf packages should always use the 'conf-' name prefix, the 'conf' \
         flag, and the 'depext' field all together, but this package only has \
         %s"
        property_descriptions

(** [msg_of_error (pkg, err)] is a string describing a linting [err] found for the [pkg] *)
let msg_of_error (package, (err : error)) =
  let pkg = OpamPackage.to_string package in
  let prefix = if is_warning err then "Warning" else "Error" in
  string_of_error err |> Printf.sprintf "%s in %s: %s" prefix pkg

(** [msg_of_errors ?machine_readable errors] is a string describing all the linting [errors].

    [errors] is a list of pairs of packages and the errors found for them. The
    function assumes that these list of pairs are ordered by package.

    If [machine_readable] is true, the output is a simple list of error
    messages, one per line.

    If false (the default), the output is more human-friendly, grouping errors
    by package and adding indentation and newlines for readability. *)
let msg_of_errors ?(machine_readable = false)
    (errors : (OpamPackage.t * error) list) =
  if machine_readable then
    errors |> List.map msg_of_error |> String.concat "\n"
    |> Printf.sprintf "%s\n"
  else
    let seen = ref OpamPackage.Set.empty in
    let spf = Printf.sprintf in
    let error_indent = "  " in
    errors
    |> List.map (fun (pkg, err) ->
           if OpamPackage.Set.mem pkg !seen then
             spf "%s- %s" error_indent (string_of_error err)
           else
             let heading = spf "Errors in %s:" (OpamPackage.to_string pkg) in
             let output =
               if OpamPackage.Set.is_empty !seen then heading
               else spf "\n%s" heading
             in
             seen := OpamPackage.Set.add pkg !seen;
             spf "%s\n%s- %s" output error_indent (string_of_error err))
    |> String.concat "\n"
