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

    Use {!msg_of_error} to produce descriptions the errors *)
type error =
  | UnnecessaryField of string
  | UnmatchedName of OpamPackage.Name.t
  | UnmatchedVersion of OpamPackage.Version.t
  | DubiousDuneSubst
  | DuneProjectMissing
  | DuneProjectParseError of string
  | DuneDependencyMissing
  | DuneLowerBoundMissing
  | DuneIsBuild
  | BadDuneConstraint of string * string
  | NoPackageSources
  | UnexpectedFile of string
  | ForbiddenPerm of string
  | OpamLint of (int * [ `Warning | `Error ] * string)
  | MaintainerWithoutContact of string list
  | NameCollision of string
  | WeakChecksum of string
  | PinDepends
  | ExtraFiles
  | RestrictedPrefix of string
  | PrefixConflictClassMismatch of prefix_conflict_class_mismatch
  | DefaultTagsPresent of string list
  | MissingUpperBound of string
  | InvalidReasonForArchiving

(**/**)

let x_reason_for_archiving_field = "x-reason-for-archiving"
(* Used in the opam repo archive *)
let x_reason_for_archiving_valid_reasons =
  ["ocaml-version"; "source-unavailable"; "maintenance-intent"; "uninstallable" ]

let msg_of_prefix_conflict_class_mismatch ~pkg = function
  | WrongPrefix { conflict_class; required_prefix } ->
      Printf.sprintf
        "Error in %s: package with conflict class '%s' requires name prefix \
         '%s'"
        pkg conflict_class required_prefix
  | WrongConflictClass { prefix; required_conflict_class } ->
      Printf.sprintf
        "Error in %s: package with prefix '%s' requires conflict class '%s'" pkg
        prefix required_conflict_class

(**/**)

(** [msg_of_error (pkg, err)] is a string describing a linting [err] found for the [pkg] *)
let msg_of_error (package, (err : error)) =
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
  | NoPackageSources ->
      Printf.sprintf "Error in %s: No package source directory provided." pkg
  | DuneProjectParseError msg ->
      Printf.sprintf "Error in %s: Failed to parse dune-project file with: '%s'"
        pkg msg
  | DuneProjectMissing ->
      Printf.sprintf
        "Warning in %s: The package seems to use dune but the dune-project \
         file is missing."
        pkg
  | DuneDependencyMissing ->
      Printf.sprintf
        "Warning in %s: The package has a dune-project file but no explicit \
         dependency on dune was found."
        pkg
  | DuneLowerBoundMissing ->
      Printf.sprintf
        "Warning in %s: The package has a dune dependency without a lower \
         bound."
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
      msg_of_prefix_conflict_class_mismatch ~pkg mismatch
  | NameCollision other_pkg ->
      Printf.sprintf "Warning in %s: Possible name collision with package '%s'"
        pkg other_pkg
  | UnexpectedFile file ->
      Printf.sprintf "Error in %s: Unexpected file in %s/%s" pkg
        (Opam_helpers.path_from_pkg ~opam_repo_dir:"" package)
        file
  | ForbiddenPerm file ->
      Printf.sprintf
        "Error in %s: Forbidden permission for file %s/%s. All files should \
         have permissions 644."
        pkg
        (Opam_helpers.path_from_pkg ~opam_repo_dir:"" package)
        (Filename.basename file)
  | OpamLint warn ->
      let warn = OpamFileTools.warns_to_string [ warn ] in
      Printf.sprintf "Error in %s: %s" pkg warn
  | MaintainerWithoutContact maintainer ->
      Printf.sprintf
        "Error in %s: There is no way to contact the maintainer(s) '%s'. A \
         package must either specify a url for 'bug-reports' or provide an \
         email address in the 'maintainer' field."
        pkg
        (String.concat ", " maintainer)
  | DefaultTagsPresent tags ->
      Printf.sprintf
        "Warning in %s: The package has not replaced the following default, \
         example tags: %s"
        pkg (String.concat ", " tags)
  | MissingUpperBound dep_name ->
      Printf.sprintf
        "Error in %s: An upper bound constraint is missing on dependency '%s'"
        pkg dep_name
  | InvalidReasonForArchiving ->
      Printf.sprintf
        "Error in %s: The field '%s' must be present and hold a nonempty list \
         of one or more of the valid reasons %s"
        pkg x_reason_for_archiving_field
        (String.concat ", " x_reason_for_archiving_valid_reasons)
