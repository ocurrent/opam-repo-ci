(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

include module type of Lint_error

module Checks : sig
  val package_name_collision : string -> string -> bool
  (** [package_name_collision p0 p1] returns true if [p0] is similar to [p1].
    Similarity is defined to be:

    - Case-insensitive string equality considering underscores ([_])
      dashes ([-]), and the empty string to be equal *)
end

type t

val v :
  pkg:OpamPackage.t ->
  newly_published:bool ->
  pkg_src_dir:string option ->
  OpamFile.OPAM.t ->
  t
(** Create an object of type [t]
    @param pkg Package that is being linted.
    @param pkg_src_dir The path to a directory containing the package sources.
    @param newly_published Flag to indicate if the [pkg] is being newly
      published to the repository, meaning no previous versions of the package
      have been published. Some additional checks are run for such packages.
    @param opam Parsed OPAM metadata for the package.
 *)

val lint_packages :
  opam_repo_dir:string ->
  t list ->
  ((OpamPackage.t * error) list, string) result
(** [lint_packages ~opam_repo_dir metas] is a list of all the
    errors detected while linting the packages in the [metas] list in the
    context of the opam repository located at [opam_repo_dir].

    @param opam_repo_dir The path a local opam repository.

    Examples:

    {[

      let passes_all_checks = assert (lint_packages ~opam_repo_dir metas |> Result.get_ok |> List.length = 0)
      let failed_some_checks = assert (lint_packages ~opam_repo_dir metas |> Result.get_ok |> List.length > 0)
      let messages_for_all_failed_checks =
        lint_packages ~opam_repo_dir ~repo_packages metas
        |> Result.get_ok |> List.map msg_of_error
    ]} *)
