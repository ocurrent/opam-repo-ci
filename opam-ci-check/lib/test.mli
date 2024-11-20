(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

type error = OpamPackage.t * exn
(** A package and the exception recording its failure to install or pass tests. *)

val error_to_string : error -> string

val test_packages_with_opam : string -> OpamPackage.t list -> error Seq.t
(** [test_packages_with_opam package revdeps] is the sequence of errors encountered
    while trying to install and test all the packages in [revdeps].

    Examples:

    {[
      let no_errors_encountered = assert (test_packages_with_opam cmdliner.1.1.1 revdeps |> Seq.length = 0)
      let errors_encountered = assert (test_packages_with_opam cmdliner.1.1.1 revdeps |> Seq.length > 0)
    ]}
  *)

val test_packages_with_dune :
  string -> string -> OpamPackage.t list -> (unit, 'a) result

val build_run_spec :
  ?use_cache:bool ->
  ?only_print:bool ->
  ?opam_repository:string ->
  base:Spec.base ->
  Spec.t ->
  (unit, Rresult.R.msg) result
