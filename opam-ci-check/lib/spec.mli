(** Defines the configuration needed to then generate an OBuilder spec *)

(** The base image from which the job starts running.
    MacOS and FreeBSD do not yet have Docker images and
    start from custom base images stored in the workers. *)
type base =
  | Docker of string
  | Macos of string
  | Freebsd of string

val base_to_string : base -> string

type package = OpamPackage.t

(** Configuration for an [opam_build] job *)
type opam_build = {
  revdep : package option;
  with_tests : bool;
  lower_bounds : bool;
  opam_version : Opam_version.t;
}

(** Configuration for a [list_revdeps] job *)
type list_revdeps = {
  opam_version : [ `Dev | `V2_0 | `V2_1 | `V2_2 | `V2_3 ];
}

(** Configuration for any job along with the package to build *)
type ty = [
  `Opam of [ `Build of opam_build | `List_revdeps of list_revdeps ] * package
] [@@deriving to_yojson]

type t = { variant : Variant.t; ty : ty; }

(** Generate configuration for an [opam_build] job *)
val opam :
  ?revdep:package ->
  variant:Variant.t ->
  lower_bounds:bool ->
  with_tests:bool -> opam_version:Opam_version.t -> package -> t

val pp_ty :
  Format.formatter ->
  [< `Opam of
       [< `Build of opam_build | `List_revdeps of list_revdeps ] *
       OpamPackage.t ] ->
  unit

val pp_summary :
  Format.formatter ->
  [< `Opam of [< `Build of 'a | `List_revdeps of 'b ] * 'c ] -> unit
