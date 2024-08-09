(** Analyze and test the reverse dependencies of a package. *)

val list_revdeps :
  ?opam_repo:string -> ?transitive:bool -> string -> OpamPackage.t list
(** [list_revdeps pkg] is a list of the transitive reverse dependencies of [pkg].

    @param opam_repo The package repository to use when calculating
    dependencies. Defaults to ["https://opam.ocaml.org"].

    @param transitive Whether or to list all transitive reverse dependencies.
    Defaults to [true].

    @param pkg The package for which reverse dependencies will be listed, in a
    form like ["pkgname.0.0.1"].

    Examples:

    {[
      let transitive_revdeps =
        list_revdeps "cmdliner.1.1.1"

      let nontransitive_revdeps =
        list_revdeps ~transitive:false "cmdliner.1.1.1"

      let nontransitive_revdeps_from_custom_repo =
        list_revdeps ~opam_repo:"/path/to/my/repo" ~transitive:false "cmdliner.1.1.1"
    ]}
*)

val find_latest_versions : OpamPackage.t list -> OpamPackage.t list
(** [find_latest_versions packages] is a list containing just the latest
    versions of each package in [packages].  *)

module Display : sig
  (** Display information about reverse dependencies *)

  val packages : OpamPackage.t list -> unit
  (** [packages ps] writes all the packages in [ps] to stdout. *)
end
