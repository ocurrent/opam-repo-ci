(** [v ~label ~spec ~master ~urgent ~base commit] runs the build specified
    by [spec], on top of [base], with the new [commit] as compared to the
    [master] branch. This job is run on a local Docker container.
    The job is labelled [label]. [urgent] has no effect, but is included
    to conform with the interface for [Cluster_build]. *)
val v :
  label:string ->
  spec:Opam_ci_check.Spec.t Current.t ->
  base:Opam_ci_check.Spec.base Current.t ->
  master:Current_git.Commit.t Current.t ->
  urgent:([ `High | `Low ] -> bool) option Current.t ->
  Current_git.Commit_id.t Current.t -> unit Current.t

(** [list_revdeps ~variant ~opam_version ~pkgopt ~base ~master ~after commit]
    lists the set of reverse dependencies of the package specified by [pkgopt],
    as modified in [commit] relative to the [master] branch. The job is run
    on a local Docker container.

    The spec is generated on top of [base], and the OCurrent job is run after
    the job specified by [after], making it a dependency. *)
val list_revdeps :
  variant:Opam_ci_check.Variant.t ->
  opam_version:Opam_ci_check.Opam_version.t ->
  pkgopt:Package_opt.t Current.t ->
  new_pkgs:OpamPackage.t list Current.t ->
  base:Opam_ci_check.Spec.base Current.t ->
  master:Current_git.Commit.t Current.t ->
  after:unit Current.t ->
  Current_git.Commit_id.t Current.t -> OpamPackage.Set.t Current.t
