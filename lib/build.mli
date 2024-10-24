type result = Index.job_ids Current.t * Summary.t Current.t

type build_recipe = {
  opam_version: Opam_version.t;
  lower_bounds: bool;
  revdeps: bool;
  label: string;
  variant: Variant.t;
}

val compilers : ?minimal:bool -> arch:Ocaml_version.arch -> unit -> build_recipe list

val linux_distributions : arch:Ocaml_version.arch -> build_recipe list

val macos : unit -> build_recipe list

val freebsd : unit -> build_recipe list

val extras : unit -> build_recipe list

(** [with_cluster ~ocluster ~analysis ~master commit] runs all the
    necessary builds for [commit] relative to [master] using a server
    cluster through [connection]. *)
val with_cluster :
  ocluster:Cluster_build.t ->
  analysis:Analyse.Analysis.t Current.t ->
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit_id.t Current.t -> result Node.t list

(** [with_docker ~analysis ~master commit] runs all the necessary builds
    for [commit] relative to [master] using local Docker containers. *)
val with_docker :
  host_arch:Ocaml_version.arch ->
  analysis:Analyse.Analysis.t Current.t ->
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit_id.t Current.t -> result Node.t list
