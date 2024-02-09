(** [with_cluster ~ocluster ~analysis ~lint ~master commit] runs all the
    necessary builds for [commit] relative to [master] using a server
    cluster through [connection]. *)
val with_cluster :
  ocluster:Opam_repo_ci.Cluster_build.t ->
  analysis:Opam_repo_ci.Analyse.Analysis.t Current.t ->
  lint:unit Current.t ->
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit_id.t Current.t -> 'b Node.t list

(** [with_docker ~analysis ~lint ~master commit] runs all the necessary builds
    for [commit] relative to [master] using local Docker containers. *)
val with_docker :
  analysis:Opam_repo_ci.Analyse.Analysis.t Current.t ->
  lint:unit Current.t ->
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit_id.t Current.t -> 'b Node.t list
