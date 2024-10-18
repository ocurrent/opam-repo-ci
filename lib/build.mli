type result = Index.job_ids Current.t * Summary.t Current.t

val compilers :
  ?minimal:bool ->
  arch:Ocaml_version.arch ->
  build:
    (opam_version:[> `Dev ] ->
    lower_bounds:bool ->
    revdeps:bool ->
    string ->
    Variant.t ->
    'a) ->
  unit ->
  'a list

val linux_distributions :
  arch:Ocaml_version.arch ->
  build:
    (opam_version:[> `Dev ] ->
    lower_bounds:bool ->
    revdeps:bool ->
    string ->
    Variant.t ->
    'a) ->
  'a list

val macos :
  build:
    (opam_version:[> `Dev ] ->
    lower_bounds:bool ->
    revdeps:bool ->
    string ->
    Variant.t ->
    'a) ->
  'a list

val freebsd :
  build:
    (opam_version:[> `Dev ] ->
    lower_bounds:bool ->
    revdeps:bool ->
    string ->
    Variant.t ->
    'a) ->
  'a list

val extras :
  build:
    (opam_version:Opam_version.t ->
    lower_bounds:bool ->
    revdeps:bool ->
    string ->
    Variant.t ->
    'a) ->
  'a list

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
