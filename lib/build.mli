module Spec : sig
  type t

  val opam :
    label:string ->
    platform:Platform.t ->
    analysis:Analyse.Analysis.t ->
    [ `Build | `Lint of [ `Doc | `Fmt ] ] ->
    t

  val pp : t Fmt.t
  val compare : t -> t -> int
  val label : t -> string
end

val pread :
  spec:Spec.t Current.t ->
  Current_docker.Raw.Image.t ->
  args:string list ->
  string Current.t

val pull :
  schedule:Current_cache.Schedule.t ->
  Spec.t Current.t ->
  Current_docker.Raw.Image.t Current.t

val v :
  spec:Spec.t Current.t ->
  base:Current_docker.Raw.Image.t Current.t ->
  ?revdep:OpamPackage.t Current.t ->
  with_tests:bool ->
  pkg:OpamPackage.t Current.t ->
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Current_docker.Raw.Image.t Current.t
