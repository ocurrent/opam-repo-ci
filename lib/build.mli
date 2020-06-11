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

val v :
  schedule:Current_cache.Schedule.t ->
  spec:Spec.t Current.t ->
  revdep:string option ->
  with_tests:bool ->
  pkg:string ->
  Current_git.Commit.t Current.t ->
  Current_docker.Raw.Image.t Current.t
