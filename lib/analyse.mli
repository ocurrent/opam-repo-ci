module Analysis : sig
  type t [@@deriving yojson]

  val packages : t -> OpamPackage.t list
  val is_duniverse : t -> bool
  val ocamlformat_source : t -> Analyse_ocamlformat.source option
end

val examine :
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
