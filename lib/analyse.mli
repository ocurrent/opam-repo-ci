module Analysis : sig
  type t [@@deriving yojson]

  val packages : t -> OpamPackage.t list
  val is_duniverse : t -> bool
end

val examine :
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
