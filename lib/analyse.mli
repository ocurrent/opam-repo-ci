module Analysis : sig
  type kind =
    | New
    | Deleted
    | SignificantlyChanged
    | UnsignificantlyChanged
  [@@deriving yojson]

  type t [@@deriving yojson]

  val packages : t -> (OpamPackage.t * kind) list
  val is_duniverse : t -> bool
end

val examine :
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
