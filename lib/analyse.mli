module Analysis : sig
  type kind =
    | New
    | Deleted
    | Unavailable
    | SignificantlyChanged
    | UnsignificantlyChanged
  [@@deriving yojson]

  type data = {
    kind : kind;
    has_tests : bool;
  } [@@deriving yojson]

  type t [@@deriving yojson]

  val get_opam : cwd:Fpath.t -> string -> (string, unit) result Lwt.t
  val packages : t -> (OpamPackage.t * data) list
  val is_duniverse : t -> bool
  val equal : t -> t -> bool
end

val examine :
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
