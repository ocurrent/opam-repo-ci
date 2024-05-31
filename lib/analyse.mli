type kind =
  | New
  | Deleted
  | Unavailable
  | SignificantlyChanged
  | InsignificantlyChanged
[@@deriving yojson]

type data = {
  kind : kind;
  has_tests : bool;
} [@@deriving yojson]

(** Used in OCurrent pipeline *)
module Analysis : sig
  type t [@@deriving yojson]

  val get_opam : cwd:Fpath.t -> string -> (string, unit) result Lwt.t
  val packages : t -> (OpamPackage.t * data) list
  val is_duniverse : t -> bool
  val equal : t -> t -> bool
end

(** Used for local tools *)
module Local_analysis : sig
  type t = { packages : (OpamPackage.t * data) list; }

  val of_dir :
    unit ->
    master:Current_git.Commit.t ->
    Fpath.t -> (t, [ `Msg of string ]) result Lwt.t
end

val examine :
  ?test_config:Integration_test.t ->
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
