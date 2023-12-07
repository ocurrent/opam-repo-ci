(** The index is:
    - A map from active Git references to the Git commit at their heads.
    - A map from project builds ([owner * name * hash)] triples) to statuses.
    - A (persisted) map from each Git commit hash to its last known OCurrent job ID. *)

type job_state = [`Not_started | `Active | `Failed of string | `Passed | `Aborted ] [@@deriving show]

type build_status = [ `Not_started | `Pending | `Failed | `Passed ]

val init : unit -> unit
(** Ensure the database is initialised (for unit-tests). *)

module Job_map : module type of Astring.String.Map

type job_ids = Current.job_id option Job_map.t

val record :
  repo:Current_github.Repo_id.t ->
  hash:string ->
  job_ids ->
  unit
(** [record ~repo ~hash jobs] updates the entry for [repo, hash] to point at [jobs]. *)

val is_known_repo : owner:string -> name:string -> bool
(** [is_known_repo ~owner ~name] is [true] iff there is an entry for a commit in repository [owner/name]. *)

val list_repos : string -> string list
(** [list_repos owner] lists all the tracked repos under [owner]. *)

val get_jobs : owner:string -> name:string -> string -> (string * job_state) list
(** [get_jobs ~owner ~name commit] is the last known set of OCurrent jobs for hash [commit] in repository [owner/name]. *)

val get_job : owner:string -> name:string -> hash:string -> variant:string -> (string option, [> `No_such_variant]) result
(** [get_job ~owner ~name ~variant] is the last known job ID for this combination. *)

val get_job_ids: owner:string -> name:string -> hash:string -> string list

val get_build_status:
  owner:string ->
  name:string ->
  hash:string ->
  build_status
(** [get_build_status ~owner ~name ~hash] is the latest status for this combination. *)

val get_n_jobs:
  owner:string ->
  name:string ->
  hash:string ->
  int
(** [get_n_jobs ~owner ~name ~hash] is the latest number of jobs for this combination. *)

val set_status:
  owner:string ->
  name:string ->
  hash:string ->
  build_status * int ->
  unit
(** [set_status ~owner ~name ~hash build_status] sets the latest status and number of jobs for this combination. *)

type n_per_status_t = { not_started : int; pending : int; failed : int; passed : int }

val get_n_per_status : unit -> n_per_status_t

val get_n_handled : unit -> int

val get_full_hash : owner:string -> name:string -> string -> (string, [> `Ambiguous | `Unknown | `Invalid]) result
(** [get_full_hash ~owner ~name short_hash] returns the full hash for [short_hash]. *)

module Account_set : Set.S with type elt = string
module Repo_map : Map.S with type key = Current_github.Repo_id.t

val set_active_accounts : Account_set.t -> unit
(** [set_active_accounts accounts] record that [accounts] is the set of accounts on which the CI is installed.
    This is displayed in the CI web interface. *)

val get_active_accounts : unit -> Account_set.t
(** [get_active_accounts ()] is the last value passed to [set_active_accounts], or [[]] if not known yet. *)

val set_active_refs : repo:Current_github.Repo_id.t -> (string * string) list -> unit
(** [set_active_refs ~repo entries] records that [entries] is the current set of Git references that the CI
    is watching. There is one entry for each branch and PR. Each entry is a pair of the Git reference name
    and the head commit's hash. *)

val get_active_refs : Current_github.Repo_id.t -> (string * string) list
(** [get_active_refs repo] is the entries last set for [repo] with [set_active_refs], or
    [] if this repository isn't known. *)

val get_jobs_per_ref : Current_github.Repo_id.t -> (string * int) list
(** [get_jobs_per_ref repo] is the number of jobs run for each ref in [repo]. *)
