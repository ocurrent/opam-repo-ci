(** Set the primary repository that the metrics will track. While multiple
    repositories can be added to [opam-repo-ci], we are typically only concerned
    with [ocaml/opam-repository]. *)
val set_primary_repo : Current_github.Repo_id.t -> unit

(** Recompute the metrics. *)
val update : unit -> unit
