(* val namespace : string
val subsystem : string
val master : string -> Prometheus.Gauge.t
val handled_prs : Prometheus.Gauge.t
val jobs_per_pr : string -> Prometheus.Gauge.t *)
(* val primary_repo : Current_github.Repo_id.t option ref *)
val set_primary_repo : Current_github.Repo_id.t -> unit
val update : unit -> unit
