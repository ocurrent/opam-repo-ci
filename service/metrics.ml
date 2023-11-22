open Prometheus

let namespace = "opamrepoci"
let subsystem = "pipeline"

let master =
  let help = "Number of PRs by state" in
  Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem
    "pr_state_total"

let update () =
  let open Opam_repo_ci in
  let n_per_status = Index.n_per_status () in
  Gauge.set (master "not_started") (float_of_int n_per_status.not_started);
  Gauge.set (master "pending") (float_of_int n_per_status.pending);
  Gauge.set (master "failed") (float_of_int n_per_status.failed);
  Gauge.set (master "passed") (float_of_int n_per_status.passed)
