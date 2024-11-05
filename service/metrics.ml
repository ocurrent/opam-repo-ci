open Prometheus

let namespace = "opamrepoci"
let subsystem = "pipeline"

let master =
  let help = "Number of PRs by state" in
  Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem
    "pr_state_total"

let handled_prs =
  let help = "Number of PRs closed or merged" in
  Gauge.v ~help ~namespace ~subsystem "pr_handled"

let jobs_per_pr =
  let help = "Number of jobs per PR" in
  Gauge.v_labels ~label_names:["ref"; "status"] ~help ~namespace ~subsystem
    "jobs_per_pr"

let primary_repo : Current_github.Repo_id.t option ref = ref None

let set_primary_repo repo = primary_repo := Some repo

(** Maps e.g. refs/pull/123/head to #123 *)
let prettify_ref ref =
  let l = Astring.String.cuts ~sep:"/" ref in
  match List.nth_opt l 2 with
  | None -> ref
  | Some ref -> Printf.sprintf "#%s" ref

let update () =
  let open Opam_repo_ci in
  let n_per_status = Index.get_n_per_status () in
  Gauge.set (master "not_started") (float_of_int n_per_status.not_started);
  Gauge.set (master "pending") (float_of_int n_per_status.pending);
  Gauge.set (master "failed") (float_of_int n_per_status.failed);
  Gauge.set (master "passed") (float_of_int n_per_status.passed);
  let n_handled = Index.get_n_handled () in
  Gauge.set handled_prs (float_of_int n_handled);
  let f repo =
    let jobs_per_ref =
      Index.get_jobs_per_ref repo
      |> List.sort (fun (_, s0) (_, s1) -> Int.compare (Summary.sum s0) (Summary.sum s1))
    in
    List.iter (fun (ref, s) ->
      Gauge.set (Gauge.labels jobs_per_pr ([prettify_ref ref; "lint"]))
        (float_of_int s.Summary.lint);
      Gauge.set (Gauge.labels jobs_per_pr ([prettify_ref ref; "skip"]))
        (float_of_int s.skip);
      Gauge.set (Gauge.labels jobs_per_pr ([prettify_ref ref; "error"]))
        (float_of_int s.err);
      Gauge.set (Gauge.labels jobs_per_pr ([prettify_ref ref; "pending"]))
        (float_of_int s.pending);
      Gauge.set (Gauge.labels jobs_per_pr ([prettify_ref ref; "ok"]))
        (float_of_int s.ok))
      jobs_per_ref
  in
  Stdlib.Option.iter f !primary_repo
