(* Utility program for testing the CI pipeline locally. *)

open Capnp_rpc_lwt
open Lwt.Infix

module Github = Current_github

let opam_repository = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" }

let () =
  Memtrace.trace_if_requested ~context:"opam-repo-ci-local" ();
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Prometheus_unix.Logging.init ();
  Metrics.set_primary_repo opam_repository;
  Prometheus.CollectorRegistry.(register_pre_collect default) Metrics.update

let main config mode capnp_address submission_uri api repo_id prometheus_config =
  Lwt_main.run begin
    Capnp_setup.run capnp_address >>= fun (vat, rpc_engine_resolver) ->
    let repo = (api, repo_id) in
    let ocluster = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
    let engine = Current.Engine.create ~config (Pipeline.local_test ~ocluster repo) in
    rpc_engine_resolver |> Option.iter (fun r -> Capability.resolve_ok r (Api_impl.make_ci ~engine));
    let routes = Current_web.routes engine in
    let site = Current_web.Site.(v ~has_role:allow_all) ~name:"opam-repo-ci-local" routes in
    let prometheus =
      List.map
        (Lwt.map @@ Result.ok)
        (Prometheus_unix.serve prometheus_config)
    in
    Lwt.choose ([
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ] @ prometheus)
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.value @@
  Arg.opt Github.Repo_id.cmdliner opam_repository @@
  Arg.info
    ~doc:"The GitHub repository (owner/name) to monitor."
    ~docv:"REPO"
    ["repo"]

let submission_service =
  Arg.required @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE"
    ["submission-service"]

let cmd =
  let doc = "Test opam-repo-ci on a local Git clone" in
  let info = Cmd.info "opam-repo-ci-local" ~doc in
  Cmd.v info
    Term.(term_result (
      const main
      $ Current.Config.cmdliner
      $ Current_web.cmdliner
      $ Capnp_setup.cmdliner
      $ submission_service
      $ Current_github.Api.cmdliner
      $ repo
      $ Prometheus_unix.opts))

let () = exit @@ Cmd.eval cmd
