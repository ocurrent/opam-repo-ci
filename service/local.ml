(** Utility program for testing the CI pipeline locally. *)

open Capnp_rpc_lwt
open Lwt.Infix

let () =
  Memtrace.trace_if_requested ~context:"opam-repo-ci-local" ();
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Prometheus_unix.Logging.init ()

(* Test configuration for integration testing *)
let get_test_config lint_only =
  if lint_only then
    Some Opam_repo_ci.Integration_test.Lint
  else
    None

let setup_capnp ~engine ~listen_address secret_key cap_file capnp_address =
  Capnp_setup.run ~listen_address ~secret_key ~cap_file capnp_address
  >|= fun (_, rpc_engine_resolver) ->
  Option.iter
    (fun r -> Capability.resolve_ok r (Api_impl.make_ci ~engine))
    rpc_engine_resolver

let main config mode capnp_address repo branch lint_only level =
  Logs.set_level level;
  Lwt_main.run begin
    let repo = Current_git.Local.v (Result.get_ok @@ Fpath.of_string repo) in
    let engine =
      Current.Engine.create ~config
        (Pipeline.local_test_pr ?test_config:(get_test_config lint_only) repo branch)
    in
    let listen_address =
      Capnp_rpc_unix.Network.Location.tcp
        ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port
    in
    setup_capnp ~engine ~listen_address Conf.Capnp.secret_key
      Conf.Capnp.cap_file capnp_address >>= fun () ->
    let routes = Current_web.routes engine in
    let site =
      Current_web.Site.(v
        ~has_role:allow_all) ~name:"opam-repo-ci-local" routes
    in
    Lwt.choose ([
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ])
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.required @@
  Arg.opt Arg.(some dir) None @@
  Arg.info
    ~doc:"The path of the local Git repository to monitor"
    ~docv:"REPO"
    ["repo"]

let branch =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"The name of the branch that contains your changes"
    ~docv:"BRANCH"
    ["branch"]

let lint_only =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Run lint check and then exit. Used for integration testing"
    ~docv:"LINT_ONLY"
    ["lint-only"]

let cmd =
  let doc = "Test opam-repo-ci on a local Git repository" in
  let info = Cmd.info "opam-repo-ci-local" ~doc in
  Cmd.v info
    Term.(term_result (
      const main
      $ Current.Config.cmdliner
      $ Current_web.cmdliner
      $ Capnp_setup.cmdliner
      $ repo
      $ branch
      $ lint_only
      $ Logs_cli.level ()))

let () = exit @@ Cmd.eval cmd
