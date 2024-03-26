(** Utility program for testing the CI pipeline locally. *)

open Capnp_rpc_lwt
open Lwt.Infix

let () =
  Memtrace.trace_if_requested ~context:"opam-repo-ci-local" ();
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Prometheus_unix.Logging.init ()

let setup_capnp ~engine ~listen_address secret_key cap_file capnp_address =
  Capnp_setup.run ~listen_address ~secret_key ~cap_file capnp_address
  >|= fun (_, rpc_engine_resolver) ->
  Option.iter
    (fun r -> Capability.resolve_ok r (Api_impl.make_ci ~engine))
    rpc_engine_resolver

let web_server_thread ~mode engine no_web_server =
  if no_web_server then []
  else
    let routes = Current_web.routes engine in
    let site =
      Current_web.Site.(v
        ~has_role:allow_all) ~name:"opam-repo-ci-local" routes
    in
    [ Current_web.run ~mode site ]

let main config mode capnp_address repo branch test_config no_web_server level =
  Logs.set_level level;
  Lwt_main.run begin
    let repo = Current_git.Local.v (Result.get_ok @@ Fpath.of_string repo) in
    let engine =
      Current.Engine.create ~config
        (Pipeline.local_test_pr ?test_config repo branch)
    in
    let listen_address =
      Capnp_rpc_unix.Network.Location.tcp
        ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port
    in
    setup_capnp ~engine ~listen_address Conf.Capnp.secret_key
      Conf.Capnp.cap_file capnp_address >>= fun () ->
    let web_server_thread = web_server_thread ~mode engine no_web_server in
    Lwt.choose ([
      Current.Engine.thread engine;
    ] @ web_server_thread)
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

let integration_test_config =
  let lint_only =
    Arg.info
      ~doc:"Run lint check and then exit. Used for integration testing."
      ~docv:"LINT_ONLY"
      ["lint-only"]
  in
  let revdeps_only =
    Arg.info
      ~doc:"List revdeps and then exit. Used for integration testing."
      ~docv:"REVDEPS_ONLY"
      ["revdeps-only"]
  in
  let open Opam_repo_ci in
  Arg.value @@
  Arg.vflag None [
    (Some Integration_test.Lint, lint_only);
    (Some List_revdeps, revdeps_only);
  ]

let no_web_server =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Don't run web server. Used for integration testing to prevent port conflicts."
    ~docv:"NO_WEB_SERVER"
    ["no-web-server"]

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
      $ integration_test_config
      $ no_web_server
      $ Logs_cli.level ()))

let () = exit @@ Cmd.eval cmd
