open Capnp_rpc_lwt
open Lwt.Infix

let () =
  Printexc.record_backtrace true;
  Printexc.register_printer (fun e -> Some (Printexc.to_string_default e ^ " -- " ^ Printexc.get_backtrace ()));
  Memtrace.trace_if_requested ~context:"opam-repo-ci" ();
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Prometheus_unix.Logging.init ();
  Prometheus.CollectorRegistry.(register_pre_collect default) Metrics.update;
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  match Conf.profile with
  | `Production -> Logs.info (fun f -> f "Using production configuration")
  | `Staging -> Logs.info (fun f -> f "Using staging configuration")
  | `Dev -> Logs.info (fun f -> f "Using dev configuration")

(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | _ ->
    match Option.map Current_web.User.id user with
    | Some ( "github:talex5"
           | "github:avsm"
           | "github:kit-ty-kate"
           | "github:samoht"
           | "github:tmcgilchrist"
           | "github:benmandrew"
           ) -> true
    | _ -> false

let add_default_matching_log_rules () =
  let default_rules =
    let open Current.Log_matcher in
    [
      { (* Opam when the package or one of its dependencies has available: <non-compatible-condition> *)
        pattern = {|[\n]\[ERROR\] .+ unmet availability conditions: .+[\n]|};
        report = {|[SKIP] Package not available|};
        score = 100;
      };
      { (* Opam 2.0's install when the package or one of its dependencies isn't available on the current switch/plateform *)
        pattern = {|[\n]\[ERROR\] No solution for .+: The following dependencies couldn't be met:[\n]|};
        report = {|[SKIP] Package not available|};
        score = 100;
      };
      { (* Opam 2.0's reinstall when the package or one of its dependencies isn't available on the current switch/plateform *)
        pattern = {|[\n]\[ERROR\] No solution for .+: Your request can't be satisfied:[\n]|};
        report = {|[SKIP] Package not available|};
        score = 100;
      };
      { (* Opam 2.1's install when the package or one of its dependencies isn't available on the current switch/plateform *)
        pattern = {|[\n]\[ERROR\] Package conflict![\n]|};
        report = {|[SKIP] Package not available|};
        score = 100;
      };
      { (* Opam 2.1's reinstall when the package or one of its dependencies isn't available on the current switch/plateform *)
        pattern = {|[\n]No solution found, exiting[\n]|};
        report = {|[SKIP] Package not available|};
        score = 100;
      };
      { (* Opam 2.1 when a system package is not available on the current platform *)
        pattern = {|[\n]\[ERROR\] Package .+ depends on the unavailable system package.+\. You can use `--no-depexts' to attempt installation anyway\.[\n]|};
        report = {|[SKIP] Package not available|};
        score = 100;
      };
      { (* Ignore failures on failing packages when the platform being tested is contained in the x-ci-accept-failures optional field *)
        (* See lib/opam_build.ml corresponding bash command printing this exact line *)
        pattern = {|[\n]A package failed and has been disabled for CI using the 'x-ci-accept-failures' field\.[\n]|};
        report = {|[SKIP] Failure ignored|};
        score = 100;
      };
      { (* Catches failures when building dependencies *)
        pattern = {|[\n]opam-repo-ci detected dependencies failing: (.*)[\n]|};
        report = {|\1 failed to build|};
        score = 75;
      };
      { (* e.g. build: ["bash"] on platforms without bash will result in this error *)
        pattern = {|[\n]# bwrap: execvp (.+): No such file or directory[\n]|};
        report = {|\1 not found|};
        score = 50;
      };
      { (* OCaml errors *)
        pattern = {|[\n]# Error: (.+)[\n]|};
        report = {|\1|};
        score = 48;
      };
      { (* OCaml Exceptions *)
        pattern = {|[\n]# Exception: (.+)[\n]|};
        report = {|\1|};
        score = 45;
      };
      { (* Generic errors caught by opam (e.g. cargo) *)
        pattern = {|[\n]# error: (.+)[\n]|};
        report = {|\1|};
        score = 40;
      };
      { (* Generic error caught by opam (e.g. uncaught OCaml exception) *)
        pattern = {|[\n]# Fatal error: (.+)[\n]|};
        report = {|\1|};
        score = 35;
      };
      { (* Generic errors caught by opam (e.g. gcc) *)
        pattern = {|[\n]# .+: error: (.+)[\n]|};
        report = {|\1|};
        score = 30;
      };
      { (* Generic opam fetching error *)
        pattern = {|[\n]\[ERROR\] (Failed to get sources of .+: .+)[\n]|};
        report = {|\1|};
        score = 25;
      };
      { (* Opam errors *)
        pattern = {|[\n]\[ERROR\] (.+)[\n]|};
        report = {|\1|};
        score = 20;
      };
    ]
  in
  List.iter Current.Log_matcher.add_rule default_rules

let main config mode app capnp_address github_auth submission_uri prometheus_config level =
  add_default_matching_log_rules ();
  Logs.set_level level;
  Lwt_main.run begin
    let listen_address = Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port in
    Capnp_setup.run ~listen_address capnp_address >>= fun (vat, rpc_engine_resolver) ->
    let ocluster = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
    let engine = Current.Engine.create ~config (Pipeline.v ~ocluster ~app) in
    rpc_engine_resolver |> Option.iter (fun r -> Capability.resolve_ok r (Api_impl.make_ci ~engine));
    let authn = Option.map Current_github.Auth.make_login_uri github_auth in
    let webhook_secret = Current_github.App.webhook_secret app in
    let has_role =
      if github_auth = None then Current_web.Site.allow_all
      else has_role
    in
    let routes =
      Routes.(s "webhooks" / s "github" /? nil @--> Current_github.webhook ~engine ~get_job_ids:Opam_repo_ci.Index.get_job_ids ~webhook_secret) ::
      Routes.(s "login" /? nil @--> Current_github.Auth.login github_auth) ::
      Current_web.routes engine in
    let site = Current_web.Site.v ?authn ~has_role ~secure_cookies:true ~name:"opam-ci" routes in
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

let submission_service =
  Arg.required @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE"
    ["submission-service"]

let cmd =
  let doc = "Build OCaml projects on GitHub" in
  let info = Cmd.info "opam-repo-ci" ~doc ~envs:Conf.cmdliner_envs in
  Cmd.v info
    Term.(term_result (
      const main
      $ Current.Config.cmdliner
      $ Current_web.cmdliner
      $ Current_github.App.cmdliner
      $ Capnp_setup.cmdliner
      $ Current_github.Auth.cmdliner
      $ submission_service
      $ Prometheus_unix.opts
      $ Logs_cli.level ()))

let () = exit @@ Cmd.eval cmd
