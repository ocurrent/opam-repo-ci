open Capnp_rpc_lwt
open Lwt.Infix

let () =
  Memtrace.trace_if_requested ~context:"opam-repo-ci" ();
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Logging.init ();
  Mirage_crypto_rng_unix.initialize ();
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
           ) -> true
    | _ -> false

let main config mode app capnp_address github_auth submission_uri =
  Logging.run begin
    let listen_address = Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port in
    Capnp_setup.run ~listen_address capnp_address >>= fun (vat, rpc_engine_resolver) ->
    let ocluster = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
    let engine = Current.Engine.create ~config (Pipeline.v ~ocluster ~app) in
    rpc_engine_resolver |> Option.iter (fun r -> Capability.resolve_ok r (Api_impl.make_ci ~engine));
    let authn = Option.map Current_github.Auth.make_login_uri github_auth in
    let has_role =
      if github_auth = None then Current_web.Site.allow_all
      else has_role
    in
    let routes =
      Routes.(s "webhooks" / s "github" /? nil @--> Current_github.webhook) ::
      Routes.(s "login" /? nil @--> Current_github.Auth.login github_auth) ::
      Current_web.routes engine in
    let site = Current_web.Site.v ?authn ~has_role ~secure_cookies:true ~name:"opam-ci" routes in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
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
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $
        Current_github.App.cmdliner $ Capnp_setup.cmdliner $ Current_github.Auth.cmdliner $ submission_service),
  Term.info "opam-repo-ci" ~doc

let () = Term.(exit @@ eval cmd)
