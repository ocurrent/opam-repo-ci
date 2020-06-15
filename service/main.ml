open Lwt.Infix

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Logging.init ();
  Mirage_crypto_rng_unix.initialize ();
  match Conf.profile with
  | `Production -> Logs.info (fun f -> f "Using production configuration")
  | `Dev -> Logs.info (fun f -> f "Using dev configuration")

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let run_capnp ~engine = function
  | None -> Lwt.return_unit
  | Some public_address ->
    let config =
      Capnp_rpc_unix.Vat_config.create
        ~public_address
        ~secret_key:(`File Conf.Capnp.secret_key)
        (Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port)
    in
    let service_id = Capnp_rpc_unix.Vat_config.derived_id config "ci" in
    let restore = Capnp_rpc_net.Restorer.single service_id (Api_impl.make_ci ~engine) in
    Capnp_rpc_unix.serve config ~restore >>= fun vat ->
    Capnp_rpc_unix.Cap_file.save_service vat service_id Conf.Capnp.cap_file |> or_die;
    Logs.app (fun f -> f "Wrote capability reference to %S" Conf.Capnp.cap_file);
    Lwt.return_unit

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

let main config mode github repo capnp_address github_auth =
  let repo = (github, repo) in
  let engine = Current.Engine.create ~config (Pipeline.v ~repo) in
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
  Logging.run begin
    run_capnp ~engine capnp_address >>= fun () ->
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let capnp_address =
  Arg.value @@
  Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None @@
  Arg.info
    ~doc:"Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no RPC)"
    ~docv:"ADDR"
    ["capnp-address"]

let repo =
  Arg.value @@
  Arg.opt Current_github.Repo_id.cmdliner { Current_github.Repo_id.owner = "ocaml"; name = "opam-repository" } @@
  Arg.info
    ~doc:"The repository to monitor (owner/name)"
    ~docv:"REPO"
    ["repository"]

let cmd =
  let doc = "Build OCaml projects on GitHub" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $
        Current_github.Api.cmdliner $ repo $ capnp_address $ Current_github.Auth.cmdliner),
  Term.info "opam-repo-ci" ~doc

let () = Term.(exit @@ eval cmd)
