(* Utility program for testing the CI pipeline locally. *)

open Capnp_rpc_lwt
open Lwt.Infix

module Github = Current_github

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Logging.init ()

let main config mode capnp_address submission_uri api repo_id =
  Logging.run begin
    Capnp_setup.run capnp_address >>= fun (vat, rpc_engine_resolver) ->
    let repo = (api, repo_id) in
    let ocluster = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
    let engine = Current.Engine.create ~config (Pipeline.local_test ~ocluster repo) in
    rpc_engine_resolver |> Option.iter (fun r -> Capability.resolve_ok r (Api_impl.make_ci ~engine));
    let routes = Current_web.routes engine in
    let site = Current_web.Site.(v ~has_role:allow_all) ~name:"opam-repo-ci-local" routes in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

let opam_repository = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" }

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
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Capnp_setup.cmdliner $ submission_service $ Current_github.Api.cmdliner $ repo),
  Term.info "opam-repo-ci-local" ~doc

let () = Term.(exit @@ eval cmd)
