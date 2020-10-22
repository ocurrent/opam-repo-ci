(* Utility program for testing the CI pipeline on a local repository. *)

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Logging.init ()

let main config mode submission_uri repo =
  Logging.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let ocluster = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
    let repo = Current_git.Local.v (Fpath.v repo) in
    let engine = Current.Engine.create ~config (Pipeline.local_test ~ocluster repo) in
    let routes = Current_web.routes engine in
    let site = Current_web.Site.(v ~has_role:allow_all) ~name:"opam-repo-ci-local" routes in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let submission_service =
  Arg.required @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE"
    ["submission-service"]

let cmd =
  let doc = "Test opam-repo-ci on a local Git clone" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ submission_service $ repo),
  Term.info "opam-repo-ci-local" ~doc

let () = Term.(exit @@ eval cmd)
