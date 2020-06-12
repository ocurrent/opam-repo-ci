(* Utility program for testing the CI pipeline on a local repository. *)

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Logging.init ()

let main config mode repo =
  let repo = Current_git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (Pipeline.local_test repo) in
  let routes = Current_web.routes engine in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:"opam-repo-ci-local" routes in
  Logging.run begin
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

let cmd =
  let doc = "Test opam-repo-ci on a local Git clone" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo),
  Term.info "opam-repo-ci-local" ~doc

let () = Term.(exit @@ eval cmd)
