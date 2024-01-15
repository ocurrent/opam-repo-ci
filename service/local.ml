(* Utility program for testing the CI pipeline locally. *)

let () =
  Memtrace.trace_if_requested ~context:"opam-repo-ci-local" ();
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Prometheus_unix.Logging.init ()

let main config mode is_macos repo branch =
  Lwt_main.run begin
    let repo = Current_git.Local.v (Result.get_ok @@ Fpath.of_string repo) in
    let engine = Current.Engine.create ~config (Pipeline.local_test_pr ~is_macos repo branch) in
    let routes = Current_web.routes engine in
    let site = Current_web.Site.(v ~has_role:allow_all) ~name:"opam-repo-ci-local" routes in
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

(* https://github.com/ocurrent/opam-repo-ci/issues/260 *)
let is_macos =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Tells the service that the host is running MacOS. opam-repo-ci may have errors otherwise."
    ~docv:"MACOS"
    ["macos"]

let cmd =
  let doc = "Test opam-repo-ci on a local Git repository" in
  let info = Cmd.info "opam-repo-ci-local" ~doc in
  Cmd.v info
    Term.(term_result (
      const main
      $ Current.Config.cmdliner
      $ Current_web.cmdliner
      $ is_macos
      $ repo
      $ branch))

let () = exit @@ Cmd.eval cmd
