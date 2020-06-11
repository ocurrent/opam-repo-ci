let src = Logs.Src.create "opam_repo_ci_web" ~doc:"opam-repo-ci web interface"
include (val Logs.src_log src : Logs.LOG)
