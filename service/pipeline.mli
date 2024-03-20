(** The main opam-repo-ci pipeline. Tests everything configured for
    GitHub application [app] using the server cluster connected to
    via connection [ocluster]. *)
val v :
  ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  app:Current_github.App.t ->
  unit -> unit Current.t

(** [local_test repo branch] is a pipeline that tests branch [branch] on
    the local Git repository at path [repo] using local Docker containers. *)
val local_test_pr : ?test_config:Opam_repo_ci.Integration_test.t -> Current_git.Local.t -> string -> unit -> unit Current.t
