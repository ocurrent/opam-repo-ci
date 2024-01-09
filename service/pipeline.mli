val local_test : is_macos:bool -> Current_git.Local.t -> string -> unit -> unit Current.t
(** [local_test repo branch] is a pipeline that tests
    branch [branch] on the local Git repository at path [repo]. *)

val v :
  is_macos:bool ->
  ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  app:Current_github.App.t ->
  unit -> unit Current.t
(** The main opam-repo-ci pipeline. Tests everything configured for GitHub application [app]. *)
