val local_test :
  is_macos:bool ->
  ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  Current_github.Api.Repo.t -> unit -> unit Current.t
(** [local_test ~ocluster repo] is a pipeline that tests GitHub repository [repo] as the CI would. *)

val v :
  is_macos:bool ->
  ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  app:Current_github.App.t ->
  unit -> unit Current.t
(** The main opam-repo-ci pipeline. Tests everything configured for GitHub application [app]. *)
