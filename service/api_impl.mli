(** Create an API that allows the web frontend to access information
    about the CI through capnp. *)
val make_ci :
  engine:Current.Engine.t ->
  Opam_repo_ci_api.Raw.Service.CI.t Capnp_rpc_lwt.Capability.t
