open Capnp_rpc_lwt

type config = Capnp_rpc_unix.Network.Location.t option
(** The address to advertise, if a Cap'n Proto endpoint is configured. *)

val run :
  ?listen_address:Capnp_rpc_unix.Network.Location.t ->
  secret_key:string ->
  cap_file:string ->
  config ->
  (Capnp_rpc_unix.Vat.t * Opam_repo_ci_api.Raw.Service.CI.t Capability.resolver option) Lwt.t
(** [run (Some public_address)] runs a Cap'n Proto service advertising its
    address as [public_address]. It returns the vat and a resolver for the
    promise of an engine that it exposes (because you can't create the engine
    until you have the vat). [run None] just creates a client-only vat (and no
    resolver).
    @param listen_address Address to listen for incoming connections. Defaults to [public_address]. *)

val cmdliner : config Cmdliner.Term.t
