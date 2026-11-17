open Capnp_rpc_lwt
open Lwt.Infix

type config = Capnp_rpc_unix.Network.Location.t option

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let run ?listen_address ~secret_key ~cap_file = function
  | None -> Lwt.return (Capnp_rpc_unix.client_only_vat (), None)
  | Some public_address ->
    let config =
      Capnp_rpc_unix.Vat_config.create
        ~public_address
        ~secret_key:(`File secret_key)
        (Stdlib.Option.value listen_address ~default:public_address)
    in
    let rpc_engine, rpc_engine_resolver = Capability.promise () in
    let service_id = Capnp_rpc_unix.Vat_config.derived_id config "ci" in
    let restore = Capnp_rpc_net.Restorer.single service_id rpc_engine in
    Capnp_rpc_unix.serve config ~restore >>= fun vat ->
    Capnp_rpc_unix.Cap_file.save_service vat service_id cap_file |> or_die;
    Logs.app (fun f -> f "Wrote capability reference to %S" cap_file);
    Lwt.return (vat, Some rpc_engine_resolver)

open Cmdliner

let public_address =
  Arg.value @@
  Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None @@
  Arg.info
    ~doc:"Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no RPC)"
    ~docv:"ADDR"
    ["capnp-address"]

let cmdliner = public_address
