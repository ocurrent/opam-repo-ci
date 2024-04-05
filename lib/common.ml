open Capnp_rpc_lwt

module type S = sig
  type spec [@@deriving to_yojson]

  module Value : Current_cache.S.WITH_MARSHAL

  val pp_ty : Format.formatter -> (spec * Opam_package.t) -> unit
  val pp_summary : Format.formatter -> (spec * Opam_package.t) -> unit

  val build_spec :
    for_docker:bool -> base:string -> variant:Variant.t ->
    spec -> OpamPackage.t -> Obuilder_spec.t

  val parse_output :
    Current.Job.t -> Cluster_api.Job.X.t Capability.t option -> (Value.t, [`Msg of string]) Lwt_result.t
end

let parse_revdeps ~pkg output =
  String.split_on_char '\n' output
  |> List.fold_left (fun acc -> function
    | "" -> acc
    | revdep ->
        let revdep = OpamPackage.of_string revdep in
        if OpamPackage.equal pkg revdep then
          (* NOTE: opam list --recursive --depends-on <pkg>
             also returns <pkg> itself *)
          acc
        else
          OpamPackage.Set.add revdep acc
  ) OpamPackage.Set.empty

(* Don't include new packages that we're adding
   in the revdeps, as these are already tested *)
let filter_new_pkgs ~new_pkgs =
  OpamPackage.Set.filter (fun p -> not @@ List.mem p new_pkgs)

let revdeps ~pkg ~new_pkgs output =
  filter_new_pkgs ~new_pkgs @@ parse_revdeps ~pkg output
