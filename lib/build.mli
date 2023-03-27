open Obuilder_spec_opam

module Spec : sig
  type t

  val opam :
    ?revdep:OpamPackage.t ->
    platform:Platform.t ->
    lower_bounds:bool ->
    with_tests:bool ->
    opam_version:Opam_version.t ->
    OpamPackage.t ->
    t
end

type base =
  | Docker of Current_docker.Raw.Image.t
  | MacOS of string

type t

val config : timeout:int64 -> [ `Submission_f4e8a768b32a7c42 ] Capnp_rpc_lwt.Sturdy_ref.t -> t

val v :
  t ->
  label:string ->
  spec:Spec.t Current.t ->
  base:base Current.t ->
  master:Current_git.Commit.t Current.t ->
  urgent:([`High | `Low] -> bool) option Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t

val list_revdeps :
  t ->
  platform:Platform.t ->
  opam_version:Opam_version.t ->
  pkgopt:PackageOpt.t Current.t ->
  base:base Current.t ->
  master:Current_git.Commit.t Current.t ->
  after:unit Current.t ->
  Current_git.Commit_id.t Current.t ->
  OpamPackage.Set.t Current.t
