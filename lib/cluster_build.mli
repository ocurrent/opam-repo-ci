type t

val config : timeout:int64 -> [ `Submission_f4e8a768b32a7c42 ] Capnp_rpc_lwt.Sturdy_ref.t -> t

val v :
  t ->
  label:string ->
  spec:Spec.t Current.t ->
  base:Spec.base Current.t ->
  master:Current_git.Commit.t Current.t ->
  urgent:([`High | `Low] -> bool) option Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t

val list_revdeps :
  t ->
  platform:Platform.t ->
  opam_version:[`V2_0 | `V2_1 | `Dev] ->
  pkgopt:PackageOpt.t Current.t ->
  base:Spec.base Current.t ->
  master:Current_git.Commit.t Current.t ->
  after:unit Current.t ->
  Current_git.Commit_id.t Current.t ->
  OpamPackage.Set.t Current.t
