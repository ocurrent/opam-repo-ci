val spec :
  for_docker:bool ->
  opam_version:[`V2_0 | `V2_1 | `Dev] ->
  base:string ->
  variant:Variant.t ->
  revdep:OpamPackage.t option ->
  lower_bounds:bool ->
  with_tests:bool ->
  pkg:OpamPackage.t ->
  Obuilder_spec.t

val revdeps :
  for_docker:bool ->
  base:string ->
  variant:Variant.t ->
  pkg:OpamPackage.t ->
  Obuilder_spec.t
