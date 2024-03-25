val spec :
  for_docker:bool ->
  opam_version:[`V2_0 | `V2_1 | `Dev] ->
  base:string ->
  variant:Variant.t ->
  revdep:OpamPackage.t option ->
  lower_bounds:bool ->
  with_tests:bool ->
  OpamPackage.t ->
  Obuilder_spec.t

val revdeps :
  ?test_config:Integration_test.t option ->
  for_docker:bool ->
  opam_version:[`V2_0 | `V2_1 | `Dev] ->
  base:string ->
  variant:Variant.t ->
  OpamPackage.t ->
  Obuilder_spec.t
