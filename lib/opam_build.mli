val spec :
  ?local:bool ->
  for_docker:bool ->
  opam_version:[`V2_0 | `V2_1 | `V2_2 | `Dev] ->
  base:string ->
  variant:Variant.t ->
  revdep:OpamPackage.t option ->
  lower_bounds:bool ->
  with_tests:bool ->
  pkg:OpamPackage.t ->
  unit ->
  Obuilder_spec.t

val revdeps :
  ?local:bool ->
  for_docker:bool ->
  opam_version:[`V2_0 | `V2_1 | `V2_2 | `Dev] ->
  base:string ->
  variant:Variant.t ->
  pkg:OpamPackage.t ->
  unit ->
  Obuilder_spec.t
