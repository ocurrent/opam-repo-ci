val spec :
  upgrade_opam:bool ->
  base:string ->
  variant:Variant.t ->
  revdep:OpamPackage.t option ->
  with_tests:bool ->
  pkg:OpamPackage.t ->
  Obuilder_spec.stage

val revdeps :
  base:string ->
  variant:Variant.t ->
  pkg:OpamPackage.t ->
  Obuilder_spec.stage
