val spec :
  base:string ->
  variant:string ->
  revdep:OpamPackage.t option ->
  compiler:Platform.compiler ->
  with_tests:bool ->
  pkg:OpamPackage.t ->
  Obuilder_spec.stage

val revdeps :
  with_tests:bool ->
  base:string ->
  variant:string ->
  compiler:Platform.compiler ->
  pkg:OpamPackage.t ->
  Obuilder_spec.stage
