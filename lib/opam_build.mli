val dockerfile :
  base:string ->
  variant:string ->
  revdep:string option ->
  with_tests:bool ->
  pkg:OpamPackage.t ->
  Obuilder_spec.stage
