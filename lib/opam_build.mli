val dockerfile :
  base:string ->
  variant:string ->
  revdep:string option ->
  with_tests:bool ->
  pkg:OpamPackage.t ->
  for_user:bool ->
  Dockerfile.t
