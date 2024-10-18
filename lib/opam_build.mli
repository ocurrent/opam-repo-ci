val spec :
  ?local:bool ->
  for_docker:bool ->
  opam_version:Opam_version.t ->
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
  opam_version:Opam_version.t ->
  base:string ->
  variant:Variant.t ->
  pkg:OpamPackage.t ->
  unit ->
  Obuilder_spec.t

val build_spec :
  ?local:bool ->
  for_docker:bool ->
  base:Spec.base  ->
  Spec.t ->
  Obuilder_spec.t
