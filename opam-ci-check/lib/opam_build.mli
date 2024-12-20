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
  ?ci_check_ref:string ->
  for_docker:bool ->
  opam_version:Opam_version.t ->
  base:string ->
  variant:Variant.t ->
  pkg:OpamPackage.t ->
  unit ->
  Obuilder_spec.t
(** @param ci_check_ref a git ref for the opam-ci-check package to pin when listing revdeps *)

val build_spec :
  ?local:bool ->
  ?ci_check_ref:string ->
  for_docker:bool ->
  base:Spec.base  ->
  Spec.t ->
  Obuilder_spec.t
(** @param ci_check_ref a git ref for the opam-ci-check package to pin when listing revdeps *)
