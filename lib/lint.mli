val fmt_dockerfile :
  base:string ->
  info:Analyse.Analysis.t ->
  variant:string ->
  Obuilder_spec.stage
(** A spec that checks the formatting. *)

val doc_dockerfile :
  base:string ->
  info:Analyse.Analysis.t ->
  variant:string ->
  Obuilder_spec.stage
(** A job that checks that the documentation in [./src/] builds without warnings. *)
