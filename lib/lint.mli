val fmt_dockerfile :
  base:string ->
  ocamlformat_source:Analyse_ocamlformat.source option ->
  variant:string ->
  Obuilder_spec.stage
(** A spec that checks the formatting. *)
