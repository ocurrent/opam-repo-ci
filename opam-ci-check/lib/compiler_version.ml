(** Data describing and manipulating supported compilers versions *)

let all_supported = Ocaml_version.Releases.significant @ Ocaml_version.Releases.unreleased_betas
