type t [@@deriving yojson, ord, eq]

(* val v : arch:arch -> distro:string -> compiler:(string * string option) -> t *)
val v :
  arch:Ocaml_version.arch ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  opam_version:Obuilder_spec_opam.Opam_version.t ->
  (t, [> `Msg of string ]) result

val pp_ocaml_version : t -> string

val arch : t -> Ocaml_version.arch
val docker_tag : t -> string
val distro_str : t -> string
val distro : t -> Obuilder_spec_opam.Distro.t
val ocaml_version : t -> Ocaml_version.t
val opam_version : t -> Obuilder_spec_opam.Opam_version.t
val os : t -> Obuilder_spec_opam.Distro.os_family

val pp : t Fmt.t

val macos_homebrew : string
