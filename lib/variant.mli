type arch = Ocaml_version.arch

type t [@@deriving to_yojson]

val v : arch:arch -> distro:string -> compiler:(string * string option) -> t

val arch : t -> arch
val docker_tag : t -> string
val distribution : t -> string
val os : t -> [`linux | `macOS]

val pp : t Fmt.t

val macos_homebrew : string
