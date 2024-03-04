(** A variant describes the platform on which a job is run, including
    information such as the OS distribution, architecture,
    and version of OCaml *)

type arch = Ocaml_version.arch

type t [@@deriving to_yojson]

val v : arch:arch -> distro:string -> compiler:(string * string option) -> t

val ocaml_version_to_string : t -> string

val arch : t -> arch
val docker_tag : t -> string
val distribution : t -> string
val os : t -> [ `Linux | `Macos | `Freebsd ]

val pp : t Fmt.t

(** The Homebrew MacOS distribution *)
val macos_homebrew : string

(** The FreeBSD distribution *)
val freebsd : string
