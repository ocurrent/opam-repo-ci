type arch = Ocaml_version.arch

let arch_to_yojson arch = `String (Ocaml_version.string_of_arch arch)

type t = {
  docker_tag : string;
  arch : arch;
} [@@deriving to_yojson]

let arch t = t.arch
let docker_tag t = t.docker_tag

let pp f { docker_tag; arch } = Fmt.pf f "%s/%s" docker_tag (Ocaml_version.string_of_arch arch)

let v ~arch docker_tag = { arch; docker_tag }
