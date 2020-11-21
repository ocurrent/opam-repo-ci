type compiler_option = [
  | `Default
  | `Flambda
  | `System
] [@@deriving to_yojson]

let ocaml_version_to_yojson v =
  `String (Ocaml_version.to_string v)

type compiler = {
  compiler_version : Ocaml_version.t [@to_yojson ocaml_version_to_yojson];
  compiler_full_version : Ocaml_version.t [@to_yojson ocaml_version_to_yojson];
  compiler_option : compiler_option;
} [@@deriving to_yojson]

type t = {
  label : string;
  pool : string;        (* OCluster pool *)
  variant : string;
}

let pp f t = Fmt.string f t.label
let compare a b = compare a.label b.label
