type arch = Ocaml_version.arch

let arch_to_yojson arch = `String (Ocaml_version.string_of_arch arch)

type t = {
  distribution : string;
  ocaml_version : string;
  ocaml_variant : string option;
  arch : arch;
} [@@deriving to_yojson]

let arch t = t.arch
let docker_tag t =
  let variant = match t.ocaml_variant with
    | None -> ""
    | Some variant -> "-"^variant
  in
  t.distribution^"-ocaml-"^t.ocaml_version^variant
let distribution t = t.distribution

let pp f t = Fmt.pf f "%s/%s" (docker_tag t) (Ocaml_version.string_of_arch t.arch)

(* TODO: do better *)
let is_macos = function
  | {distribution = "macos-homebrew"; _} -> true
  | _ -> false

let v ~arch ~distro ~compiler:(ocaml_version, ocaml_variant) =
  { arch; distribution = distro; ocaml_version; ocaml_variant }
