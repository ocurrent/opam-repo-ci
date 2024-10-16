type arch = Ocaml_version.arch

let arch_to_yojson arch = `String (Ocaml_version.string_of_arch arch)

type t = {
  distribution : string;
  ocaml_version : string;
  ocaml_variant : string option;
  arch : arch;
} [@@deriving to_yojson]

let ocaml_version_to_string t =
  let variant = match t.ocaml_variant with
    | None -> ""
    | Some variant -> "-" ^ variant
  in
  t.ocaml_version ^ variant

let arch t = t.arch

let docker_tag t =
  t.distribution ^ "-ocaml-" ^ ocaml_version_to_string t
  |> String.map (function
    | '+' | '~' -> '-'
    | x -> x)

let distribution t = t.distribution
let pp f t = Fmt.pf f "%s/%s" (docker_tag t) (Ocaml_version.string_of_arch t.arch)

let freebsd = "freebsd"
let macos_homebrew = "macos-homebrew"

let macos_distributions = [
  macos_homebrew;
  (* TODO: Add macos-macports *)
]

(* TODO: Remove that when MacOS uses ocaml-dockerfile *)
let os { distribution; _ } =
  if List.exists (String.equal distribution) macos_distributions then
    `Macos
  else if List.exists (String.equal distribution) [ "freebsd" ] then
    `Freebsd
  else
    `Linux

let v ~arch ~distro ~compiler:(ocaml_version, ocaml_variant) =
  { arch; distribution = distro; ocaml_version; ocaml_variant }
