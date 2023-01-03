type arch = Ocaml_version.arch

let arch_to_yojson arch = `String (Ocaml_version.string_of_arch arch)

type t = {
  distribution : string;
  ocaml_version : string;
  ocaml_variant : string option;
  arch : arch;
} [@@deriving to_yojson]

let pp_ocaml_version t =
  let variant = match t.ocaml_variant with
    | None -> ""
    | Some variant -> "-"^variant
  in
  t.ocaml_version^variant

let arch t = t.arch
let docker_tag t =
  t.distribution^"-ocaml-"^pp_ocaml_version t
let distribution t = t.distribution

let packages t =
  let version = Ocaml_version.with_patch (Ocaml_version.of_string_exn t.ocaml_version) (Some 0) in
  let comp =
    let name, version = Ocaml_version.Opam.V2.package version in
    name^"."^version
  in
  match Ocaml_version.Opam.V2.additional_packages version with
  | [] -> comp
  | extras -> comp^","^String.concat "," extras

let pp f t = Fmt.pf f "%s/%s" (docker_tag t) (Ocaml_version.string_of_arch t.arch)

let macos_homebrew = "macos-homebrew"

let macos_distributions = [
  macos_homebrew;
  (* TODO: Add macos-macports *)
]

(* TODO: Remove that when macOS uses ocaml-dockerfile *)
let os {distribution; _} =
  if List.exists (String.equal distribution) macos_distributions then
    `macOS
  else
    `linux

let v ~arch ~distro ~compiler:(ocaml_version, ocaml_variant) =
  { arch; distribution = distro; ocaml_version; ocaml_variant }
