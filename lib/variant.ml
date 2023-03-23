module D = Obuilder_spec_opam.Distro

module Ocaml_version = struct
  include Ocaml_version
  (* Extend Ocaml_version with json support. *)

  let arch_to_yojson arch = `String (string_of_arch arch)

  let arch_of_yojson j =
    match j with
    | `String a -> (
        match arch_of_string a with
        | Ok v -> Ok v
        | Error _ -> Error ("unknown arch " ^ a))
    | _ -> Error "unknown json type for arch"

  let compare_arch = Stdlib.compare
  let equal_arch = ( = )
  let to_yojson t = `String (to_string t)

  let of_yojson j =
    match j with
    | `String a -> (
        match of_string a with
        | Ok v -> Ok v
        | Error _ -> Error ("unknown ocaml version " ^ a))
    | _ -> Error "unknown json for ocaml version"
end

let arch_to_yojson arch = `String (Ocaml_version.string_of_arch arch)

(* type t = {
  distribution : string;
  ocaml_version : string;
  ocaml_variant : string option;
  arch : arch;
} [@@deriving to_yojson] *)

type t = {
  distro : string;
  ocaml_version : Ocaml_version.t;
  arch : Ocaml_version.arch;
  opam_version : Opam_version.t;
}
[@@deriving yojson, ord, eq]

(* let pp_ocaml_version t =
  let variant = match t.ocaml_variant with
    | None -> ""
    | Some variant -> "-"^variant
  in
  t.ocaml_version^variant *)

let pp_ocaml_version t = Ocaml_version.to_string t.ocaml_version

let arch t = t.arch
let docker_tag t =
  t.distro ^ "-ocaml-" ^ pp_ocaml_version t
let distro_str t = t.distro
let distro t = D.distro_of_tag t.distro |> Option.get
let ocaml_version t = t.ocaml_version
let opam_version t = t.opam_version

let pp f t = Fmt.pf f "%s/%s" (docker_tag t) (Ocaml_version.string_of_arch t.arch)

let macos_homebrew = "macos-homebrew"

let macos_distributions = [
  macos_homebrew;
  (* TODO: Add macos-macports *)
]

(* TODO: Remove that when macOS uses ocaml-dockerfile *)
let os {distro; _} =
  if List.exists (String.equal distro) macos_distributions then
    `Macos
  else
    `Linux

(* let v ~arch ~distro ~compiler:(ocaml_version, ocaml_variant) =
  { arch; distribution = distro; ocaml_version; ocaml_variant } *)

let v ~arch ~distro ~ocaml_version ~opam_version =
  (* Just check we understand all the variants first *)
  match Ocaml_version.Configure_options.of_t ocaml_version with
  | Ok _ -> Ok { arch; distro; ocaml_version; opam_version }
  | Error e -> Error e



