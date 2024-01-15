(* TODO: Make macOS use docker images *)
type base =
  | Docker of Current_docker.Raw.Image.t
  | MacOS of string
  | FreeBSD of string

let base_to_string = function
  | Docker img -> Current_docker.Raw.Image.hash img
  | MacOS base -> base
  | FreeBSD base -> base

type package = OpamPackage.t

let package_to_yojson x = `String (OpamPackage.to_string x)

type opam_build = {
  revdep : package option;
  with_tests : bool;
  lower_bounds : bool;
  opam_version : [`V2_0 | `V2_1 | `Dev];
} [@@deriving to_yojson]

type list_revdeps = {
  opam_version : [`V2_0 | `V2_1 | `Dev];
} [@@deriving to_yojson]

type ty = [
  | `Opam of [ `Build of opam_build | `List_revdeps of list_revdeps ] * package
] [@@deriving to_yojson]

type t = {
  platform : Platform.t;
  ty : ty;
}

let opam ?revdep ~platform ~lower_bounds ~with_tests ~opam_version pkg =
  let ty = `Opam (`Build { revdep; lower_bounds; with_tests; opam_version }, pkg) in
  { platform; ty }

let pp_pkg ?revdep f pkg =
  match revdep with
  | Some revdep -> Fmt.pf f "%s with %s" (OpamPackage.to_string revdep) (OpamPackage.to_string pkg)
  | None -> Fmt.string f (OpamPackage.to_string pkg)

let pp_opam_version = function
  | `V2_0 -> "2.0"
  | `V2_1 -> "2.1"
  | `Dev -> "dev"

let pp_ty f = function
  | `Opam (`List_revdeps {opam_version}, pkg) ->
      Fmt.pf f "list revdeps of %s, using opam %s" (OpamPackage.to_string pkg)
        (pp_opam_version opam_version)
  | `Opam (`Build { revdep; lower_bounds; with_tests; opam_version }, pkg) ->
    let action = if with_tests then "test" else "build" in
    Fmt.pf f "%s %a%s, using opam %s" action (pp_pkg ?revdep) pkg
      (if lower_bounds then ", lower-bounds" else "")
      (pp_opam_version opam_version)

let pp_summary f = function
  | `Opam (`List_revdeps _, _) -> Fmt.string f "Opam list revdeps"
  | `Opam (`Build _, _) -> Fmt.string f "Opam project build"
