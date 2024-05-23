open Opam_repo_ci

module Git = Current_git

type platform = {
  variant : Variant.t;
  opam_version : [ `Dev | `V2_0 | `V2_1];
  lower_bounds : bool;
  revdeps : bool;
}

let opam_version_to_string = function
  | `Dev -> "dev"
  | `V2_1 -> "2.1"
  | `V2_0 -> "2.0"

let yesno_of_bool b = if b then "Yes" else "No"

let output_platform { variant; opam_version; lower_bounds; revdeps } =
  Format.asprintf
    "| %s | %s | %s | %s | %s | %s |"
    (Variant.ocaml_version_to_string variant)
    (Variant.distribution variant)
    (Ocaml_version.string_of_arch @@ Variant.arch variant)
    (opam_version_to_string opam_version)
    (yesno_of_bool lower_bounds)
    (yesno_of_bool revdeps)

let platforms () =
  let arch = `X86_64 in
  let build ~opam_version ~lower_bounds ~revdeps _ variant =
    [ { variant; opam_version; lower_bounds; revdeps; } ]
  in
  List.concat @@
    (Build.compilers ~arch ~build ()) @
    (Build.linux_distributions ~arch ~build) @
    (Build.macos ~build) @
    (Build.freebsd ~build) @
    (Build.extras ~build)

let main outfile =
  let oc = open_out outfile in
  Printf.fprintf oc "## Tested platforms\n\n";
  Printf.fprintf oc "| OCaml version | OS | Arch | Opam version | Test lower-bounds | Test reverse dependencies |\n";
  Printf.fprintf oc "| --- | --- | --- | --- | --- | --- |\n";
  platforms ()
  |> List.map output_platform
  |> List.iter (Printf.fprintf oc "%s\n");
  close_out oc

(* Command-line parsing *)
open Cmdliner

let outfile =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"The destination file for the list of platforms."
    ~docv:"OUTFILE"
    ["out"; "o"]

let cmd =
  let doc = "Generate documentation for the list of platforms" in
  let info = Cmd.info "doc" ~doc in
  let cmd_t = Term.(const main $ outfile) in
  Cmd.v info cmd_t

let () = exit (Cmd.eval cmd)
