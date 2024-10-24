open Opam_repo_ci

module Git = Current_git

let yesno_of_bool b = if b then "Yes" else "No"

let output_platform Build.{ variant; opam_version; lower_bounds; revdeps; label = _ } =
  Format.asprintf
    "| %s | %s | %s | %s | %s | %s |"
    (Variant.distribution variant)
    (Ocaml_version.string_of_arch @@ Variant.arch variant)
    (Variant.ocaml_version_to_string variant)
    (Opam_version.to_string opam_version)
    (yesno_of_bool lower_bounds)
    (yesno_of_bool revdeps)

let compare_platform (p : Build.build_recipe) (p' : Build.build_recipe) =
  let module V = Variant in
  let cmp = String.compare in
  match
    cmp
      (V.distribution p.variant)
      (V.distribution p'.variant)
  with
  | os when os <> 0 -> os
  | _ ->
  match
    cmp
      (Ocaml_version.string_of_arch (V.arch p.variant))
      (Ocaml_version.string_of_arch (V.arch p'.variant))
  with
  | arch when arch <> 0 -> arch
  | _ ->
  cmp (V.ocaml_version_to_string p.variant) (V.ocaml_version_to_string p'.variant)

let platforms : Build.build_recipe list =
  let arch = `X86_64 in
  List.sort_uniq compare_platform @@
  (Build.compilers ~arch ()) @
  (Build.linux_distributions ~arch ) @
  (Build.macos ()) @
  (Build.freebsd ()) @
  (Build.extras ())

let operating_systems =
  platforms
  |> List.map (fun p -> Variant.distribution p.Build.variant)
  |> List.sort_uniq String.compare

let architectures =
  platforms
  |> List.map (fun p -> Ocaml_version.string_of_arch (Variant.arch p.Build.variant))
  |> List.sort_uniq String.compare

let ocaml_versions =
  platforms
  |> List.map (fun p -> Variant.ocaml_version_to_string p.Build.variant)
  |> List.sort_uniq String.compare

let print_items oc items =
    List.iter (Printf.fprintf oc "- %s\n") items;
    Printf.fprintf oc "\n"

let main outfile =
  let oc = open_out outfile in
  Printf.fprintf oc "# Platforms Tested on the Opam Repository CI\n\n";
  Printf.fprintf oc "## Operating Systems\n\n";
  print_items oc operating_systems;
  Printf.fprintf oc "## Architectures\n\n";
  print_items oc architectures;
  Printf.fprintf oc "## OCaml Versions\n\n";
  print_items oc ocaml_versions;
  Printf.fprintf oc "## Platforms Matrix\n\n";
  Printf.fprintf oc "|  OS | Arch | OCaml version |Opam version | Test lower-bounds | Test reverse dependencies |\n";
  Printf.fprintf oc "| --- | --- | --- | --- | --- | --- |\n";
  platforms
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
