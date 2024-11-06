(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

open Cmdliner
open Opam_ci_check
module Distro = Dockerfile_opam.Distro

(* This is Cmdliner.Term.map, which is not available in Cmdliner 1.1.1 *)
let map_term f x = Term.app (Term.const f) x

let to_exit_code : (unit, string) result Term.t -> Cmd.Exit.code Term.t =
  map_term @@ function
  | Ok () -> 0
  | Error msg ->
      Printf.eprintf "%s%!" msg;
      1

let lint (changed_pkgs, new_pkgs) local_repo_dir =
  match local_repo_dir with
  | None -> failwith "TODO: default to using the opam repository"
  | Some d -> (
      print_endline @@ Printf.sprintf "Linting opam-repository at %s ..." d;
      match Lint.check ~new_pkgs ~changed_pkgs d with
      | [] ->
          print_endline "No errors";
          Ok ()
      | errors ->
          errors |> List.map Lint.msg_of_error |> String.concat "\n"
          |> Printf.sprintf "%s\n" |> Result.error)

let show_revdeps pkg local_repo_dir no_transitive_revdeps =
  (* Get revdeps for the package *)
  let revdeps =
    Revdeps.list_revdeps ?opam_repo:local_repo_dir
      ~transitive:(not no_transitive_revdeps)
      pkg
  in
  Revdeps.Display.packages revdeps;
  Ok ()

let testing_revdeps_confirmed revdeps =
  OpamConsole.confirm "Do you want test %d revdeps?" (List.length revdeps)

let test_revdeps pkg local_repo_dir use_dune no_transitive_revdeps =
  (* Get revdeps for the package *)
  let revdeps =
    Revdeps.list_revdeps ?opam_repo:local_repo_dir
      ~transitive:(not no_transitive_revdeps)
      pkg
  in

  (* Install and test the first reverse dependency *)
  let latest_versions = Revdeps.find_latest_versions revdeps in

  Revdeps.Display.packages latest_versions;
  if not (testing_revdeps_confirmed latest_versions) then Ok ()
  else
    match (use_dune, local_repo_dir) with
    | true, Some d -> Test.test_packages_with_dune d pkg latest_versions
    | true, None -> Error "Opam local repository path must be specified!\n"
    | false, _ ->
        let num_failed_installs =
          Test.test_packages_with_opam pkg latest_versions
          |> Seq.map (fun e -> Printf.eprintf "%s\n" (Test.error_to_string e))
          |> Seq.length
        in
        if num_failed_installs = 0 then Ok ()
        else
          Error
            (Printf.sprintf "tests failed in %d reverse dependencies"
               num_failed_installs)

let default_compiler () =
  let compiler =
    Ocaml_version.(to_string (with_just_major_and_minor Releases.latest))
  in
  (compiler, None)

let default_distro () =
  (Distro.resolve_alias Distro.master_distro :> Distro.t)
  |> Distro.tag_of_distro

let parse_distro_compiler_hash base =
  let img, hash =
    match Astring.String.cut ~sep:"@" base with
    | Some (s, hash) -> (s, Some hash)
    | None -> (base, None)
  in
  let _docker_img, tag =
    match Astring.String.cut ~sep:":" img with
    | Some (docker_img, tag) -> (docker_img, tag)
    | None -> ("ocaml/opam", img)
  in
  let distro, ocaml =
    match Astring.String.cut ~sep:"-ocaml-" tag with
    | Some (distro, ocaml) -> (Some distro, Some ocaml)
    | None ->
        if
          String.starts_with ~prefix:"ocaml-" tag
          || Str.string_match (Str.regexp "[0-9]") tag 0
        then (None, Some tag)
        else (Some tag, None)
  in
  let compiler =
    let ( let* ) = Stdlib.Option.bind in
    let* ocaml = ocaml in
    let version_variant =
      ocaml
      |> Str.replace_first (Str.regexp "ocaml-") ""
      |> Str.replace_first (Str.regexp {|-\(beta\|alpha\)|}) "~\\1"
    in
    let version, variant =
      match Astring.String.cut ~sep:"-" version_variant with
      | Some (version, variant) -> (version, Some variant)
      | None -> (version_variant, None)
    in
    Some (version, variant)
  in
  (distro, compiler, hash)

let config_from_base_image ~base_image ~with_tests ~lower_bounds pkg =
  let distro, compiler, hash =
    match base_image with
    | Some base_image -> parse_distro_compiler_hash base_image
    | None -> (None, None, None)
  in
  let opam_version = `Dev in
  let distro = Stdlib.Option.value distro ~default:(default_distro ()) in
  let compiler = Stdlib.Option.value compiler ~default:(default_compiler ()) in
  (* TODO: Support other archs *)
  let variant = Variant.v ~distro ~compiler ~arch:`X86_64 in
  let base =
    Spec.Docker
      (if Option.is_some hash then Option.get base_image
       else "ocaml/opam:" ^ Variant.docker_tag variant)
  in
  let config = Spec.opam ~variant ~lower_bounds ~with_tests ~opam_version pkg in
  (base, config)

let build_run_spec no_cache base_image with_tests lower_bounds pkg
    opam_repository =
  let pkg = OpamPackage.of_string pkg in
  let base, config =
    config_from_base_image ~base_image ~with_tests ~lower_bounds pkg
  in
  Test.build_run_spec ~use_cache:(not no_cache) ?opam_repository ~base config
  |> Result.map_error (fun _ -> "Failed to build and test the package")

let make_abs_path s =
  if Filename.is_relative s then Filename.concat (Sys.getcwd ()) s else s

let opam_repo_dir =
  let parse s =
    if Sys.file_exists s then
      let repo_file = Filename.concat s "repo" in
      let packages_dir = Filename.concat s "packages" in
      if Sys.file_exists repo_file && Sys.is_directory packages_dir then
        Ok (Some (make_abs_path s))
      else
        Error
          (`Msg
            "The specified directory does not look like an opam repository. It \
             doesn't contain required 'repo' file or 'packages' directory.")
    else Error (`Msg "The specified directory does not exist.")
  in
  let print fmt = function Some s -> Format.fprintf fmt "%s" s | None -> () in
  Arg.conv (parse, print)

let local_opam_repo_term =
  let info =
    Arg.info [ "r"; "opam-repository" ]
      ~doc:
        "Path to local clone of Opam Repository. This is optional and only \
         required if we wish to test a version of a package not released on \
         the opam repository."
  in
  Arg.value (Arg.opt opam_repo_dir None info)

let no_transitive_revdeps =
  let info =
    Arg.info [ "no-transitive" ]
      ~doc:
        "Don't test transitive reverse dependencies - only test the direct \
         reverse dependencies."
  in
  Arg.value (Arg.flag info)

let use_dune_term =
  let info =
    Arg.info [ "d"; "use-dune" ]
      ~doc:"Use dune to build, install and test the reverse dependencies."
  in
  Arg.value (Arg.flag info)

let pkg_term =
  let info = Arg.info [] ~doc:"Package name + version" in
  Arg.required (Arg.pos 0 (Arg.some Arg.string) None info)

let changed_pkgs_term =
  let info =
    Arg.info
      [ "c"; "changed-packages" ]
      ~doc:"List of changed package name + version"
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [] info)

let newly_published_pkgs_term =
  let info =
    Arg.info [ "n"; "newly-published" ]
      ~doc:"List of newly published package name + version"
  in
  Arg.value (Arg.opt (Arg.list Arg.string) [] info)

let packages_term =
  let create_term changed_pkgs newly_published_pkgs =
    if changed_pkgs = [] && newly_published_pkgs = [] then
      `Error
        ( false,
          "You must provide at least one changed or newly published package." )
    else `Ok (changed_pkgs, newly_published_pkgs)
  in
  Term.(ret (const create_term $ changed_pkgs_term $ newly_published_pkgs_term))

let lint_cmd =
  let doc = "Lint the opam repository directory" in
  let term =
    Term.(const lint $ packages_term $ local_opam_repo_term) |> to_exit_code
  in
  let info =
    Cmd.info "lint" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let list_cmd =
  let doc = "List the revdeps for a package" in
  let term =
    Term.(
      const show_revdeps $ pkg_term $ local_opam_repo_term
      $ no_transitive_revdeps)
    |> to_exit_code
  in
  let info =
    Cmd.info "list" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let test_cmd =
  let doc = "Test the revdeps for a package" in
  let term =
    Term.(
      const test_revdeps $ pkg_term $ local_opam_repo_term $ use_dune_term
      $ no_transitive_revdeps)
    |> to_exit_code
  in
  let info =
    Cmd.info "test" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let no_cache =
  let info = Arg.info [ "no-cache" ] ~doc:"Don't use the docker cache" in
  Arg.value (Arg.flag info)

let with_test =
  let info =
    Arg.info [ "with-test" ]
      ~doc:"Build image which also runs tests for the package"
  in
  Arg.value (Arg.flag info)

let lower_bounds =
  let info =
    Arg.info [ "lower-bounds" ] ~doc:"Build image using lower bounds packages"
  in
  Arg.value (Arg.flag info)

let base_image =
  let info =
    Arg.info [ "base-image" ]
      ~doc:
        "Base image to use for the build. Defaults to ocaml/opam on Debian \
         stable with the latest OCaml release."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let build_cmd =
  let doc =
    "Build a package optionally with tests and/or using lower bounds packages"
  in
  let term =
    Term.(
      const build_run_spec $ no_cache $ base_image $ with_test $ lower_bounds
      $ pkg_term $ local_opam_repo_term)
    |> to_exit_code
  in
  let info =
    Cmd.info "build" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let cmd : Cmd.Exit.code Cmd.t =
  let doc = "A tool to list revdeps and test the revdeps locally" in
  let exits = Cmd.Exit.defaults in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Cmd.info "opam-ci-check" ~doc ~sdocs:"COMMON OPTIONS" ~exits in
  Cmd.group ~default info [ lint_cmd; list_cmd; test_cmd; build_cmd ]

let () = exit (Cmd.eval' cmd)
