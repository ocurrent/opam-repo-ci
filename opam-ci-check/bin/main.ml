(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

open Cmdliner
open Opam_ci_check
module Distro = Dockerfile_opam.Distro

let ( // ) = Filename.concat

(* [(let+)] is [Term.(const f $ v)] *)
let ( let+ ) t f = Term.(const f $ t)

(* [(and+)] is [Term.(const (fun x y -> (x, y)) $ a $ b)] *)
let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)

(* This is Cmdliner.Term.map, which is not available in Cmdliner 1.1.1 *)
let map_term f x = Term.app (Term.const f) x

let to_exit_code : (unit, string) result Term.t -> Cmd.Exit.code Term.t =
  map_term @@ function
  | Ok () -> 0
  | Error msg ->
      Printf.eprintf "%s%!" msg;
      1

let fetch_package_src ~dir ~pkg opam =
  match opam.OpamFile.OPAM.url with
  | None -> None
  | Some url -> (
      let res =
        OpamProcess.Job.run
        @@ OpamRepository.pull_tree
             (OpamPackage.to_string pkg)
             (OpamFilename.Dir.of_string dir)
             (OpamFile.URL.checksum url)
             [ OpamFile.URL.url url ]
      in
      match res with
      | OpamTypes.Not_available (_, msg) ->
          print_endline msg;
          None
      | Up_to_date _ | Result _ -> Some dir)

let read_package_opam ~opam_repo_dir pkg =
  let opam_path = Opam_helpers.path_from_pkg ~opam_repo_dir pkg // "opam" in
  (* NOTE: We use OpamFile.OPAM.read_from_channel instead of OpamFile.OPAM.file
     to prevent the name and version fields being automatically added *)
  In_channel.with_open_text opam_path @@ fun ic ->
  try OpamFile.OPAM.read_from_channel ic
  with
  | OpamPp.Bad_format ((_, msg) : OpamPp.bad_format)
  | OpamPp.Bad_version (((_, msg) : OpamPp.bad_format), _)
    ->
    Printf.eprintf "Error in %s: Failed to parse the opam file due to '%s'" opam_path msg;
    exit 1

type package_spec = {
  pkg : OpamPackage.t;
  src : string option; (* package source directory *)
  newly_published : bool option;
}

let lint package_specs local_repo_dir =
  match local_repo_dir with
  | None -> failwith "TODO: default to using the opam repository"
  | Some opam_repo_dir -> (
      print_endline
      @@ Printf.sprintf "Linting opam-repository at %s ..." opam_repo_dir;
      Dir_helpers.with_temp_dir "opam-ci-check-lint-" @@ fun dir ->
      let process_package { pkg; src; newly_published } =
        let opam = read_package_opam ~opam_repo_dir pkg in
        let pkg_src_dir =
          if Option.is_none src then fetch_package_src ~dir ~pkg opam else src
        in
        Lint.v ~pkg ~newly_published ~pkg_src_dir opam
      in
      let all_lint_packages = List.map process_package package_specs in
      let errors = Lint.lint_packages ~opam_repo_dir all_lint_packages in
      match errors with
      | Ok [] ->
          print_endline "No errors";
          Ok ()
      | Ok errors ->
          errors |> List.map Lint.msg_of_error |> String.concat "\n"
          |> Printf.sprintf "%s\n" |> Result.error
      | Error _ as e -> e)

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

let build_run_spec ~variant ~hash ~no_cache ~only_print ~with_tests
    ~lower_bounds ~pkg ~opam_repository =
  let pkg = OpamPackage.of_string pkg in
  let base =
    let hash =
      Stdlib.Option.(hash |> map (fun h -> "@" ^ h) |> value ~default:"")
    in
    let image =
      Printf.sprintf "ocaml/opam:%s%s" (Variant.docker_tag variant) hash
    in
    Spec.Docker image
  in
  let config =
    Spec.opam ~variant ~lower_bounds ~with_tests ~opam_version:`Dev pkg
  in
  Test.build_run_spec ~use_cache:(not no_cache) ~only_print ?opam_repository
    ~base config
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

let split_on_first c s =
  match String.split_on_char c s with
  | [a] | [a; ""] -> Ok (a, None)
  | [a; b] -> Ok (a, Some b)
  | _      -> Error s

let arg_with_optional_attrs_conv arg_conv attrs_conv =
  let (let^) = Result.bind in
  let conv s =
    match split_on_first ':' s with
    | Ok (a, None) ->
      let^ x = Arg.conv_parser arg_conv a in
      Ok (x, [])
    | Ok (a, Some attrs) ->
      let^ x = Arg.conv_parser arg_conv a in
      let^ attrs = Arg.(conv_parser (list attrs_conv)) attrs in
      Ok (x, attrs)
    | Error invalid ->
      Fmt.error_msg
        "Invalid argument spec %s. Argument specs should be of the form arg[:k1=v1[,k2=v2]]"
        invalid
  in
  let pp fmt (x, attrs) =
    Fmt.pf fmt "%a[:%a]"
      Arg.(conv_printer arg_conv) x
      Arg.(conv_printer (list ~sep:',' attrs_conv)) attrs
  in
  Arg.conv ~docv:"ARG[:key=value[,key=value]]" (conv, pp)

let package_specs_term =
  let opam_file_conv =
      let conv = Arg.parser_of_kind_of_string ~kind:"opam package spec in the form <name.version>" OpamPackage.of_string_opt in
      let pp = Fmt.of_to_string OpamPackage.to_string in
      Arg.conv ~docv:"PACKAGE_SPEC" (conv, pp)
  in
  let attr_conv =
    let parser s =
      match split_on_first '=' s with
      | Ok ("new", Some b) -> (
          match bool_of_string_opt b with
          | Some bool -> Ok (`New bool)
          | None   -> Error (`Msg (b ^ " must be [true] or [false]"))
        )
      | Ok ("src", Some dir)  -> (
          match Sys.is_directory dir with
          | true -> Ok (`Src dir)
          | false -> Error (`Msg (dir ^ " is not a directory"))
          | exception (Sys_error msg) ->  Error (`Msg msg))
      | _ -> Error (`Msg (Printf.sprintf "%s is an not a valid attribute. Only [src=<path>] or [new=<true|false>] allowed" s) )
    in
    let pp fmt v =
      match v with
      | `New b -> Fmt.pf fmt "new=%b" b
      | `Src s -> Fmt.pf fmt "src=%s" s
    in
    Arg.conv ~docv:"ATTR" (parser, pp)
  in
  let package_spec_conv = arg_with_optional_attrs_conv opam_file_conv attr_conv
  in
  let info =
    Arg.info []
      ~doc:
        "List of package specifications (format: \
         <name.version>[:src=<path>][,new=<true|false>]). If [src] is not \
         specified, the sources are downloaded from the source URL. If [new] \
         is not specified, it is inferred from the opam repository."
  in
  let+ pgk_spec_data = Arg.value (Arg.pos_all package_spec_conv [] info) in
  pgk_spec_data |>
  List.map (fun (pkg, specs) ->
      let src = List.find_map (function `Src s -> Some s | _ -> None) specs in
      let newly_published = List.find_map (function `New b -> Some b | _ -> None) specs in
      {pkg; src; newly_published}
    )


let lint_cmd =
  let doc = "Lint the opam repository directory" in
  let term =
    Term.(const lint $ package_specs_term $ local_opam_repo_term)
    |> to_exit_code
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

let distro_options =
  Dockerfile_opam.Distro.distros
  |> List.map (fun distro ->
         let tag = Dockerfile_opam.Distro.tag_of_distro distro in
         (tag, tag))

let arch_options =
  Ocaml_version.arches
  |> List.map (fun arch -> (Ocaml_version.string_of_arch arch, arch))

let arch_conv = Arg.enum arch_options
let distro_conv = Arg.enum distro_options

(* TODO: This should be limited to an enum, but we can't currently compute  it
   without depending on /lib/build.ml *)
let compiler_conv =
  let of_string s =
    Ocaml_version.of_string s
    |> Result.map (fun v ->
           let variant = Ocaml_version.extra v in
           let version =
             v |> Ocaml_version.with_just_major_and_minor
             |> Ocaml_version.to_string
           in
           (version, variant))
  in
  let pp =
    Fmt.of_to_string @@ fun (ver, var) ->
    let open Ocaml_version in
    ver |> of_string_exn |> Fun.flip with_variant var |> to_string
  in
  Arg.conv ~docv:"COMPILER" (of_string, pp)

let variant =
  let+ arch =
    let info =
      Arg.info [ "arch" ]
        ~doc:
          ("The target architecture to build on. Supported architectures: "
          ^ (arch_options |> List.map fst |> String.concat ", "))
    in
    Arg.value (Arg.opt arch_conv `X86_64 info)
  and+ distro =
    let default =
      (Distro.resolve_alias Distro.master_distro :> Distro.t)
      |> Distro.tag_of_distro
    in
    let info =
      Arg.info [ "distro" ]
        ~doc:
          ("The target distro to build on. Supported distros: "
          ^ (distro_options |> List.map fst |> String.concat ", "))
    in
    Arg.value (Arg.opt distro_conv default info)
  and+ compiler =
    let default =
      ( Ocaml_version.(to_string (with_just_major_and_minor Releases.latest)),
        None )
    in
    let info =
      Arg.info [ "compiler" ]
        ~doc:
          {|The compiler to build with. Supported compilers are currently
limited to those listed in https://github.com/ocurrent/opam-repo-ci/blob/master/doc/platforms.md#ocaml-versions|}
    in
    Arg.value (Arg.opt compiler_conv default info)
  in
  Variant.v ~arch ~distro ~compiler

let hash =
  let info =
    Arg.info [ "hash" ]
      ~doc:
        {|The specific hash of a pre-built platform base to build in.  Not that
this MUST correspond to the correct distro and compiler used for the base.
E.g., For a docker image with the tag
'ocaml/opam:debian-12-ocaml-4.14-flambda@sha256:4d8b208fb0017792b379e59d3fbae4be866c38af4bcbc2d1f348e4d249e6546f'
The build must be invoked with '--distro debian-12 --compiler 4.14~flambda
--hash sha256:4d8b208fb0017792b379e59d3fbae4be866c38af4bcbc2d1f348e4d249e6546f'.
Use of other values for the distro and compiler will result in a build failure.
|}
  in
  Arg.value (Arg.(opt (some string) None) info)

let only_print =
  let info =
    Arg.info [ "only-print" ]
      ~doc:"Only print the Dockerfile, but don't build it"
  in
  Arg.value (Arg.flag info)

let build_cmd =
  let doc =
    "Build a package optionally with tests and/or using lower bounds packages"
  in
  let term =
    to_exit_code
    @@
    let+ variant = variant
    and+ hash = hash
    and+ no_cache = no_cache
    and+ only_print = only_print
    and+ lower_bounds = lower_bounds
    and+ with_tests = with_test
    and+ pkg = pkg_term
    and+ opam_repository = local_opam_repo_term in
    build_run_spec ~variant ~hash ~no_cache ~only_print ~with_tests
      ~lower_bounds ~pkg ~opam_repository
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
