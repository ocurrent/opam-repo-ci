open Cmdliner
open Opam_ci_check

let lint pkg newly_published local_repo_dir =
  match local_repo_dir with
  | Some d ->
      print_endline @@ Printf.sprintf "Linting %s in %s ..." pkg d;
      (* TODO: Automatically detect newly published *)
      Lint.run_lint pkg newly_published d
  | None ->
      (* TODO Add error handling *)
      print_endline "No opam repository directory specified."

let show_revdeps pkg local_repo_dir no_transitive_revdeps =
  (* Create local opam root and switch *)
  Env.create_local_switch_maybe local_repo_dir;

  (* Get revdeps for the package *)
  let revdeps = Revdeps.list_revdeps pkg no_transitive_revdeps in

  Revdeps.Display.packages revdeps;

  ()

let test_revdeps pkg local_repo_dir use_dune no_transitive_revdeps =
  (* Create local opam root and switch *)
  Env.create_local_switch_maybe local_repo_dir;

  (* Get revdeps for the package *)
  let revdeps = Revdeps.list_revdeps pkg no_transitive_revdeps in

  (* Install and test the first reverse dependency *)
  let latest_versions = Revdeps.find_latest_versions revdeps in

  Revdeps.Display.packages latest_versions;

  (match (use_dune, local_repo_dir) with
  | true, Some d -> Test.test_packages_with_dune d pkg latest_versions
  | true, None ->
      OpamConsole.msg "Opam local repository path must be specified!\n"
  | false, _ -> Test.test_packages_with_opam pkg latest_versions);

  ()

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

let newly_published_term =
  let info =
    Arg.info [ "n"; "newly-published" ]
      ~doc:"The package is a newly published package."
  in
  Arg.value (Arg.flag info)

let lint_cmd =
  let doc = "Lint the opam repository directory" in
  let term =
    Term.(const lint $ pkg_term $ newly_published_term $ local_opam_repo_term)
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
  in
  let info =
    Cmd.info "test" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let cmd =
  let doc = "A tool to list revdeps and test the revdeps locally" in
  let exits = Cmd.Exit.defaults in
  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Cmd.info "opam-ci-check" ~doc ~sdocs:"COMMON OPTIONS" ~exits in
  Cmd.group ~default:term info [ lint_cmd; list_cmd; test_cmd ]

let () = exit (Cmd.eval cmd)
