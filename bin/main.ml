open Cmdliner
open Prototype

let show_revdeps pkg local_repo_dir =
  let package = OpamPackage.of_string pkg in
  (* Set Local Opam Reposistory URL to default repo *)
  (match local_repo_dir with
  | Some d ->
      OpamConsole.msg "Setting default repository URL to %s\n" d;
      set_default_repository d
  | _ -> ());

  (* Get revdeps for the package *)
  let revdeps = list_revdeps package in
  OpamConsole.msg "Number of reverse dependencies: %d\n"
    (OpamPackage.Set.cardinal revdeps);

  (* Display on console *)
  OpamPackage.Set.iter
    (fun pkg -> OpamConsole.msg "%s\n" (OpamPackage.to_string pkg))
    revdeps;

  ()

let test_revdeps pkg local_repo_dir =
  let package = OpamPackage.of_string pkg in
  (* Set Local Opam Reposistory URL to default repo *)
  (match local_repo_dir with
  | Some d ->
      OpamConsole.msg "Setting default repository URL to %s\n" d;
      set_default_repository d
  | _ -> ());

  (* Get revdeps for the package *)
  let revdeps = list_revdeps package in
  OpamConsole.msg "Number of reverse dependencies: %d\n"
    (OpamPackage.Set.cardinal revdeps);

  (* Install and test the first reverse dependency *)
  (* FIXME: Install the latest versions of all dependencies? *)
  match OpamPackage.Set.elements revdeps with
  | [] -> OpamConsole.msg "No reverse dependencies to install and test.\n"
  | hd :: _ ->
      OpamConsole.msg "Installing and testing the first package: %s\n"
        (OpamPackage.to_string hd);
      let _ = install_and_test_package hd in
      ();
      ()

let opam_repo_dir =
  let parse s =
    if Sys.file_exists s then
      let repo_file = Filename.concat s "repo" in
      let packages_dir = Filename.concat s "packages" in
      if Sys.file_exists repo_file && Sys.is_directory packages_dir then
        Ok (Some s)
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

let pkg_term =
  let info = Arg.info [] ~doc:"Package name + version" in
  Arg.required (Arg.pos 0 (Arg.some Arg.string) None info)

let list_cmd =
  let doc = "List the revdeps for a package" in
  let term = Term.(const show_revdeps $ pkg_term $ local_opam_repo_term) in
  let info =
    Cmd.info "list" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let test_cmd =
  let doc = "Test the revdeps for a package" in
  let term = Term.(const test_revdeps $ pkg_term $ local_opam_repo_term) in
  let info =
    Cmd.info "test" ~doc ~sdocs:"COMMON OPTIONS" ~exits:Cmd.Exit.defaults
  in
  Cmd.v info term

let cmd =
  let doc = "A tool to list revdeps and test the revdeps locally" in
  let exits = Cmd.Exit.defaults in
  let term = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info = Cmd.info "opam-revdeps" ~doc ~sdocs:"COMMON OPTIONS" ~exits in
  Cmd.group ~default:term info [ list_cmd; test_cmd ]

let () = exit (Cmd.eval cmd)
