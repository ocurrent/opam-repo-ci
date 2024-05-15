module H = Dune_helpers

let make_repo path =
  let repo_name = OpamRepositoryName.of_string "default" in
  let repo_url = OpamUrl.parse path in
  { OpamTypes.repo_name; repo_url; repo_trust = None }

let local_opam_root () =
  let switch_dir = ".opam-revdeps" in
  OpamFilename.Dir.of_string switch_dir

let create_local_switch_maybe repo_path =
  let root_dir = local_opam_root () in
  let create_switch_dir repo_path =
    OpamClientConfig.opam_init ~root_dir ();
    (* opam init*)
    let init_config = OpamInitDefaults.init_config ~sandboxing:true () in
    let shell = OpamStd.Sys.guess_shell_compat () in
    let repo = make_repo repo_path in
    let gt, rt, _default_compiler =
      OpamClient.init ~init_config ~repo ~bypass_checks:true ~interactive:false
        ~update_config:false shell
    in
    (* opam switch create *)
    let update_config = true in
    let switch = OpamSwitch.of_string "default" in
    (* FIXME: OCaml version should be a CLI arg? *)
    let name = OpamPackage.Name.of_string "ocaml-base-compiler" in
    let version_constraint =
      OpamFormula.Atom (`Eq, OpamPackage.Version.of_string "5.1.1")
    in
    let invariant = OpamFormula.Atom (name, version_constraint) in
    (* FIXME: Install OCaml compiler in the switch *)
    let _created, _switch_state =
      OpamSwitchCommand.create gt ~rt ~update_config ~invariant switch
        (fun st -> (true, st))
    in
    ()
  in
  (* FIXME: reinit if already exists? *)
  if not (OpamFilename.exists_dir root_dir) then
    match repo_path with
    | Some d ->
        OpamConsole.msg
          "Creating local opam switch in %s with default repository URL %s\n"
          (OpamFilename.Dir.to_string root_dir)
          d;
        create_switch_dir d
    | None -> failwith "Opam local repository path must be specified!"
  else ()

let with_locked_switch () =
  let root_dir = local_opam_root () in
  OpamClientConfig.opam_init ~root_dir ();
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_write gt @@ fun _rt ->
  OpamSwitchState.with_ `Lock_write gt

let filter_coinstallable st original_package packages =
  let universe =
    OpamSwitchState.universe st
      ~requested:(OpamPackage.Set.union original_package packages)
      Query
  in
  OpamSolver.coinstallable_subset universe original_package packages

let opam_file_of_package st p =
  match OpamSwitchState.opam_opt st p with
  | None -> OpamFile.OPAM.create p
  | Some o ->
      OpamFile.OPAM.(
        with_name p.OpamPackage.name (with_version p.OpamPackage.version o))

let transitive_revdeps st package_set =
  (* Optional dependencies are not transitive: if a optionally depends on b,
     and b optionally depends on c, it does not follow that a optionally depends on c. *)
  let depopts = false in
  (* [with-only] dependencies are also not included in [reverse_dependencies]. *)
  try
    (* Computes the transitive closure of the reverse dependencies *)
    OpamSwitchState.reverse_dependencies ~depopts ~build:true ~post:false
      ~installed:false ~unavailable:false st package_set
  with Not_found ->
    failwith "TODO: Handle packages that are not found in repo"

(* Optional ([depopt]) and test-only ([with-test]) revdeps are not included in
   [transitive_revdeps], but we still want those revdeps that depend on our
   target package_set optionally or just for tests *directly*. The sole purpose
   of this function is to compute the direct [depopt] and [with-test] revdeps.

   This function adapts the core logic from the non-recursive case of [opam list
   --depends-on].

   See https://github.com/ocaml/opam/blob/b3d2f5c554e6ef3cc736a9f97e252486403e050f/src/client/opamListCommand.ml#L238-L244 *)
let non_transitive_revdeps st package_set =
  (* We filter through all packages known to the state, which follows the logic
     of the [opam list] command, except that we omit inclusion of merely
     installed packages that are not in a repository in the state. This is
     because we are only concerned with revdep testing of installable packages,
     not of random local things that a user may have installed in their system.

     See https://github.com/ocaml/opam/blob/b3d2f5c554e6ef3cc736a9f97e252486403e050f/src/client/opamCommands.ml#L729-L731 *)
  let all_known_packages = st.OpamStateTypes.packages in
  let packages_depending_on_target_packages revdep_candidate_pkg =
    let dependancy_on =
      revdep_candidate_pkg |> opam_file_of_package st
      |> OpamPackageVar.all_depends ~test:true ~depopts:true st
      |> OpamFormula.verifies
    in
    OpamPackage.Set.exists dependancy_on package_set
  in
  OpamPackage.Set.filter packages_depending_on_target_packages
    all_known_packages

let list_revdeps package no_transitive_revdeps =
  OpamConsole.msg "Listing revdeps for %s\n" (OpamPackage.to_string package);
  let package_set = OpamPackage.Set.singleton package in
  with_locked_switch () (fun st ->
      let transitive =
        if no_transitive_revdeps then OpamPackage.Set.empty
        else transitive_revdeps st package_set
      in
      let non_transitive = non_transitive_revdeps st package_set in
      OpamPackage.Set.union transitive non_transitive
      |> filter_coinstallable st package_set)

let find_latest_versions packages =
  let open OpamPackage in
  let versions_map = to_map packages in
  Name.Map.fold
    (fun name _versions acc ->
      let latest_version = max_version packages name in
      Set.add latest_version acc)
    versions_map Set.empty

let install_and_test_package_with_opam package revdep =
  OpamConsole.msg "Installing and testing: package - %s; revdep - %s\n"
    (OpamPackage.to_string package)
    (OpamPackage.to_string revdep);
  let nvs =
    [ package; revdep ]
    |> List.map (fun pkg ->
           (OpamPackage.name pkg, Some (`Eq, OpamPackage.version pkg)))
  in
  with_locked_switch () @@ fun st ->
  OpamCoreConfig.update ~verbose_level:1 ();
  OpamStateConfig.update ?build_test:(Some true) ();

  try
    (* Install the packages *)
    (* TODO: Remove prompt for each install; use --yes *)
    let _ = OpamClient.install st nvs in
    ()
  with e ->
    (* NOTE: The CI is identifying packages to SKIP, error types, etc. based on
       the log output. See
       https://github.com/ocurrent/opam-repo-ci/blob/8746f52b479569c0a55904361c9d64b54628b971/service/main.ml#L34.
       But, we may be able to do better, since we are not a shell script? *)
    (* TODO: Capture the output of the failed command and display all failures
       at the end *)
    OpamConsole.msg "Failed to install %s\n" (OpamPackage.to_string revdep);
    OpamConsole.msg "Error: %s\n" (Printexc.to_string e);
    ()

let install_and_test_packages_with_opam target revdeps_list =
  (match
     OpamConsole.confirm "Do you want test %d revdeps?"
       (List.length revdeps_list)
   with
  | true ->
      OpamConsole.msg "Installing reverse dependencies with pinned %s\n"
        (OpamPackage.to_string target);

      List.iter (install_and_test_package_with_opam target) revdeps_list
  | _ -> print_endline "Quitting!");
  ()

let install_and_test_packages_with_dune opam_repository target packages =
  OpamConsole.msg
    "Installing latest version of reverse dependencies with pinned %s\n"
    (OpamPackage.to_string target);
  let parent = H.create_temp_dir "revdeps_" in
  (* FIXME: there can be 1000s of revdeps?! *)
  let selected_packages = H.take 3 packages in
  (* Prompt before creating the projects *)
  Printf.printf "Do you want to generate %d dummy dune project in %s? (y/n): "
    (List.length selected_packages)
    parent;
  (match read_line () with "y" | "Y" -> () | _ -> failwith "Quitting!");
  let dirs =
    H.create_dummy_projects parent opam_repository target selected_packages
  in
  List.iter H.generate_lock_and_build dirs;
  ()
