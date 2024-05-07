module H = Dune_helpers

let set_default_repository path =
  OpamClientConfig.opam_init ();
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_write gt @@ fun rt ->
  let repo_name = OpamRepositoryName.of_string "default" in
  let repo_url = OpamUrl.parse path in
  let repo =
    try
      let r = OpamRepositoryName.Map.find repo_name rt.repositories in
      { r with repo_url }
    with Not_found -> { repo_name; repo_url; repo_trust = None }
  in
  OpamFilename.cleandir (OpamRepositoryPath.root rt.repos_global.root repo_name);
  OpamFilename.remove (OpamRepositoryPath.tar rt.repos_global.root repo_name);
  OpamRepositoryState.remove_from_repos_tmp rt repo_name;
  let repositories =
    OpamRepositoryName.Map.add repo_name repo rt.repositories
  in
  let repo_opams =
    OpamRepositoryName.Map.filter
      (fun name _ ->
        OpamRepositoryName.Map.find_opt name rt.repositories
        = OpamRepositoryName.Map.find_opt name repositories)
      rt.repo_opams
  in
  let rt = { rt with repositories; repo_opams } in
  OpamRepositoryState.Cache.remove ();
  OpamRepositoryState.write_config rt;
  let open OpamProcess.Job.Op in
  let repo_root = OpamRepositoryState.get_repo_root rt repo in
  let _ =
    OpamProcess.Job.with_text "Updating repository..."
    @@ OpamRepository.update repo repo_root
    @@+ fun _ -> Done ()
  in
  ()

let with_locked_switch () =
  OpamClientConfig.opam_init ();
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_write gt @@ fun _rt ->
  OpamSwitchState.with_ `Lock_write gt

let filter_installable st packages =
  let universe = OpamSwitchState.universe st ~requested:packages Query in
  OpamSolver.installable_subset universe packages

let filter_coinstallable st original_package packages =
  let universe =
    OpamSwitchState.universe st
      ~requested:(OpamPackage.Set.union original_package packages)
      Query
  in
  OpamSolver.coinstallable_subset universe original_package packages

let list_revdeps package =
  OpamConsole.msg "Listing revdeps for %s\n" (OpamPackage.to_string package);
  let package_set = OpamPackage.Set.singleton package in
  let dependencies =
    with_locked_switch () @@ fun st ->
    (* FIXME: Add other commands; This is --depopts *)
    OpamSwitchState.reverse_dependencies st ~depopts:true ~build:true
      ~post:false ~installed:false ~unavailable:false package_set
  in
  with_locked_switch () @@ fun st ->
  let installable_deps = filter_installable st dependencies in
  let coinstallable_deps =
    filter_coinstallable st package_set installable_deps
  in
  coinstallable_deps

let find_latest_versions packages =
  let open OpamPackage in
  let versions_map = to_map packages in
  Name.Map.fold
    (fun name _versions acc ->
      let latest_version = max_version packages name in
      Set.add latest_version acc)
    versions_map Set.empty

let install_and_test_package_with_opam package =
  (* FIXME: We need to pin the target package when trying to install and test the new packages *)
  OpamConsole.msg "Installing and testing package: %s\n"
    (OpamPackage.to_string package);
  let name = OpamPackage.name package in
  let version = OpamPackage.version package in
  let version_contstaint = (`Eq, version) in
  with_locked_switch () @@ fun st ->
  OpamClient.install st [ (name, Some version_contstaint) ]

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
