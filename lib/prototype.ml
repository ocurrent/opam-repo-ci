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

let list_revdeps package =
  OpamConsole.msg "Listing revdeps for %s\n" (OpamPackage.to_string package);
  let package_set = OpamPackage.Set.singleton package in
  with_locked_switch () (fun st ->
      let transitive = transitive_revdeps st package_set in
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
  (* FIXME: We need to pin the target package when trying to install and test
     the new packages *)
  let name = OpamPackage.name revdep in
  let version = OpamPackage.version revdep in
  let version_contstaint = (`Eq, version) in
  with_locked_switch () @@ fun st ->
  let _ = OpamClient.install st [ (name, Some version_contstaint) ] in
  (* FIXME: This doesn't run the tests of the revdeps package *)
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
  (* FIXME: This doesn't run the tests of the revdeps package *)
  List.iter H.generate_lock_and_build dirs;
  ()
