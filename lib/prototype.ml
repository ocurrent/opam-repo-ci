module H = Dune_helpers
module Env = Env
module Revdeps = Revdeps

let install_and_test_package_with_opam package revdep =
  OpamConsole.msg "Installing and testing: package - %s; revdep - %s\n"
    (OpamPackage.to_string package)
    (OpamPackage.to_string revdep);
  let nvs =
    [ package; revdep ]
    |> List.map (fun pkg ->
           (OpamPackage.name pkg, Some (`Eq, OpamPackage.version pkg)))
  in
  Env.with_locked_switch () @@ fun st ->
  OpamCoreConfig.update ~verbose_level:0 ();
  OpamStateConfig.update ?build_test:(Some true) ();

  try
    (* Don't prompt for install / remove *)
    OpamCoreConfig.update ~confirm_level:`unsafe_yes ();

    (* Install the packages *)
    let _ = OpamClient.install st nvs in

    (* Clean-up switch for next test: We remove only the revdep, but not the
       target package being tested. *)
    let _ = OpamClient.remove st ~autoremove:true ~force:true (List.tl nvs) in

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

let install_and_test_packages_with_opam target_pkg revdeps_list =
  let target = OpamPackage.of_string target_pkg in

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

let install_and_test_packages_with_dune opam_repository target_pkg packages =
  let target = OpamPackage.of_string target_pkg in

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
