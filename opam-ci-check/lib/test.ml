(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

module H = Dune_helpers
module D = Dir_helpers

type error = OpamPackage.t * exn

let error_to_string : error -> string =
 fun (pkg, err) ->
  Printf.sprintf "Failed to install %s\nError: %s\n"
    (OpamPackage.to_string pkg)
    (Printexc.to_string err)

let pkg_atom : OpamPackage.t -> OpamFormula.atom =
 fun pkg -> (OpamPackage.name pkg, Some (`Eq, OpamPackage.version pkg))

let test_package_with_opam_in_state st revdep =
  OpamConsole.msg "Installing and testing %s\n" (OpamPackage.to_string revdep);
  let revdep_atoms = [ pkg_atom revdep ] in
  match OpamClient.install st revdep_atoms with
  | st' ->
      (* Clean-up switch for next test: We remove only the revdep, but not the
         target package being tested. *)
      ignore (OpamClient.remove st' ~autoremove:true ~force:true revdep_atoms);
      None
  | exception e ->
      (* NOTE: The CI is identifying packages to SKIP, error types, etc. based on
         the log output. See
         https://github.com/ocurrent/opam-repo-ci/blob/8746f52b479569c0a55904361c9d64b54628b971/service/main.ml#L34.
         But, we may be able to do better, since we are not a shell script? *)
      Some (revdep, e)

let test_packages_with_opam target_pkg revdeps_list =
  let target = OpamPackage.of_string target_pkg in
  match
    OpamConsole.confirm "Do you want test %d revdeps?"
      (List.length revdeps_list)
  with
  | true ->
      OpamStateConfig.update ?build_test:(Some true) ();
      OpamCoreConfig.update ~verbose_level:0 (* run commands quietly *)
        ~confirm_level:`unsafe_yes (* Don't prompt for install / remove *) ();
      OpamConsole.msg "Installing reverse dependencies with pinned %s\n"
        (OpamPackage.to_string target);
      Env.with_locked_switch () @@ fun st ->
      let st' = OpamClient.install st [ pkg_atom target ] in
      revdeps_list |> List.to_seq
      |> Seq.filter_map (test_package_with_opam_in_state st')
  | _ ->
      print_endline "Quitting!";
      Seq.empty

let test_packages_with_dune opam_repository target_pkg packages =
  let target = OpamPackage.of_string target_pkg in

  OpamConsole.msg
    "Installing latest version of reverse dependencies with pinned %s\n"
    (OpamPackage.to_string target);
  let parent = D.create_temp_dir "revdeps_" in
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
  Ok ()

let ( let* ) = Result.bind

(* Use Docker's ability to build from git repos to default to official opam repo.
   See https://docs.docker.com/build/concepts/context/#git-repositories *)
let build_run_spec
    ?(opam_repository = "https://github.com/ocaml/opam-repository.git") ~base
    config =
  let spec = Opam_build.build_spec ~for_docker:true ~base config in
  let dockerfile =
    Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false ~os:`Unix spec
  in
  let dockerignore = ".git\nREADME.md\n" in
  ()
  |> Bos.OS.Dir.with_tmp "opam-ci-check-build-%s"
       (fun
         (* Create the docker file and ignore file in the tmp dir to avoid polluting the opam-repo  *)
           tmp_dir
         ()
       ->
         let dockerfile_path = Fpath.(tmp_dir / "Dockerfile") in
         let* () =
           (* See https://docs.docker.com/build/concepts/context/#filename-and-location *)
           Bos.OS.File.write
             Fpath.(tmp_dir / "Dockerfile.dockerignore")
             dockerignore
         in
         let* () = Bos.OS.File.write dockerfile_path dockerfile in
         let* () = Bos.OS.Dir.set_current tmp_dir in
         let cmd =
           Bos.Cmd.(
             v "docker" % "build" % "--no-cache" % "--progress=plain" % "--file"
             % Fpath.to_string dockerfile_path
             % "--" % opam_repository)
         in
         match Bos.OS.Cmd.(in_string dockerfile |> run_in cmd) with
         | Ok () -> Ok ()
         | Error (`Msg err) ->
             Error (`Msg ("Failed to build and test the package: " ^ err)))
  |> Result.join
