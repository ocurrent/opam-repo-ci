(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

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

let list_revdeps ?(opam_repo = "https://opam.ocaml.org") ?(transitive = true)
    pkg =
  (* Create local opam root and switch *)
  Env.create_local_switch_maybe (Some opam_repo);
  OpamConsole.msg "Listing revdeps for %s\n" pkg;
  let package = OpamPackage.of_string pkg in
  let package_set = OpamPackage.Set.singleton package in
  Env.with_unlocked_switch () (fun st ->
      let transitive_deps =
        if transitive then transitive_revdeps st package_set
        else OpamPackage.Set.empty
      in
      let non_transitive = non_transitive_revdeps st package_set in
      OpamPackage.Set.union transitive_deps non_transitive
      |> filter_coinstallable st package_set
      |> OpamPackage.Set.to_list_map (fun x -> x))

let find_latest_versions packages =
  let open OpamPackage in
  let packages_set = packages |> OpamPackage.Set.of_list in
  let versions_map = packages_set |> to_map in
  Name.Map.fold
    (fun name _versions acc ->
      let latest_version = max_version packages_set name in
      Set.add latest_version acc)
    versions_map Set.empty
  |> OpamPackage.Set.to_list_map (fun x -> x)

module Display = struct
  let packages packages =
    packages
    |> List.iter (fun p -> OpamConsole.msg "%s\n" (OpamPackage.to_string p));
    OpamConsole.msg "Number of reverse dependencies: %d\n"
      (List.length packages)
end
