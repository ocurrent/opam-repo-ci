(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

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
    let gt, rt, _ =
      OpamClient.init ~init_config ~repo ~bypass_checks:true ~interactive:false
        ~update_config:false shell
    in
    (* opam switch create *)
    let update_config = true in
    let switch = OpamSwitch.of_string "default" in
    (* FIXME: OCaml version should be a CLI arg? *)
    let compiler_pkg = OpamPackage.of_string "ocaml-base-compiler.5.1.1" in
    let compiler_name = OpamPackage.name compiler_pkg in
    let compiler_version = (`Eq, OpamPackage.version compiler_pkg) in
    let version_constraint = OpamFormula.Atom compiler_version in
    let invariant = OpamFormula.Atom (compiler_name, version_constraint) in
    let _created, _st =
      OpamSwitchCommand.create gt ~rt ~update_config ~invariant switch
        (fun st ->
          let compiler = (compiler_name, Some compiler_version) in
          OpamCoreConfig.update ~confirm_level:`unsafe_yes ();
          (true, OpamClient.install st [ compiler ]))
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
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_write gt

let with_unlocked_switch () =
  let root_dir = local_opam_root () in
  OpamClientConfig.opam_init ~root_dir ();
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt
