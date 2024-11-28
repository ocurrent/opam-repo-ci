(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

(*Generate random temporary file names. **)
let temp_file_name dir prefix suffix =
  let prng = Random.State.make_self_init () in
  let random_suffix = string_of_int (Random.State.int prng 0x10000000) in
  Filename.concat dir (prefix ^ random_suffix ^ suffix)

(*Create a temporary directory with the given prefix. **)
let create_temp_dir prefix =
  let base_temp_dir = Filename.get_temp_dir_name () in
  let unique_temp_dir = temp_file_name base_temp_dir prefix "" in
  Unix.mkdir unique_temp_dir 0o700;
  unique_temp_dir

let remove_dir dir =
  let rec remove_dir_rec dir =
    let entries = Sys.readdir dir in
    Array.iter
      (fun entry ->
        let entry = Filename.concat dir entry in
        let entry_is_directory =
          (* Sys.is_directory raises a Sys_error for broken symlinks *)
          try Sys.is_directory entry with Sys_error _ -> false
        in
        if entry_is_directory then remove_dir_rec entry else Sys.remove entry)
      entries;
    Sys.rmdir dir
  in
  remove_dir_rec dir

let with_temp_dir prefix f =
  let dir = create_temp_dir prefix in
  match f dir with
  | exception exn ->
      remove_dir dir;
      raise exn
  | result ->
      remove_dir dir;
      result
