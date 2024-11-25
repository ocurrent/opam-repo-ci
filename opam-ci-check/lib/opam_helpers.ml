(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

let ( // ) = Filename.concat

let path_from_pkg ~opam_repo_dir pkg =
  opam_repo_dir // "packages"
  // OpamPackage.Name.to_string (OpamPackage.name pkg)
  // OpamPackage.to_string pkg
