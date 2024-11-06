(* SPDX-License-Identifier: Apache-2.0
 * Copyright (c) 2024 Puneeth Chaganti <punchagan@muse-amuse.in>, Shon Feder <shon.feder@gmail.com>, Tarides <contact@tarides.com>
 *)

(** Data describing and manipulating supported compilers versions *)

let all_supported = Ocaml_version.Releases.recent @ Ocaml_version.Releases.unreleased_betas
