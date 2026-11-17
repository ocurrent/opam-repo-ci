  module Prefix = struct
    (* For context, see https://github.com/ocurrent/opam-repo-ci/pull/316#issuecomment-2160069803 *)
    let prefix_conflict_class_map =
        ("mysys2-", "msys2-env");
        ("arch-", "ocaml-arch");
        ("ocaml-env-mingw", "ocaml-env-mingw");
        ("ocaml-env-msvc", "ocaml-env-msvc");
        ("host-arch-", "ocaml-host-arch");
        ("host-system-", "ocaml-host-system");
        ("system-", "ocaml-system");
    let conflict_class_prefix_map =
      List.map (fun (a, b) -> (b, a)) prefix_conflict_class_map
    let prefixes = List.map fst prefix_conflict_class_map
      let name = OpamPackage.name_to_string pkg in
          if String.starts_with ~prefix name then
      let prefix =
        List.find_opt (fun prefix -> String.starts_with ~prefix name) prefixes
      match prefix with
      | Some prefix -> (
          match List.assoc_opt prefix prefix_conflict_class_map with
          | Some required_conflict_class ->
              else
                ( pkg,
                  PrefixConflictClassMismatch
          match List.assoc_opt conflict_class conflict_class_prefix_map with
                ( pkg,
                  PrefixConflictClassMismatch
                    (WrongPrefix { conflict_class; required_prefix = prefix })
                )
      let conflict_classes =
      let name = OpamPackage.name_to_string pkg in
  let msg_of_prefix_conflict_class_mismatch ~pkg = function
    | WrongPrefix { conflict_class; required_prefix } ->
          pkg conflict_class required_prefix
    | WrongConflictClass { prefix; required_conflict_class } ->
          "Error in %s: package with prefix '%s' requires conflict class '%s'"
          pkg prefix required_conflict_class

    | DuneIsBuild ->
          pkg
    | NameCollision other_pkg ->
