let parse_revdeps ~pkg output =
  String.split_on_char '\n' output
  |> List.fold_left (fun acc -> function
    | "" -> acc
    | line ->
        (* day10's revdeps output can carry non-package lines (opam warnings)
           between the @@@OUTPUT markers, so skip anything that isn't a package.
           OBuilder's [opam list] output is clean, so this is a no-op there.
           NOTE: [opam list --recursive --depends-on <pkg>] also returns <pkg>
           itself, which we drop. *)
        match OpamPackage.of_string_opt line with
        | Some revdep when not (OpamPackage.equal pkg revdep) ->
            OpamPackage.Set.add revdep acc
        | _ -> acc
  ) OpamPackage.Set.empty

(* Don't include new packages that we're adding
   in the revdeps, as these are already tested *)
let filter_new_pkgs ~new_pkgs =
  OpamPackage.Set.filter (fun p -> not @@ List.mem p new_pkgs)

let revdeps ~pkg ~new_pkgs output =
  filter_new_pkgs ~new_pkgs @@ parse_revdeps ~pkg output
