let parse_revdeps ~pkg output =
  String.split_on_char '\n' output
  |> List.fold_left (fun acc -> function
    | "" -> acc
    | revdep ->
        let revdep = OpamPackage.of_string revdep in
        if OpamPackage.equal pkg revdep then
          (* NOTE: opam list --recursive --depends-on <pkg>
             also returns <pkg> itself *)
          acc
        else
          OpamPackage.Set.add revdep acc
  ) OpamPackage.Set.empty

(* Don't include new packages that we're adding
   in the revdeps, as these are already tested *)
let filter_new_pkgs ~new_pkgs =
  OpamPackage.Set.filter (fun p -> not @@ List.mem p new_pkgs)

let revdeps ~pkg ~new_pkgs output =
  filter_new_pkgs ~new_pkgs @@ parse_revdeps ~pkg output
