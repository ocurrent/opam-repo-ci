open Opam_ci_check

let () =
  (* Detect collision modulo ('-' | '_' | '') *)
  assert (Lint.Checks.package_name_collision "foo_barbaz" "foo-bar-baz");
  (* No collision otherwise *)
  assert (not @@ Lint.Checks.package_name_collision "foo-barbaz" "eoocar_caz")
