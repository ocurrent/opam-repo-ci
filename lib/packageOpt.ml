type t = {
  pkg : OpamPackage.t;
  wait_for_revdeps : bool;
}

let compare {pkg = pkg1; wait_for_revdeps = _} {pkg = pkg2; wait_for_revdeps = _} =
  OpamPackage.compare pkg1 pkg2

let pp f {pkg; wait_for_revdeps} =
  Fmt.pf f "%s (wait_for_revdeps = %b)" (OpamPackage.to_string pkg) wait_for_revdeps
