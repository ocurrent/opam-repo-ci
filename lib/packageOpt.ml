type t = {
  pkg : OpamPackage.t;
  urgent : ([`High | `Low] -> bool) option;
  has_tests : bool;
}

let compare {pkg = pkg1; urgent = _; has_tests = _} {pkg = pkg2; urgent = _; has_tests = _} =
  OpamPackage.compare pkg1 pkg2

let pp f {pkg; urgent = _; has_tests = _} =
  Fmt.of_to_string OpamPackage.to_string f pkg
