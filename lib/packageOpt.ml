type t = {
  pkg : OpamPackage.t;
  urgent : ([`High | `Low] -> bool) option;
}

let compare {pkg = pkg1; urgent = _} {pkg = pkg2; urgent = _} =
  OpamPackage.compare pkg1 pkg2

let pp f {pkg; urgent = _} =
  Fmt.of_to_string OpamPackage.to_string f pkg
