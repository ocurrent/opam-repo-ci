open Obuilder_spec_opam

type t = {
  label : string;
  pool : string;        (* OCluster pool *)
  variant : Variant.t;
}

let pp f t = Fmt.string f t.label
let compare a b = compare a.label b.label
