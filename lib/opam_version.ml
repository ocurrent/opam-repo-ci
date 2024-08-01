type t = [ `Dev | `V2_0 | `V2_1 | `V2_2] [@@deriving to_yojson]

let to_string = function
  | `Dev -> "dev"
  | `V2_2 -> "2.2"
  | `V2_1 -> "2.1"
  | `V2_0 -> "2.0"
