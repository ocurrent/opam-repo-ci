type t = [ `Dev | `V2_0 | `V2_1 | `V2_2 | `V2_3 | `V2_4 ] [@@deriving to_yojson]

let to_string = function
  | `Dev -> "dev"
  | `V2_4 -> "2.4"
  | `V2_3 -> "2.3"
  | `V2_2 -> "2.2"
  | `V2_1 -> "2.1"
  | `V2_0 -> "2.0"
