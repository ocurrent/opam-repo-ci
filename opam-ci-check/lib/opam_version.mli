type t = [ `Dev | `V2_0 | `V2_1 | `V2_2 | `V2_3 | `V2_4 | `V2_5 ] [@@deriving to_yojson]

val to_string : t -> string
