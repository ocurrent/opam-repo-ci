type t
(** A node in the tree of results. *)

val branch : label:string -> t list -> t
(** [branch ~label children] is a branch node. *)

val root : t list -> t
(** [root children] is a branch with no label, used for the root node. *)

val of_job : label:string -> [ `Built | `Checked ] -> _ Current.t -> t Current.t
(** [of_job kind job ~label] is a leaf node whose status is the state of [job]
    but with the value replaced with [kind]. The job ID is extracted from the
    job (so [job] must be a primitive). Although the result is a [Current.t] it
    is always successful and immediately available. *)

val flatten :
  (label:string ->
   job_id:string option ->
   result:[ `Built | `Checked ] Current_term.Output.t -> 'a) ->
  t -> 'a list
(** [flatten f t] converts the tree to a list, applying [f] to each element. *)

val dump : t Fmt.t
