type action
(** A result *)

type t
(** A node in the tree of results. *)

val actioned_branch : label:string -> action -> t list -> t
(** [actioned_branch ~label action children] is a branch node with an
    action directly attached to the node itself. *)

val branch : label:string -> t list -> t
(** [branch ~label children] is a branch node. *)

val root : t list -> t
(** [root children] is a branch with no label, used for the root node. *)

val leaf : label:string -> action -> t
(** [leaf ~label action] is a leaf node. *)

val action : [ `Built | `Analysed | `Linted ] -> _ Current.t -> action Current.t
(** [action kind job] is a result whose status is the state of [job]
    but with the value replaced with [kind]. The job ID is extracted from the
    job (so [job] must be a primitive). Although the result is a [Current.t] it
    is always successful and immediately available. *)

val flatten :
  (label:string ->
   job_id:string option ->
   result:[ `Built | `Analysed | `Linted ] Current_term.Output.t -> 'a) ->
  t -> 'a list
(** [flatten f t] converts the tree to a list, applying [f] to each element. *)

val dump : t Fmt.t
