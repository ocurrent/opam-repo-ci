(** Alternative node representation used to minimise memory load.

    Context in https://github.com/ocurrent/opam-repo-ci/pull/161.  *)

type kind = [`Built | `Analysed | `Linted]

type 'a t
(** A tree of ocurrent results. *)

type 'a f = { f: 'x . label: string Current.t -> kind -> 'x Current.t -> 'a Current.t }
(** A mapping function for the leaves of the tree: It's quantified over
    its input ['x Current.t] as the actions can produce anything, so [f]
    will only be able to extract their metadata with [job_id] and/or [Current.state]. *)

val flatten : 'a t -> map: 'a f -> merge: ('a -> 'a -> 'a) -> empty: 'a -> 'a Current.t
(** [flatten t ~map ~merge ~empty] applies [map] to every [action] leaves
    of the tree, then fold all the results with the [merge] function
    (which should be associative, commutative, with [empty] as its zero). *)

val empty : 'a t
(** An [empty] tree with no leaf. *)

val root : 'a t list -> 'a t
(** [root children] is a branch with no label, used for the root node. *)

val leaf : label:string -> 'a t -> 'a t
(** [leaf ~label action] is a leaf node. *)

val leaf_dyn : label:string Current.t -> 'a t -> 'a t
(** Same as [leaf], but with a dynamic [label]. *)

val action : [ `Built | `Analysed | `Linted ] -> 'a Current.t -> 'b t
(** [action kind job] is a leaf that will be mapped by [flatten]. *)

val actioned_branch : label:string -> 'a t -> 'a t list -> 'a t
(** [actioned_branch ~label action children] is a branch node with an action
    directly attached to the node itself. *)

val actioned_branch_dyn : label:string Current.t -> 'a t -> 'a t list -> 'a t
(** Same as [actioned_branch], but with a dynamic [label]. *)

val branch : label:string -> 'a t list -> 'a t
(** [branch ~label children] is a branch node. *)

val branch_dyn : label:string Current.t -> 'a t list -> 'a t
(** Same as [branch], but with a dynamic [label]. *)

val list_map :
  (module Current_term.S.ORDERED with type t = 'a) ->
  ?collapse_key:string -> ('a Current.t -> 'b t) ->
  'a list Current.t -> 'b t
(** [list_map ord ?collapse_key f lst] is the dynamic list
    [Current.list_map ord ?collapse_key f lst], except that its output is
    fixed to [empty] until the input [lst] is successful. You must ensure
    that the status of the input [lst] is reported elsewhere. *)

val bool_map : (unit -> 'a t) -> bool Current.t -> 'a t

val collapse : key:string -> value:string -> input:_ Current.t -> 'a t -> 'a t
(** [collapse ~key ~value ~input t] performs [Current.collapse]. *)

val job_id : 'a Current.t -> Current.job_id option Current.t
(** [job_id t] is the job id associated with the ocurrent term [t], or [None]
    if the job hasn't been created yet (or if [t] is not an ocurrent primitive). *)
