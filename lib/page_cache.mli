(** A small in-process cache of rendered pages.

    Entries are keyed by string, bounded by total body size, and expire after
    a per-entry time-to-live chosen by the renderer. Concurrent requests for a
    key that is not cached share one render ("single-flight"), so a burst of
    requests for an expensive page costs one render rather than one per
    request. *)

type 'e t
(** A cache whose renderers fail with ['e]. *)

val create : ?clock:(unit -> float) -> max_bytes:int -> unit -> 'e t
(** [create ~max_bytes ()] makes an empty cache that holds at most [max_bytes]
    of bodies. When inserting would exceed that, the oldest entries are
    evicted first. A body larger than [max_bytes] on its own is served but
    never stored. [clock] defaults to [Unix.gettimeofday]. *)

type outcome = {
  body : string;
  max_age : int;  (** Seconds until this body expires, as of now. *)
}

val find_or_render :
  'e t -> string ->
  (unit -> (string * float, 'e) result Lwt.t) ->
  (outcome, 'e) result Lwt.t
(** [find_or_render t key render] returns the cached body for [key] if present
    and unexpired. Otherwise it calls [render], which produces the body and
    its time-to-live in seconds, stores the result, and returns it. If a render
    for [key] is already in progress, the call waits for it instead of starting
    another. A render that returns [Error] or raises is not cached, and every
    waiter sees the same failure. A body whose time-to-live is not positive is
    returned but not stored. *)

val size : _ t -> int
(** Total bytes of bodies currently stored. *)

val length : _ t -> int
(** Number of entries currently stored. *)

val invalidate : _ t -> string -> unit
(** [invalidate t key] drops the stored body for [key], if any. A render in
    progress for [key] is unaffected. *)

val clear : _ t -> unit
