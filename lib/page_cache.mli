(** A small in-process cache of rendered pages.

    Entries are keyed by string, bounded by total body size, and expire after
    a per-entry time-to-live chosen by the renderer. Concurrent requests for a
    key that is not cached share one render ("single-flight"), so a burst of
    requests for an expensive page costs one render rather than one per
    request. An entry may carry a tag that the caller can check on each hit,
    to drop a page whose source has changed before its time is up. *)

type 'e t
(** A cache whose renderers fail with ['e]. *)

val create : ?clock:(unit -> float) -> max_bytes:int -> unit -> 'e t
(** [create ~max_bytes ()] makes an empty cache that holds at most [max_bytes]
    of bodies. When inserting would exceed that, the oldest entries are
    evicted first. A body larger than [max_bytes] on its own is served but
    never stored. [clock] defaults to [Unix.gettimeofday]. *)

type entry = {
  body : string;
  ttl : float;    (** Seconds to keep the body. Not positive: serve, don't store. *)
  tag : string;   (** Opaque; handed back to [valid] on each hit. *)
}

type source =
  | Hit     (** Served from the store. *)
  | Miss    (** Nothing stored; this call ran the render. *)
  | Joined  (** Nothing stored; waited on a render another call started. *)
  | Stale   (** Stored but [valid] rejected it; rendered again. *)

type outcome = {
  body : string;
  max_age : int;  (** Seconds until this body expires, as of now. *)
  age : int;      (** Seconds since the body was stored; 0 when just rendered. *)
  tag : string;
  source : source;
}

val find_or_render :
  'e t -> string ->
  ?valid:(string -> bool Lwt.t) ->
  (unit -> (entry, 'e) result Lwt.t) ->
  (outcome, 'e) result Lwt.t
(** [find_or_render t key render] returns the cached body for [key] if present,
    unexpired, and (when [valid] is given) [valid tag] holds. Otherwise it
    calls [render], stores the result, and returns it. If a render for [key]
    is already in progress, the call waits for it instead of starting another.
    A render that returns [Error] or raises is not cached, and every waiter
    sees the same failure. *)

val invalidate : _ t -> string -> unit
(** [invalidate t key] drops the stored body for [key], if any. A render in
    progress for [key] is unaffected. *)

val size : _ t -> int
(** Total bytes of bodies currently stored. *)

val length : _ t -> int
(** Number of entries currently stored. *)

val clear : _ t -> unit
