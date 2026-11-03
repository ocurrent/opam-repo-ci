type stored = {
  body : string;
  expires : float;
  tag : string;
}

type entry = {
  body : string;
  ttl : float;
  tag : string;
}

type 'e t = {
  clock : unit -> float;
  max_bytes : int;
  entries : (string, stored) Hashtbl.t;
  order : string Queue.t;  (* insertion order, oldest first; may hold stale keys *)
  in_flight : (string, (entry, 'e) result Lwt.t) Hashtbl.t;
  mutable bytes : int;
}

type outcome = {
  body : string;
  max_age : int;
  tag : string;
}

let create ?(clock = Unix.gettimeofday) ~max_bytes () =
  {
    clock;
    max_bytes;
    entries = Hashtbl.create 64;
    order = Queue.create ();
    in_flight = Hashtbl.create 8;
    bytes = 0;
  }

let size t = t.bytes
let length t = Hashtbl.length t.entries

let remove t key =
  match Hashtbl.find_opt t.entries key with
  | None -> ()
  | Some (e : stored) ->
    Hashtbl.remove t.entries key;
    t.bytes <- t.bytes - String.length e.body

let invalidate t key = remove t key

let clear t =
  Hashtbl.reset t.entries;
  Queue.clear t.order;
  t.bytes <- 0

(* Drop the oldest entries until [need] more bytes fit. Keys in [order] whose
   entry was already replaced or evicted are skipped by [remove]. *)
let rec make_room t need =
  if t.bytes + need > t.max_bytes then
    match Queue.take_opt t.order with
    | None -> ()
    | Some key ->
      remove t key;
      make_room t need

let store t key { body; ttl; tag } =
  let len = String.length body in
  remove t key;
  if ttl > 0. && len <= t.max_bytes then begin
    make_room t len;
    Hashtbl.replace t.entries key { body; expires = t.clock () +. ttl; tag };
    Queue.add key t.order;
    t.bytes <- t.bytes + len
  end

let max_age t expires =
  int_of_float (Float.ceil (expires -. t.clock ())) |> max 0

let find t key =
  match Hashtbl.find_opt t.entries key with
  | Some e when e.expires > t.clock () -> Some e
  | Some _ -> remove t key; None
  | None -> None

let render_and_store t key render =
  let open Lwt.Infix in
  let p =
    match Hashtbl.find_opt t.in_flight key with
    | Some p -> p
    | None ->
      let p = Lwt.catch render Lwt.reraise in
      Hashtbl.replace t.in_flight key p;
      (* Register before attaching the cleanup: if [p] is already resolved
         the callback runs now, and must find the entry to remove. *)
      Lwt.on_termination p (fun () -> Hashtbl.remove t.in_flight key);
      p
  in
  p >|= function
  | Error _ as e -> e
  | Ok ({ body; ttl; tag } as e) ->
    (* Every waiter runs this; only the first finds nothing stored. *)
    if find t key = None then store t key e;
    Ok { body; max_age = int_of_float (Float.ceil ttl) |> max 0; tag }

let find_or_render t key ?valid render =
  let open Lwt.Infix in
  match find t key with
  | None -> render_and_store t key render
  | Some e ->
    let hit = Lwt.return (Ok { body = e.body; max_age = max_age t e.expires; tag = e.tag }) in
    match valid with
    | None -> hit
    | Some valid ->
      valid e.tag >>= fun ok ->
      if ok then hit
      else begin
        remove t key;
        render_and_store t key render
      end
