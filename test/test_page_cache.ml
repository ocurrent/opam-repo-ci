module C = Opam_repo_ci.Page_cache

let now = ref 0.
let clock () = !now

let cache ?(max_bytes = 1000) () =
  now := 0.;
  C.create ~clock ~max_bytes ()

let renders = ref 0

let render ?(ttl = 60.) ?(tag = "") body () =
  incr renders;
  Lwt.return (Ok { C.body; ttl; tag })

let last : C.outcome option ref = ref None

let get ?ttl ?tag ?valid c key body =
  let open Lwt.Infix in
  C.find_or_render c key ?valid (render ?ttl ?tag body) >|= function
  | Ok o -> last := Some o; o.C.body, o.C.max_age
  | Error () -> failwith "unexpected error"

let source = Alcotest.testable
    (fun f s -> Fmt.string f (match s with C.Hit -> "hit" | Miss -> "miss" | Joined -> "joined" | Stale -> "stale"))
    ( = )

let check_source msg expected =
  match !last with
  | Some o -> Alcotest.check source msg expected o.C.source
  | None -> Alcotest.fail "no outcome recorded"

let test_hit_and_expiry _ () =
  let open Lwt.Infix in
  let c = cache () in
  renders := 0;
  get c "k" "one" >>= fun (b, age) ->
  Alcotest.(check string) "rendered" "one" b;
  Alcotest.(check int) "max-age" 60 age;
  check_source "first is a miss" C.Miss;
  now := 10.;
  get c "k" "two" >>= fun (b, age) ->
  Alcotest.(check string) "served from cache" "one" b;
  Alcotest.(check int) "max-age counts down" 50 age;
  check_source "second is a hit" C.Hit;
  (match !last with Some o -> Alcotest.(check int) "age" 10 o.C.age | None -> ());
  Alcotest.(check int) "one render" 1 !renders;
  now := 61.;
  get c "k" "two" >>= fun (b, _) ->
  Alcotest.(check string) "re-rendered after expiry" "two" b;
  Alcotest.(check int) "two renders" 2 !renders;
  C.invalidate c "k";
  get c "k" "three" >>= fun (b, _) ->
  Alcotest.(check string) "re-rendered after invalidate" "three" b;
  Lwt.return_unit

let test_no_store_when_ttl_zero _ () =
  let open Lwt.Infix in
  let c = cache () in
  get ~ttl:0. c "k" "one" >>= fun (b, age) ->
  Alcotest.(check string) "served" "one" b;
  Alcotest.(check int) "max-age 0" 0 age;
  Alcotest.(check int) "not stored" 0 (C.length c);
  Lwt.return_unit

let test_eviction _ () =
  let open Lwt.Infix in
  let c = cache ~max_bytes:10 () in
  get c "a" "aaaa" >>= fun _ ->
  get c "b" "bbbb" >>= fun _ ->
  Alcotest.(check int) "two stored" 8 (C.size c);
  get c "c" "cccc" >>= fun _ ->
  Alcotest.(check int) "oldest evicted" 8 (C.size c);
  Alcotest.(check int) "two entries" 2 (C.length c);
  renders := 0;
  get c "a" "AAAA" >>= fun (b, _) ->
  Alcotest.(check string) "a was evicted" "AAAA" b;
  get c "big" "0123456789ab" >>= fun (b, _) ->
  Alcotest.(check string) "oversize body still served" "0123456789ab" b;
  Alcotest.(check bool) "oversize body not stored" true (C.size c <= 10);
  Lwt.return_unit

let test_single_flight _ () =
  let open Lwt.Infix in
  let c = cache () in
  let started = ref 0 in
  let release, wake = Lwt.wait () in
  let slow () =
    incr started;
    release >|= fun () -> Ok { C.body = "slow"; ttl = 60.; tag = "" }
  in
  let r1 = C.find_or_render c "k" slow in
  let r2 = C.find_or_render c "k" slow in
  let r3 = C.find_or_render c "k" slow in
  Alcotest.(check int) "one render started" 1 !started;
  Alcotest.(check bool) "waiters pending" true (Lwt.state r2 = Lwt.Sleep);
  Lwt.wakeup wake ();
  Lwt.all [ r1; r2; r3 ] >>= fun rs ->
  List.iter (function
      | Ok o -> Alcotest.(check string) "shared body" "slow" o.C.body
      | Error () -> Alcotest.fail "unexpected error") rs;
  (match rs with
   | [ Ok a; Ok b; Ok c ] ->
     Alcotest.check source "starter is a miss" C.Miss a.C.source;
     Alcotest.check source "waiter joined" C.Joined b.C.source;
     Alcotest.check source "waiter joined" C.Joined c.C.source
   | _ -> Alcotest.fail "unexpected results");
  Alcotest.(check int) "stored once" 1 (C.length c);
  C.find_or_render c "k" slow >>= fun _ ->
  Alcotest.(check int) "later call is a hit" 1 !started;
  Lwt.return_unit

exception Raised

let test_error_not_cached _ () =
  let open Lwt.Infix in
  let c = cache () in
  let failing () = Lwt.return (Error "boom") in
  let r1 = C.find_or_render c "k" failing in
  let r2 = C.find_or_render c "k" failing in
  Lwt.all [ r1; r2 ] >>= fun rs ->
  List.iter (function
      | Error "boom" -> ()
      | _ -> Alcotest.fail "expected the render error") rs;
  Alcotest.(check int) "nothing stored" 0 (C.length c);
  let raising () = Lwt.fail Raised in
  Lwt.catch
    (fun () -> C.find_or_render c "k" raising >|= fun _ -> Alcotest.fail "expected exception")
    (function Raised -> Lwt.return_unit | ex -> Lwt.fail ex)
  >>= fun () ->
  (* A failed render must not leave the key marked in-flight. *)
  C.find_or_render c "k" (fun () -> Lwt.return (Ok { C.body = "ok"; ttl = 60.; tag = "" })) >|= function
  | Ok o -> Alcotest.(check string) "renders again after failure" "ok" o.C.body
  | Error _ -> Alcotest.fail "unexpected error"

let test_validation _ () =
  let open Lwt.Infix in
  let c = cache () in
  renders := 0;
  let checked = ref [] in
  let valid tag = checked := tag :: !checked; Lwt.return (tag = "passed") in
  get ~tag:"passed" c "k" "one" >>= fun (b, _) ->
  Alcotest.(check string) "rendered" "one" b;
  get ~valid c "k" "two" >>= fun (b, _) ->
  Alcotest.(check string) "valid tag is a hit" "one" b;
  Alcotest.(check (list string)) "validator saw the stored tag" [ "passed" ] !checked;
  Alcotest.(check int) "one render" 1 !renders;
  (* Simulate the source changing: the stored tag no longer validates. *)
  let valid _ = Lwt.return_false in
  get ~tag:"pending" ~valid c "k" "three" >>= fun (b, _) ->
  Alcotest.(check string) "invalid tag re-renders" "three" b;
  check_source "rejected entry is stale" C.Stale;
  Alcotest.(check int) "two renders" 2 !renders;
  get c "k" "four" >>= fun (b, _) ->
  Alcotest.(check string) "no validator: plain hit on the new entry" "three" b;
  Lwt.return_unit

let tests = [
  Alcotest_lwt.test_case "hit-and-expiry" `Quick test_hit_and_expiry;
  Alcotest_lwt.test_case "no-store-when-ttl-zero" `Quick test_no_store_when_ttl_zero;
  Alcotest_lwt.test_case "eviction" `Quick test_eviction;
  Alcotest_lwt.test_case "single-flight" `Quick test_single_flight;
  Alcotest_lwt.test_case "error-not-cached" `Quick test_error_not_cached;
  Alcotest_lwt.test_case "validation" `Quick test_validation;
]
