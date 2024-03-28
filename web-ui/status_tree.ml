module Client = Opam_repo_ci_api.Client

type key = string

type 'a tree =
  | Leaf of key * 'a
  | Branch of key * 'a option * 'a t
and 'a t = 'a tree list

let rec add k x ts = match k, ts with
  | [], _ -> assert false
  | [k], [] -> [Leaf (k, x)]
  | [k], Leaf (k', _)::_ when String.equal k k' -> assert false
  | [k], (Leaf _ as t)::ts -> t :: add [k] x ts
  | [k], Branch (k', Some _, _)::_ when String.equal k k' -> assert false
  | [k], Branch (k', None, t)::ts when String.equal k k' -> Branch (k, Some x, t) :: ts
  | [k], (Branch _ as t)::ts -> t :: add [k] x ts
  | k::ks, [] -> [Branch (k, None, add ks x [])]
  | k::ks, Leaf (k', y)::ts when String.equal k k' -> Branch (k, Some y, add ks x []) :: ts
  | k::ks, Branch (k', y, t)::ts when String.equal k k' -> Branch (k, y, add ks x t) :: ts
  | _::_, t::ts -> t :: add k x ts

let is_skip = Astring.String.is_prefix ~affix:"[SKIP]"

open Tyxml.Html

let status (s, elms1) elms2 =
  let status_class_name =
    match (s : Client.State.t) with
    | NotStarted -> "not-started"
    | Aborted -> "aborted"
    | Failed m when is_skip m -> "skipped"
    | Failed _ -> "failed"
    | Passed -> "passed"
    | Active -> "active"
    | Undefined _ -> "undefined"
  in
  li ~a:[a_class [status_class_name]] (elms1 @ elms2)

let tag_experimental b =
  (* TODO: Remove this *)
  if Astring.String.is_prefix ~affix:"macos-homebrew" b ||
    Astring.String.is_prefix ~affix:"freebsd" b
  then b ^ " (experimental)"
  else b

let rec render_status : (Client.State.t * _) tree -> _ = function
  | Leaf (_, x) ->
    status x []
  | Branch (b, None, ss) ->
    let b = tag_experimental b in
    li ~a:[a_class ["none"]]
      [txt b; ul ~a:[a_class ["statuses"]] (List.map render_status ss)]
  | Branch (_, Some (((NotStarted | Aborted | Failed _ | Undefined _), _) as x), _) ->
    (* Do not show children of a node that has failed (guarantees in
       service/pipeline.ml means that only successful parents have
       children with meaningful error messages) *)
    status x []
  | Branch (_, Some (((Passed | Active), _) as x), ss) ->
    status x [ul ~a:[a_class ["statuses"]] (List.map render_status ss)]

let render ss = ul ~a:[a_class ["statuses"]] (List.map render_status ss)
