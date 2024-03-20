open Current.Syntax

type kind = [`Built | `Analysed | `Linted]

type 'a f = { f : 'x . label: string Current.t -> kind -> 'x Current.t -> 'a Current.t }

type 'a ctx = {
  label : string Current.t;
  map : 'a f;
  merge : 'a -> 'a -> 'a;
  empty : 'a;
}

type 'a t = ctx: 'a ctx -> 'a Current.t

let status_sep = String.make 1 Opam_repo_ci_api.Common.status_sep

let flatten t ~map ~merge ~empty =
  let ctx = { label = Current.return "" ; map ; merge ; empty } in
  t ~ctx

let empty ~ctx = Current.return ctx.empty

let action kind job ~ctx = ctx.map.f ~label:ctx.label kind job

let leaf ~label t ~ctx =
  t ~ctx: { ctx with label = let+ prefix = ctx.label in prefix ^ label }

let leaf_dyn ~label t ~ctx =
  t ~ctx: { ctx with label = let+ prefix = ctx.label and+ label = label in prefix ^ label }

let dir ~label = leaf ~label:(label ^ status_sep)

let dir_dyn ~label t ~ctx =
  t ~ctx: { ctx with label = let+ prefix = ctx.label and+ label = label in prefix ^ label ^ status_sep }

let of_list children ~ctx =
  List.fold_left
    (fun acc t ->
      let+ acc = acc
      and+ t = t ~ctx
      in ctx.merge acc t)
    (empty ~ctx)
    children

let root = of_list

let branch ~label children = dir ~label (of_list children)

let branch_dyn ~label children = dir_dyn ~label (of_list children)

let actioned_branch (type a) ~label (action : a t) children ~ctx =
  let+ t = leaf ~label action ~ctx
  and+ ts = dir ~label (of_list children) ~ctx
  in ctx.merge t ts

let actioned_branch_dyn ~label action children ~ctx =
  let+ t = leaf_dyn ~label action ~ctx
  and+ ts = dir_dyn ~label (of_list children) ~ctx
  in ctx.merge t ts

let map_reduce ordered ?collapse_key ~map ~merge ~empty input =
  let+ lst = Current.list_map ordered ?collapse_key map input in
  List.fold_left merge empty lst

let list_map ordered ?collapse_key fn input ~ctx =
  let results =
    map_reduce ordered ?collapse_key
      ~map: (fun t -> fn t ~ctx)
      ~merge: ctx.merge
      ~empty: ctx.empty
      input
  in
  let+ state = Current.state ~hidden:true results
  and+ input = Current.state ~hidden:true input
  in
  match input, state with
  | Error _, _ -> ctx.empty (* Ignore any error coming from the input. *)
  | Ok _, Ok x -> x
  | Ok _, Error (`Msg m) -> failwith m
  | Ok _, Error (`Active _) ->
    Logs.warn (fun f -> f "Node.list_map: input is ready but output is pending!");
    ctx.empty

module Bool = struct
  include Bool
  let pp = Fmt.bool
end

let bool_map fn input =
  let input = Current.map (fun x -> if x then [x] else []) input in
  list_map (module Bool) (fun _ -> fn ()) input

let collapse ~key ~value ~input t ~ctx =
  Current.collapse ~key ~value ~input (t ~ctx)

let job_id job =
  let+ md = Current.Analysis.metadata job in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None
