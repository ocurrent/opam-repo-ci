open Current.Syntax

type action = {
  job_id : Current.job_id option;
  result : [`Built | `Analysed | `Linted] Current_term.Output.t;
}

type t =
  | Root of t list
  | Branch of { label : string; action : action option; children : t list }

let status_sep = String.make 1 Opam_repo_ci_api.Common.status_sep

let job_id job =
  let+ md = Current.Analysis.metadata job in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let root children = Root children
let branch ~label children = Branch { label; action = None; children }
let actioned_branch ~label action children = Branch { label; action = Some action; children }
let leaf ~label action = Branch { label; action = Some action; children = [] }

let action kind job =
  let+ job_id = job_id job
  and+ result = job |> Current.map (fun _ -> kind) |> Current.state ~hidden:true
  in
  { job_id; result }

let rec flatten ~prefix f = function
  | Root children ->
    List.concat_map (flatten ~prefix f) children
  | Branch { label; action = None; children } ->
    let prefix = prefix ^ label ^ status_sep in
    List.concat_map (flatten ~prefix f) children
  | Branch { label; action = Some { job_id; result }; children } ->
    let prefix_children = prefix ^ label ^ status_sep in
    let label = prefix ^ label in
    f ~label ~job_id ~result :: List.concat_map (flatten ~prefix:prefix_children f) children

let flatten f t = flatten f t ~prefix:""

let pp_result f = function
  | Ok `Built -> Fmt.string f "built"
  | Ok `Analysed -> Fmt.string f "analysed"
  | Ok `Linted -> Fmt.string f "linted"
  | Error (`Active (`Ready | `Running)) -> Fmt.string f "active"
  | Error (`Active `Waiting_for_confirmation) -> Fmt.string f "waiting-for-confirmation"
  | Error (`Msg m) -> Fmt.string f m

let rec dump f = function
  | Root children ->
    Fmt.pf f "@[<v>%a@]"
      (Fmt.(list ~sep:cut) dump) children
  | Branch { label; action = None; children } ->
    Fmt.pf f "@[<v2>%s%a@]"
      label
      Fmt.(list ~sep:nop (cut ++ dump)) children
  | Branch { label; action = Some { job_id = _; result }; children } ->
    Fmt.pf f "@[<v2>%s (%a)%a@]"
      label
      pp_result result
      Fmt.(list ~sep:nop (cut ++ dump)) children
