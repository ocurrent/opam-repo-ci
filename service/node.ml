open Current.Syntax

type t =
  | Root of t list
  | Branch of { label : string; children : t list }
  | Leaf of
      {
        label : string;
        job_id : Current.job_id option;
        result : [`Built | `Checked] Current_term.Output.t;
      }

let status_sep = String.make 1 Opam_repo_ci_api.Common.status_sep

let job_id job =
  let+ md = Current.Analysis.metadata job in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let root children = Root children
let branch ~label children = Branch { label; children }
let leaf ~label ~job_id result = Leaf { label; job_id; result }

let of_job ~label kind job =
  let+ job_id = job_id job
  and+ result = job |> Current.map (fun _ -> kind) |> Current.state ~hidden:true
  in
  leaf ~label ~job_id result

let rec flatten ~prefix f = function
  | Root children ->
    List.concat_map (flatten ~prefix f) children
  | Branch { label; children } ->
    let prefix = prefix ^ label ^ status_sep in
    List.concat_map (flatten ~prefix f) children
  | Leaf { label; job_id; result } ->
    let label = prefix ^ label in
    [f ~label ~job_id ~result]

let flatten f t = flatten f t ~prefix:""

let pp_result f = function
  | Ok `Built -> Fmt.string f "built"
  | Ok `Checked -> Fmt.string f "checked"
  | Error (`Active _) -> Fmt.string f "active"
  | Error (`Msg m) -> Fmt.string f m

let rec dump f = function
  | Root children ->
    Fmt.pf f "@[<v>%a@]"
      (Fmt.(list ~sep:cut) dump) children
  | Branch { label; children } ->
    Fmt.pf f "@[<v2>%s%a@]"
      label
      Fmt.(list ~sep:nop (cut ++ dump)) children
  | Leaf { label; job_id = _; result } ->
    Fmt.pf f "%s (%a)" label pp_result result
