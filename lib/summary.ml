open Current.Syntax

type t = { ok: int; pending: int; err: int; skip: int; lint: int }

let merge a b =
  {
    ok = a.ok + b.ok;
    pending = a.pending + b.pending;
    err = a.err + b.err;
    skip = a.skip + b.skip;
    lint = a.lint + b.lint;
  }

let empty = { ok = 0; pending = 0; err = 0; skip = 0; lint = 0 }
let ok = { empty with ok = 1 }
let pending = { empty with pending = 1 }
let err = { empty with err = 1 }
let skip = { empty with skip = 1 }
let lint = { empty with lint = 1 }

let of_current t =
  let+ result = Current.state ~hidden:true t in
  match result with
  | Ok `Analysed -> empty
  | Ok `Linted -> lint
  | Ok `Built -> ok
  | Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> skip
  | Error `Msg _ -> err
  | Error `Active _ -> pending

let to_string { ok; pending; err; skip; lint } =
  let lint = lint > 0 in
  let main_jobs =
    if pending > 0 then Error (`Active `Running)
    else match ok, err, skip with
      | 0, 0, 0 -> Ok "No build was necessary"
      | 0, 0, _skip -> Error (`Msg "Everything was skipped")
      | ok, 0, 0 -> Ok (Fmt.str "%d jobs passed" ok)
      | ok, 0, skip -> Ok (Fmt.str "%d jobs passed, %d jobs skipped" ok skip)
      | ok, err, skip -> Error (`Msg (Fmt.str "%d jobs failed, %d jobs skipped, %d jobs passed" err skip ok))
  in
  (lint, main_jobs)

let sum { ok; pending; err; skip; lint } = ok + pending + err + skip + lint
