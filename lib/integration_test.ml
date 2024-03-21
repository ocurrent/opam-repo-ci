(* Which part of the pipeline should be tested *)
type t =
  | Lint
  | Analyse

let check_lint ?test_config output =
  let f = function
    | Ok () ->
      Printf.printf "Ok ()\n";
      exit 0
    | Error (`Msg s) ->
      Printf.printf "Error \"%s\"\n" s;
      exit 0
    | Error (`Active _) as s -> s
  in
  match test_config with
  | None -> output
  | Some Lint -> f output
  | Some _ -> Ok ()

let check_analyse ?test_config output =
  let f = function
    | Ok analysis ->
      Printf.printf "%s\n" @@ Yojson.Safe.(prettify @@ to_string analysis);
      exit 0
    | Error (`Msg s) ->
      Printf.printf "Error \"%s\"\n" s;
      exit 0
    | Error (`Active _) as s -> s
  in
  match test_config with
  | Some Analyse -> f output
  | None
  | Some _ -> output
