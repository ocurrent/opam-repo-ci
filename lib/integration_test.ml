(* Which part of the pipeline should be tested *)
type t =
  | Lint
  | List_revdeps

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

let check_list_revdeps = function
  | Ok l ->
    OpamPackage.Set.iter (fun p -> Printf.printf "%s\n" @@ OpamPackage.to_string p) l;
    exit 0
  | Error (`Msg s) ->
    Printf.printf "Error \"%s\"\n" s;
    exit 0
  | Error (`Active _) as s -> s
