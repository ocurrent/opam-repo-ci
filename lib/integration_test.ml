open Current.Syntax

(* Which part of the pipeline should be tested *)
type t =
  | Lint

let check_lint ~test_config lint =
  let f () =
    let+ result = Current.catch lint in
    begin match result with
      | Ok () -> Printf.printf "Ok ()\n"
      | Error (`Msg s) -> Printf.printf "Error \"%s\"\n" s
    end;
    exit 0
  in
  Option.iter (fun _ -> ignore @@ f ()) test_config
