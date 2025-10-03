open Lwt.Infix
open Current.Syntax

let pool_size = 4
let pool = Current.Pool.create ~label:"lint" pool_size

let ( >>/= ) x f = x >>= fun x -> f (Result.get_ok x)
let exec ~cwd ~job cmd = Current.Process.exec ~cwd ~cancellable:true ~job ("", cmd)

module Check = struct
  type t = unit

  let marshal () = Yojson.Safe.to_string `Null
  let unmarshal _ = ()

  let parse_errors_from_log job =
    let job_id = Current.Job.id job in
    let path = Current.Job.log_path job_id in
    match path with
    | Ok path ->
        Lwt_io.with_file ~mode:Lwt_io.input (Fpath.to_string path) (fun ch ->
          let rec aux acc =
            (* We choose the relevant output lines by filtering lines starting
               with Error/Warning. The other log lines in the file would start
               with a timestamp (logged info), or not start with the
               Error/Warning prefix (other text output from the CLI tool). *)
            Lwt_io.read_line_opt ch >>= function
            | Some line ->
              if (String.starts_with ~prefix:"Error" line ||
                  String.starts_with ~prefix:"Warning" line)
              then
                aux (line :: acc)
              else
                aux acc
            | None -> Lwt.return acc
          in
          aux [] >>= function
          | [] -> Lwt.return (Error (`Msg "Could not find any Error/Warning lines in CLI output."))
          | errors -> Lwt.return (Ok (String.concat "\n" errors)))
    | Error (`Msg m) -> Lwt.return (Error (`Msg (Fmt.str "Could not find the log file for job <%s>: %s" job_id m)))

  let of_dir ~master ~job ~packages cwd =
    let master = Current_git.Commit.hash master in
    exec ~cwd ~job [|"git"; "merge"; "-q"; "--"; master|] >>/= fun () ->
    let package_args =
      packages
      |> List.filter_map (fun (pkg, change) ->
           let pkg_str = OpamPackage.to_string pkg in
           let new_arg = match change with
             | Analyse.Analysis.(New Package) -> Some "true"
             | Analyse.Analysis.(New Release | Unavailable | SignificantlyChanged | InsignificantlyChanged ) -> Some "false"
             | Deleted ->
                Current.Job.log job "Skipping deleted package: %s" (OpamPackage.to_string pkg);
                None
           in
           Option.map (Printf.sprintf "%s:new=%s" pkg_str) new_arg)
    in
    let cmd = ["opam-ci-check"; "lint"; "--machine-readable"; "--opam-repository"; "."] @ package_args in
    (* Show instructions to run locally *)
    let install_instructions = ["opam"; "pin"; "opam-ci-check"; "git+https://github.com/ocurrent/opam-repo-ci.git#live"] in
    Current.Job.write job
    (Fmt.str "@.\
              To reproduce locally, on the opam-repository PR branch, run: @.\
              @[@;<4 4>%s@]\
              @[@;<4 4>%s@]@.@."
     (String.concat " " install_instructions)
     (String.concat " " cmd));
    (* Run the lint! *)
    exec ~cwd ~job (cmd |> Array.of_list)
    >>= function
    | Error (`Msg err) ->
      (* The exec function doesn't capture stdout when the command fails; so we
         parse the command output from the log file, instead. *)
        parse_errors_from_log job >>= (function
        | Ok errors -> Lwt_result.fail (`Msg errors)
        | Error (`Msg msg) ->
          let error_msg = (Fmt.str "internal error: %s - %s" err msg) in
          Lwt_result.fail (`Msg error_msg))
    | Ok () ->
        Lwt_result.return ()
end

module Lint = struct
  type t = {
    master : Current_git.Commit.t;
  }

  module Key = struct
    type t = {
      src : Current_git.Commit.t;
      packages : (OpamPackage.t * Analyse.Analysis.kind) list
    }

    let digest {src; packages} =
      Yojson.Safe.to_string (`Assoc [
        "src", `String (Current_git.Commit.hash src);
        "packages", `List (List.map (fun (pkg, kind) ->
          `Assoc [
            "pkg", `String (OpamPackage.to_string pkg);
            "kind", Analyse.Analysis.kind_to_yojson kind;
          ]) packages);
      ])
  end

  module Value = struct
    type t = unit

    let digest () =
      let json = `Assoc [] in 
      Yojson.Safe.to_string json

  end

  module Outcome = Check

  let id = "opam-ci-lint"

  let run { master } job { Key.src; packages } () =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    match packages with
    | [] ->
      Current.Job.log job "No packages to lint.";
      Lwt.return (Ok ())
    | _ ->
      Current_git.with_checkout ~job src @@ fun dir ->
      Check.of_dir ~master ~job ~packages dir
  let pp f _ = Fmt.string f "Lint"

  let auto_cancel = true
  let latched = true
end

module Lint_cache = Current_cache.Generic(Lint)

let get_packages_kind =
  Current.map (fun packages ->
    List.map (fun (pkg, {Analyse.Analysis.kind; has_tests = _}) ->
      (pkg, kind))
      packages)

let check ?test_config ~master ~packages src =
  Current.component "Lint" |>
  let> src
  and> packages = get_packages_kind packages
  and> master in
  Lint_cache.run { master } { src; packages } ()
  |> Current.Primitive.map_result @@ Integration_test.check_lint ?test_config
