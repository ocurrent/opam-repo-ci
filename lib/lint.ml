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

  let of_dir ~master ~job ~packages cwd =
    let master = Current_git.Commit.hash master in
    exec ~cwd ~job [|"git"; "merge"; "-q"; "--"; master|] >>/= fun () ->
    let changed =
      packages
      |> List.filter_map (fun (pkg, change) ->
          match change with
          | Analyse.Analysis.(New Release | Unavailable | SignificantlyChanged | InsignificantlyChanged ) -> Some (OpamPackage.to_string pkg)
          | _ -> None)
      |> function
          | [] -> []
          | changed -> ["--changed-packages"; String.concat "," changed ]
    in
    let new_ =
      packages
      |> List.filter_map (fun (pkg, change) ->
          match change with
          | Analyse.Analysis.(New Package) -> Some (OpamPackage.to_string pkg)
          | _ -> None)
      |> function
          | [] -> []
          | new_ -> ["--newly-published"; String.concat "," new_ ]
    in
    let cmd = ["opam-ci-check"; "lint"; "--opam-repository"; "."] @ changed @ new_ in
    exec ~cwd ~job (cmd |> Array.of_list)
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
