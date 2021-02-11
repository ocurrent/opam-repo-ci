open Lwt.Infix
open Current.Syntax

let pool = Current.Pool.create ~label:"lint" 2

let ( >>/= ) x f = x >>= fun x -> f (Result.get_ok x)
let exec ~cwd ~job cmd = Current.Process.exec ~cwd ~cancellable:true ~job ("", cmd)
let exec_out ~cwd ~job cmd = Current.Process.check_output ~cwd ~cancellable:true ~job ("", cmd)

type error =
  | UnnecessaryField of string
  | UnmatchedName of OpamPackage.Name.t
  | UnmatchedVersion of OpamPackage.Version.t

module Check = struct
  type t = unit

  let marshal () = Yojson.Safe.to_string `Null
  let unmarshal _ = ()

  let get_opam ~cwd ~job pkg =
    let pkg_path =
      Fmt.str "packages/%s/%s/opam"
        (OpamPackage.Name.to_string (OpamPackage.name pkg))
        (OpamPackage.to_string pkg)
    in
    exec_out ~cwd ~job [|"git"; "show"; "HEAD:"^pkg_path|] >>/= fun opam ->
    Lwt.return (OpamFile.OPAM.read_from_string opam)

  let of_dir ~master ~job ~packages cwd =
    let master = Current_git.Commit.hash master in
    exec ~cwd ~job [|"git"; "merge"; "-q"; "--"; master|] >>/= fun () ->
    Lwt_list.fold_left_s (fun errors (pkg, kind) ->
      match kind with
      | Analyse.Analysis.Deleted ->
          Lwt.return errors (* TODO *)
      | Analyse.Analysis.(New | SignificantlyChanged | UnsignificantlyChanged) ->
          get_opam ~cwd ~job pkg >>= fun opam ->
          let errors = match OpamFile.OPAM.name_opt opam with
            | None -> errors
            | Some name ->
                if OpamPackage.Name.equal name (OpamPackage.name pkg) then
                  OpamPackage.Map.add pkg (UnnecessaryField "name") errors
                else
                  OpamPackage.Map.add pkg (UnmatchedName name) errors
          in
          let errors = match OpamFile.OPAM.version_opt opam with
            | None -> errors
            | Some version ->
                if OpamPackage.Version.equal version (OpamPackage.version pkg) then
                  OpamPackage.Map.add pkg (UnnecessaryField "version") errors
                else
                  OpamPackage.Map.add pkg (UnmatchedVersion version) errors
          in
          Lwt.return errors
    ) OpamPackage.Map.empty packages
end

module Lint = struct
  type t = No_context

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
    type t = {
      master : Current_git.Commit.t;
    }

    let digest { master } =
      let json = `Assoc [
          "master", `String (Current_git.Commit.hash master);
        ]
      in
      Yojson.Safe.to_string json
  end

  module Outcome = Check

  let id = "opam-ci-lint"

  let msg_of_errors =
    OpamPackage.Map.mapi (fun package err ->
      let pkg = OpamPackage.to_string package in
      match err with
      | UnnecessaryField field ->
          Fmt.str "Warning in %s: Unnecessary field '%s'. It is suggested to remove it." pkg field
      | UnmatchedName value ->
          Fmt.str "Error in %s: The field 'name' that doesn't match its context. \
                   Field 'name' has value '%s' but was expected of value '%s'."
            pkg
            (OpamPackage.Name.to_string value)
            (OpamPackage.Name.to_string (OpamPackage.name package))
      | UnmatchedVersion value ->
          Fmt.str "Error in %s: The field 'version' that doesn't match its context. \
                   Field 'version' has value '%s' but was expected of value '%s'."
            pkg
            (OpamPackage.Version.to_string value)
            (OpamPackage.Version.to_string (OpamPackage.version package))
    )

  let run No_context job { Key.src; packages } { Value.master } =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun dir ->
    Check.of_dir ~master ~job ~packages dir >|= fun errors ->
    let errors = msg_of_errors errors in
    let errors = OpamPackage.Map.values errors in
    List.iter (Current.Job.log job "%s") errors;
    match errors with
    | [] -> Ok ()
    | [msg] -> Error (`Msg msg)
    | _::_ -> Error (`Msg (Fmt.str "%d errors" (List.length errors)))

  let pp f _ = Fmt.string f "Lint"

  let auto_cancel = false
  let latched = true
end

module Lint_cache = Current_cache.Generic(Lint)

let check ~master ~packages src =
  Current.component "Lint" |>
  let> src = src
  and> packages = packages
  and> master = master in
  Lint_cache.run Lint.No_context { Lint.Key.src; packages } { Lint.Value.master }
