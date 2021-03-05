open Lwt.Infix
open Current.Syntax

let pool = Current.Pool.create ~label:"lint" 4

let ( // ) = Filename.concat
let ( >>/= ) x f = x >>= fun x -> f (Result.get_ok x)
let exec ~cwd ~job cmd = Current.Process.exec ~cwd ~cancellable:true ~job ("", cmd)

type error =
  | UnnecessaryField of string
  | UnmatchedName of OpamPackage.Name.t
  | UnmatchedVersion of OpamPackage.Version.t
  | UnexpectedFile of string
  | ForbiddenPerm of string
  | OpamLint of (int * [`Warning | `Error] * string)

module Check = struct
  type t = unit

  let marshal () = Yojson.Safe.to_string `Null
  let unmarshal _ = ()

  let path_from_pkg pkg =
    "packages" //
    (OpamPackage.Name.to_string (OpamPackage.name pkg)) //
    (OpamPackage.to_string pkg)

  let get_opam ~cwd pkg =
    Analyse.Analysis.get_opam ~cwd (path_from_pkg pkg // "opam") >>/= fun opam ->
    Lwt.return (OpamFile.OPAM.read_from_string opam)

  let is_perm_644 file =
    Lwt_unix.stat file >|= function
    | {st_kind = S_REG; st_perm = 0o644; _} -> true
    | _ -> false

  let get_files dirname =
    Lwt_unix.opendir dirname >>= fun dir ->
    let rec aux files =
      Lwt.catch begin fun () ->
        Lwt_unix.readdir dir >>= fun file ->
        if Fpath.is_rel_seg file then
          aux files
        else
          aux (file :: files)
      end begin function
      | End_of_file -> Lwt.return files
      | exn -> Lwt.fail exn
      end
    in
    Lwt.finalize
      (fun () -> aux [])
      (fun () -> Lwt_unix.closedir dir)

  let scan_dir ~cwd errors pkg =
    let dir = Fpath.to_string cwd // path_from_pkg pkg in
    get_files dir >>= fun files ->
    let rec aux errors extra_files = function
      | [] -> Lwt.return (errors, extra_files)
      | "opam"::files ->
          is_perm_644 (dir // "opam") >|= begin function
          | true -> errors
          | false -> ((pkg, ForbiddenPerm (dir // "opam")) :: errors)
          end >>= fun errors ->
          aux errors extra_files files
      | "files"::files ->
          get_files (dir // "files") >>= fun extra_files ->
          Lwt_list.fold_left_s (fun errors file ->
            is_perm_644 (dir // "files" // file) >|= function
            | true -> errors
            | false -> ((pkg, ForbiddenPerm ("files" // file)) :: errors)
          ) errors extra_files >>= fun errors ->
          let check_hash file hash = try OpamHash.check_file file hash with _ -> false in
          let extra_files =
            List.map (fun file ->
              (OpamFilename.Base.of_string file, check_hash (dir // "files" // file))
            ) extra_files
          in
          aux errors extra_files files
      | file::files ->
          aux ((pkg, UnexpectedFile file) :: errors) extra_files files
    in
    aux errors [] files

  let of_dir ~master ~job ~packages cwd =
    let master = Current_git.Commit.hash master in
    exec ~cwd ~job [|"git"; "merge"; "-q"; "--"; master|] >>/= fun () ->
    Lwt_list.fold_left_s (fun errors (pkg, kind) ->
      match kind with
      | Analyse.Analysis.Deleted ->
          Lwt.return errors (* TODO *)
      | Analyse.Analysis.(New | SignificantlyChanged | UnsignificantlyChanged) ->
          get_opam ~cwd pkg >>= fun opam ->
          let errors = match OpamFile.OPAM.name_opt opam with
            | None -> errors
            | Some name ->
                if OpamPackage.Name.equal name (OpamPackage.name pkg) then
                  (pkg, UnnecessaryField "name") :: errors
                else
                  (pkg, UnmatchedName name) :: errors
          in
          let errors = match OpamFile.OPAM.version_opt opam with
            | None -> errors
            | Some version ->
                if OpamPackage.Version.equal version (OpamPackage.version pkg) then
                  (pkg, UnnecessaryField "version") :: errors
                else
                  (pkg, UnmatchedVersion version) :: errors
          in
          scan_dir ~cwd errors pkg >>= fun (errors, check_extra_files) ->
          let errors =
            OpamFileTools.lint ~check_extra_files ~check_upstream:true opam |>
            List.fold_left (fun errors x -> (pkg, OpamLint x) :: errors) errors
          in
          Lwt.return errors
    ) [] packages
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
    List.map (fun (package, err) ->
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
      | UnexpectedFile file ->
          Fmt.str "Error in %s: Unexpected file in %s/files/%s" pkg (Check.path_from_pkg package) file
      | ForbiddenPerm file ->
          Fmt.str
            "Error in %s: Forbidden permission for file %s/%s. All files should have permissions 644."
            pkg (Check.path_from_pkg package) file
      | OpamLint warn ->
          let warn = OpamFileTools.warns_to_string [warn] in
          Fmt.str "Error in %s: %s" pkg warn
    )

  let run No_context job { Key.src; packages } { Value.master } =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun dir ->
    Check.of_dir ~master ~job ~packages dir >|= fun errors ->
    let errors = msg_of_errors errors in
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
