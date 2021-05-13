open Lwt.Infix
open Current.Syntax

let pool_size = 4
let pool = Current.Pool.create ~label:"lint" pool_size
let () = Lwt_preemptive.init 0 pool_size (fun _errlog -> ())

let ( // ) = Filename.concat
let ( >>/= ) x f = x >>= fun x -> f (Result.get_ok x)
let exec ~cwd ~job cmd = Current.Process.exec ~cwd ~cancellable:true ~job ("", cmd)

type error =
  | UnnecessaryField of string
  | MissingField of string
  | UnmatchedName of OpamPackage.Name.t
  | UnmatchedVersion of OpamPackage.Version.t
  | DubiousDuneSubst
  | DuneProjectMissing
  | DuneConstraintMissing
  | BadDuneConstraint of string * string
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

  let get_dune_project_version url =
    Lwt_io.with_temp_dir @@ fun dir ->
    Lwt_preemptive.detach begin fun () ->
      OpamProcess.Job.run (OpamDownload.download ~overwrite:false (OpamFile.URL.url url) (OpamFilename.Dir.of_string dir))
    end () >>= fun f ->
    Lwt_io.with_temp_dir @@ fun dir ->
    Lwt_preemptive.detach begin fun () ->
      OpamFilename.extract f (OpamFilename.Dir.of_string dir)
    end () >>= fun () ->
    let dune_project = Filename.concat dir "dune-project" in
    Lwt.catch begin fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Input dune_project (fun ch -> Lwt_io.read ch >|= Sexplib.Sexp.parse) >>= begin function
      | Sexplib.Sexp.(Done (List [Atom "lang"; Atom "dune"; Atom version], _)) -> Lwt.return_some version
      | Done _ -> Lwt.fail_with "(lang dune ...) is not the first construct"
      | Cont _ -> Lwt.fail_with "Failed to parse the dune-project file"
      end
    end begin function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_none
    | exn -> Lwt.fail exn
    end

  let get_dune_constraint opam =
    let get_max = function
      | None, None -> None
      | Some x, None -> Some x
      | None, Some x -> Some x
      | Some x, Some y when OpamVersionCompare.compare x y >= 0 -> Some x
      | Some _, Some y -> Some y
    in
    let get_min = function
      | None, None | Some _, None | None, Some _ -> None
      | Some x, Some y when OpamVersionCompare.compare x y >= 0 -> Some y
      | Some x, Some _ -> Some x
    in
    let rec get_lower_bound = function
      | OpamFormula.Atom (OpamTypes.Constraint ((`Gt | `Geq), OpamTypes.FString version)) -> Some version
      | Empty | Atom (Filter _) | Atom (Constraint _) -> None
      | Block x -> get_lower_bound x
      | And (x, y) -> get_max (get_lower_bound x, get_lower_bound y)
      | Or (x, y) -> get_min (get_lower_bound x, get_lower_bound y)
    in
    let rec aux = function
      | OpamFormula.Atom (pkg, constr) ->
          if OpamPackage.Name.equal pkg (OpamPackage.Name.of_string "dune") then
            Some (Option.value ~default:"1.0" (get_lower_bound constr))
          else
            None
      | Empty -> None
      | Block x -> aux x
      | And (x, y) -> get_max (aux x, aux y)
      | Or (x, y) -> get_min (aux x, aux y)
    in
    aux opam.OpamFile.OPAM.depends

  let check_dune_constraints ~errors ~pkg opam =
    match opam.OpamFile.OPAM.url with
    | Some url ->
        get_dune_project_version url >|= fun dune_version ->
        let dune_constraint = get_dune_constraint opam in
        begin match dune_constraint, dune_version with
        | None, None -> errors
        | Some _, None -> (pkg, DuneProjectMissing) :: errors
        | None, Some _ -> (pkg, DuneConstraintMissing) :: errors
        | Some dep, Some ver ->
            if OpamVersionCompare.compare dep ver >= 0 then
              errors
            else
              (pkg, BadDuneConstraint (dep, ver)) :: errors
        end
    | None ->
        Lwt.return errors

  let of_dir ~master ~job ~packages cwd =
    let master = Current_git.Commit.hash master in
    exec ~cwd ~job [|"git"; "merge"; "-q"; "--"; master|] >>/= fun () ->
    Lwt_list.fold_left_s (fun errors (pkg, kind) ->
      match kind with
      | Analyse.Analysis.Deleted ->
          Lwt.return errors (* TODO *)
      | Analyse.Analysis.(New | SignificantlyChanged | UnsignificantlyChanged) ->
          get_opam ~cwd pkg >>= fun opam ->
          (* Check name field *)
          let errors = match OpamFile.OPAM.name_opt opam with
            | None -> errors
            | Some name ->
                if OpamPackage.Name.equal name (OpamPackage.name pkg) then
                  (pkg, UnnecessaryField "name") :: errors
                else
                  (pkg, UnmatchedName name) :: errors
          in
          (* Check version field *)
          let errors = match OpamFile.OPAM.version_opt opam with
            | None -> errors
            | Some version ->
                if OpamPackage.Version.equal version (OpamPackage.version pkg) then
                  (pkg, UnnecessaryField "version") :: errors
                else
                  (pkg, UnmatchedVersion version) :: errors
          in
          (* Check presence of the license field *)
          (* TODO: Get rid of this when https://github.com/ocaml/opam/issues/4598 is fixed *)
          let errors = match OpamFile.OPAM.license opam with
            | [] -> (pkg, MissingField "license") :: errors
            | _ -> errors
          in
          (* Check correct use of dune subst *)
          let errors =
            List.fold_left
              (fun errors -> function
                 | OpamTypes.([(CString "dune", None); (CString "subst", None)], filter) ->
                     begin match filter with
                     | Some (OpamTypes.FIdent ([], var, None)) when
                         String.equal (OpamVariable.to_string var) "dev" -> errors
                     | _ -> (pkg, DubiousDuneSubst) :: errors
                     end
                 | _ -> errors
              )
              errors
              opam.OpamFile.OPAM.build
          in
          (* Check correct constraint for dune *)
          check_dune_constraints ~errors ~pkg opam >>= fun errors ->
          (* Check directory structure correctness *)
          scan_dir ~cwd errors pkg >>= fun (errors, check_extra_files) ->
          (* opam lint *)
          Lwt_preemptive.detach begin fun () ->
            OpamFileTools.lint ~check_extra_files ~check_upstream:true opam |>
            List.fold_left (fun errors x -> (pkg, OpamLint x) :: errors) errors
          end () >>= fun errors ->
          Lwt.return errors
    ) [] packages
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
      let json = `Assoc [
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
      | MissingField field ->
          Fmt.str "Error in %s: The field '%s' is not present."
            pkg field
      | DubiousDuneSubst ->
          Fmt.str "Warning in %s: Dubious use of 'dune subst'. \
                   'dune subst' should always only be called with {dev} (i.e. [\"dune\" \"subst\"] {dev}) \
                   If your opam file has been autogenerated by dune, you need to upgrade your dune-project \
                   to at least (lang dune 2.7)."
            pkg
      | DuneProjectMissing ->
          Fmt.str "Warning in %s: The package seems to use dune but the dune-project file is missing." pkg
      | DuneConstraintMissing ->
          Fmt.str "Warning in %s: The package has a dune-project file but no explicit dependency on dune was found." pkg
      | BadDuneConstraint (dep, ver) ->
          Fmt.str "Error in %s: Your dune-project file indicates that this package requires at least dune %s \
                   but your opam file only requires dune >= %s. Please check which requirement is the right one, and fix the other."
            pkg ver dep
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

  let run {master} job { Key.src; packages } () =
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
  Lint_cache.run {Lint.master} { Lint.Key.src; packages } ()
