open Lwt.Infix
open Current.Syntax

let pool = Current.Pool.create ~label:"analyse" 4

let ( >>!= ) = Lwt_result.bind

let read_file ~max_len path =
  Lwt_io.with_file ~mode:Lwt_io.input path
    (fun ch ->
       Lwt_io.length ch >>= fun len ->
       let len =
         if len <= Int64.of_int max_len then Int64.to_int len
         else Fmt.failwith "File %S too big (%Ld bytes)" path len
       in
       let buf = Bytes.create len in
       Lwt_io.read_into_exactly ch buf 0 len >|= fun () ->
       Bytes.to_string buf
    )

module OpamPackage = struct
  include OpamPackage

  let to_yojson x = [%derive.to_yojson:string] (OpamPackage.to_string x)
  let of_yojson x = Result.map OpamPackage.of_string ([%derive.of_yojson:string] x)
end

module Analysis = struct
  type kind =
    | New
    | Deleted
    | SignificantlyChanged
    | UnsignificantlyChanged
  [@@deriving yojson]

  type t = {
    packages : (OpamPackage.t * kind) list;
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ppx_deriving_yojson_runtime.Result.Ok x -> x
    | Ppx_deriving_yojson_runtime.Result.Error _ -> failwith "lol"

  let packages t = t.packages

  let is_duniverse _ = false

  let opam_version_2 = OpamVersion.of_string "2"

  let check_opam opam =
    let opam_version = OpamFile.OPAM.opam_version opam in
    let pkg = OpamFile.OPAM.package opam in
    if OpamVersion.compare opam_version opam_version_2 < 0 then
      Fmt.failwith
        "Package %S uses unsupported opam version %s (need >= 2)"
        (OpamPackage.to_string pkg)
        (OpamVersion.to_string opam_version);
    begin match OpamFile.OPAM.format_errors opam with
    | [] -> ()
    | errors ->
        let errors =
          errors |>
          List.map (fun (_, err) -> "  - "^OpamPp.string_of_bad_format (OpamPp.Bad_format err)) |>
          String.concat "\n"
        in
        Fmt.failwith "Format errors detected in %s:\n%s" (OpamPackage.to_string pkg) errors
    end;
    ()

  let get_package_name ~path ~name ~package =
    let nme =
      try OpamPackage.Name.of_string name
      with Failure msg -> Fmt.failwith "%S is not a valid package name (in %S): %s" name path msg
    in
    let pkg =
      try OpamPackage.of_string package
      with Failure msg -> Fmt.failwith "%S is not a valid package name.version (in %S): %s" package path msg
    in
    if OpamPackage.Name.compare nme (OpamPackage.name pkg) <> 0 then
      Fmt.failwith "Mismatch between package dir name %S and parent directory name %S" package name;
    pkg

  (* we check extensions in case it changes the outcome of the CI (e.g. x-ci-accept-failures) *)
  let ci_extensions_equal old_file new_file =
    let filter_ci_exts = OpamStd.String.Map.filter (fun name _ -> OpamStd.String.starts_with ~prefix:"x-ci-" name) in
    let old_exts = filter_ci_exts (OpamFile.OPAM.extensions old_file) in
    let new_exts = filter_ci_exts (OpamFile.OPAM.extensions new_file) in
    OpamStd.String.Map.equal OpamPrinter.FullPos.value_equals old_exts new_exts

  let depexts_equal old_file new_file =
    let aux (set1, filter1) (set2, filter2) =
      OpamSysPkg.Set.equal set1 set2 &&
      filter1 = filter2 (* TODO: Add a proper filter_equal to opam-format *)
    in
    List.equal aux old_file.OpamFile.OPAM.depexts new_file.OpamFile.OPAM.depexts

  let add_pkg ~path ~name ~package kind pkgs =
    let update old_kind = match old_kind, kind with
      (* NOTE: stronger_kind >= weaker_kind *)
      | New, (New | Deleted | SignificantlyChanged | UnsignificantlyChanged)
      | Deleted, (Deleted | SignificantlyChanged | UnsignificantlyChanged)
      | SignificantlyChanged, (SignificantlyChanged | UnsignificantlyChanged)
      | UnsignificantlyChanged, UnsignificantlyChanged ->
          old_kind
      (* NOTE: weaker_kind < stronger_kind *)
      | Deleted, New
      | SignificantlyChanged, (New | Deleted)
      | UnsignificantlyChanged, (New | Deleted | SignificantlyChanged) ->
          kind
    in
    OpamPackage.Map.update (get_package_name ~path ~name ~package) update kind pkgs

  let get_opam ~cwd path =
    Lwt.catch begin fun () ->
      read_file ~max_len:102400 (Filename.concat (Fpath.to_string cwd) path) >>=
      Lwt.return_ok
    end begin function
    | Unix.Unix_error _ -> Lwt.return (Error ())
    | e -> Lwt.fail e
    end

  let find_changed_packages ~job ~master dir =
    let cmd = "", [| "git"; "diff"; "--name-only"; master |] in
    Current.Process.check_output ~cwd:dir ~cancellable:true ~job cmd >>!= fun output ->
    output
    |> String.split_on_char '\n'
    |> List.filter (function "" -> false | _ -> true)
    |> Lwt_list.fold_left_s (fun pkgs path ->
        match String.split_on_char '/' path with
        | [_] | ".github"::_ ->
            Lwt.return pkgs
        | "packages" :: name :: package :: "files" :: _ ->
            Lwt.return (add_pkg ~path ~name ~package SignificantlyChanged pkgs)
        | ["packages"; name; package; "opam"] ->
            let cmd = "", [| "git"; "show"; master^":"^path |] in
            Current.Process.check_output ~cwd:dir ~cancellable:true ~job cmd >>= begin function
            | Error _ -> (* new release *)
                Lwt.return (add_pkg ~path ~name ~package New pkgs)
            | Ok old_content ->
                get_opam ~cwd:dir path >|= begin function
                | Error () -> (* deleted package *)
                    add_pkg ~path ~name ~package Deleted pkgs
                | Ok new_content -> (* modified package *)
                    let filename = OpamFile.make (OpamFilename.raw path) in
                    let old_file =
                      try OpamFile.OPAM.read_from_string ~filename old_content
                      with OpamPp.Bad_format _ | OpamPp.Bad_version _ -> OpamFile.OPAM.empty
                    in
                    let new_file =
                      try OpamFile.OPAM.read_from_string ~filename new_content
                      with
                      | OpamPp.Bad_format (_, msg)
                      | OpamPp.Bad_version (_, msg) ->
                          Fmt.failwith "%S failed to be parsed: %s" path msg
                    in
                    check_opam new_file;
                    if OpamFile.OPAM.effectively_equal old_file new_file &&
                       ci_extensions_equal old_file new_file &&
                       depexts_equal old_file new_file
                    then
                      (* the changes are not significant so we ignore this package *)
                      add_pkg ~path ~name ~package UnsignificantlyChanged pkgs
                    else
                      add_pkg ~path ~name ~package SignificantlyChanged pkgs
                end
            end
        | _ ->
          Fmt.failwith "Unexpected path %S in output (expecting 'packages/name/pkg/...')" path
      ) OpamPackage.Map.empty
    >|= Result.ok

  let of_dir ~job ~master dir =
    let master = Current_git.Commit.hash master in
    let cmd = "", [| "git"; "merge"; "-q"; "--"; master |] in
    Current.Process.exec ~cwd:dir ~cancellable:true ~job cmd >>= function
    | Error (`Msg msg) ->
      Current.Job.log job "Merge failed: %s" msg;
      Lwt_result.fail (`Msg "Cannot merge to master - please rebase!")
    | Ok () ->
      find_changed_packages ~job ~master dir >>!= fun packages ->
      let r = { packages = OpamPackage.Map.bindings packages } in
      Current.Job.log job "@[<v2>Results:@,%a@]" Yojson.Safe.(pretty_print ~std:true) (to_yojson r);
      Lwt.return (Ok r)
end

module Examine = struct
  type t = No_context

  module Key = struct
    type t = {
      src : Current_git.Commit.t;
    }

    let digest {src} =
      Current_git.Commit.hash src
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

  module Outcome = Analysis

  let id = "opam-ci-analyse"

  let run No_context job { Key.src } { Value.master } =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun dir ->
    Lwt.catch
      (fun () -> Analysis.of_dir ~master ~job dir)
      (function
        | Failure msg -> Lwt_result.fail (`Msg msg)
        | ex -> Lwt.fail ex
      )

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = true
  let latched = true
end

module Examine_cache = Current_cache.Generic(Examine)

let examine ~master src =
  Current.component "Analyse" |>
  let> src = src
  and> master = master in
  Examine_cache.run Examine.No_context { Examine.Key.src } { Examine.Value.master }
