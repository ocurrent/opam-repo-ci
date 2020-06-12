open Lwt.Infix
open Current.Syntax

let pool = Current.Pool.create ~label:"analyse" 2

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

let read_file ~max_len path =
  let ch = open_in path in
  Fun.protect ~finally:(fun () -> close_in ch)
    (fun () ->
       let len = in_channel_length ch in
       if len <= max_len then really_input_string ch len
       else Fmt.failwith "File %S too big (%d bytes)" path len
    )

module OpamPackage = struct
  include OpamPackage

  let to_yojson x = [%derive.to_yojson:string] (OpamPackage.to_string x)
  let of_yojson x = Result.map OpamPackage.of_string ([%derive.of_yojson:string] x)
end

module Analysis = struct
  type t = {
    packages : OpamPackage.t list;
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ppx_deriving_yojson_runtime.Result.Ok x -> x
    | Ppx_deriving_yojson_runtime.Result.Error _ -> failwith "lol"

  let packages t = t.packages

  let is_duniverse _ = false

  let ocamlformat_source _ = None
  
  let check_opam_version =
    let version_2 = OpamVersion.of_string "2" in
    fun name opam ->
      let opam_version = OpamFile.OPAM.opam_version opam in
      if OpamVersion.compare opam_version version_2 < 0 then
        Fmt.failwith "Package %S uses unsupported opam version %s (need >= 2)" name (OpamVersion.to_string opam_version)

  let of_dir ~job ~master dir =
    let master = Current_git.Commit.hash master in
    let cmd = "", [| "git"; "merge"; "-q"; "--"; master |] in
    Current.Process.exec ~cwd:dir ~cancellable:true ~job cmd >>= function
    | Error (`Msg msg) ->
      Current.Job.log job "Merge failed: %s" msg;
      Lwt_result.fail (`Msg "Cannot merge to master - please rebase!")
    | Ok () ->
      let cmd = "", [| "git"; "diff"; "--name-only"; master; "packages/" |] in
      Current.Process.check_output ~cwd:dir ~cancellable:true ~job cmd >>!= fun output ->
      let packages =
        String.split_on_char '\n' output
        |> List.filter_map (fun path ->
            match String.split_on_char '/' path with
            | [] | [""] | ["packages"] | ["packages"; _] -> None
            | "packages" :: name :: package :: _ ->
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
              Some pkg
            | _ ->
              Fmt.failwith "Unexpected path %S in output (expecting 'packages/name/pkg/...')" path
          )
        |> List.sort_uniq OpamPackage.compare
        |> List.filter_map (fun pkg ->
            let rel_path =
              Printf.sprintf "packages/%s/%s"
                (OpamPackage.name_to_string pkg)
                (OpamPackage.to_string pkg)
            in
            let full_path = Fpath.to_string dir / rel_path in
            match Unix.lstat full_path with
            | Unix.{ st_kind = S_DIR; _ } ->
              (* Check it exists, parses, and is the right version. *)
              let opam_path = full_path / "opam" in
              let content = read_file ~max_len:102400 opam_path in
              let opam = OpamFile.OPAM.read_from_string content in
              check_opam_version opam_path opam;
              Some pkg
            | _ ->
              Fmt.failwith "%S is not a directory!" rel_path
            | exception Unix.Unix_error(Unix.ENOENT, _, _) ->
              (* Note: we check here (rather than in the diff command) so that
                 deleting something in a package's files directory still re-checks the package. *)
              Current.Job.log job "Package %s has been deleted" (OpamPackage.to_string pkg);
              None
          )
      in
      let r = { packages } in
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

  let auto_cancel = false
  let latched = true
end

module Examine_cache = Current_cache.Generic(Examine)

let examine ~master src =
  Current.component "Analyse" |>
  let> src = src
  and> master = master in
  Examine_cache.run Examine.No_context { Examine.Key.src } { Examine.Value.master }
