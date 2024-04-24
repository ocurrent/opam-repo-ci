let src = Logs.Src.create "opam_repo_ci.index" ~doc:"opam-repo-ci indexer"
module Log = (val Logs.src_log src : Logs.LOG)

module Db = Current.Db

module Job_map = Astring.String.Map

type job_ids = Current.job_id option Job_map.t

type t = {
  record_job : Sqlite3.stmt;
  remove : Sqlite3.stmt;
  repo_exists : Sqlite3.stmt;
  get_jobs : Sqlite3.stmt;
  get_job : Sqlite3.stmt;
  get_job_ids : Sqlite3.stmt;
  list_repos : Sqlite3.stmt;
  full_hash : Sqlite3.stmt;
}

type job_state = [`Not_started | `Active | `Failed of string | `Passed | `Aborted ] [@@deriving show]

let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let is_valid_hash hash =
  let open Astring in
  String.length hash >= 6 && String.for_all Char.Ascii.is_alphanum hash

let get_short_hash = Astring.String.with_range ~len:6

let db = lazy (
  let db = Lazy.force Current.Db.v in
  Current_cache.Db.init ();
  Sqlite3.exec db {|
CREATE TABLE IF NOT EXISTS ci_build_index (
  owner     TEXT NOT NULL,
  name      TEXT NOT NULL,
  hash      TEXT NOT NULL,
  variant   TEXT NOT NULL,
  job_id    TEXT,
  PRIMARY KEY (owner, name, hash, variant)
)|} |> or_fail "create table";
  let record_job = Sqlite3.prepare db "INSERT OR REPLACE INTO ci_build_index \
                                     (owner, name, hash, variant, job_id) \
                                     VALUES (?, ?, ?, ?, ?)" in
  let remove = Sqlite3.prepare db "DELETE FROM ci_build_index \
                                     WHERE owner = ? AND name = ? AND hash = ? AND variant = ?" in
  let list_repos = Sqlite3.prepare db "SELECT DISTINCT name FROM ci_build_index WHERE owner = ?" in
  let repo_exists = Sqlite3.prepare db "SELECT EXISTS (SELECT 1 FROM ci_build_index \
                                                       WHERE owner = ? AND name = ?)" in
  let get_jobs = Sqlite3.prepare db "SELECT ci_build_index.variant, ci_build_index.job_id, cache.ok, cache.outcome \
                                     FROM ci_build_index \
                                     LEFT JOIN cache ON ci_build_index.job_id = cache.job_id \
                                     WHERE ci_build_index.owner = ? AND ci_build_index.name = ? AND ci_build_index.hash = ?" in
  let get_job = Sqlite3.prepare db "SELECT job_id FROM ci_build_index \
                                     WHERE owner = ? AND name = ? AND hash = ? AND variant = ?" in
  let get_job_ids = Sqlite3.prepare db "SELECT variant, job_id FROM ci_build_index \
                                     WHERE owner = ? AND name = ? AND hash = ?" in
  let full_hash = Sqlite3.prepare db "SELECT DISTINCT hash FROM ci_build_index \
                                      WHERE owner = ? AND name = ? AND hash LIKE ?" in
  {
    record_job;
    remove;
    repo_exists;
    get_jobs;
    get_job;
    get_job_ids;
    list_repos;
    full_hash
  }
)

let init () = ignore (Lazy.force db)

let get_job_ids_with_variant t ~owner ~name ~hash =
  Log.info (fun f -> f "@[<h>Index.get_job_ids_with_variant %s/%s %s@]" owner name (get_short_hash hash));
  Db.query t.get_job_ids Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ]
  |> List.fold_left begin fun acc -> function
  | Sqlite3.Data.[ TEXT variant; NULL ] -> Job_map.add variant None acc
  | Sqlite3.Data.[ TEXT variant; TEXT id ] -> Job_map.add variant (Some id) acc
  | row -> Fmt.failwith "get_job_ids: invalid row %a" Db.dump_row row
  end Job_map.empty

let get_job_ids ~owner ~name ~hash =
  let t = Lazy.force db in
  get_job_ids_with_variant t ~owner ~name ~hash
  |> Job_map.bindings |> List.filter_map snd

type n_per_status_t = { not_started : int; pending : int; failed : int; passed : int }

module Metrics = struct
  let empty_n_per_status = { not_started = 0; pending = 0; failed = 0; passed = 0 }
  let n_per_status = ref empty_n_per_status

  let reset_n_per_status () = n_per_status := empty_n_per_status

  let modify_n_per_status f status =
    let g acc = function
    | `Not_started -> { acc with not_started = f acc.not_started }
    | `Pending -> { acc with pending = f acc.pending }
    | `Failed -> { acc with failed = f acc.failed }
    | `Passed -> { acc with passed = f acc.passed }
    in
    n_per_status := g !n_per_status status

  let n_handled = ref 0

  let inc_n_handled n = n_handled := !n_handled + n
end

type build_status = [ `Not_started | `Pending | `Failed | `Passed ]

module Commit_info_cache = struct
  type key = string * string * string

  type elt = {
    build_status : build_status;
    summary : Summary.t
  }

  let empty_elt = { build_status = `Not_started; summary = Summary.empty }

  let cache : (key, elt) Hashtbl.t = Hashtbl.create 1_000
  let cache_max_size = 1_000_000

  let add ~owner ~name ~hash status =
    if Hashtbl.length cache > cache_max_size then begin
      Hashtbl.clear cache;
      Metrics.reset_n_per_status ()
    end;
    let key = (owner, name, hash) in
    (* Decrement existing status if it exists *)
    Hashtbl.find_opt cache key
    |> Option.iter (fun x -> Metrics.modify_n_per_status (fun x -> x - 1) x.build_status);
    (* Increment new status *)
    Metrics.modify_n_per_status (fun x -> x + 1) status.build_status;
    Hashtbl.add cache key status

  let find ~owner ~name ~hash =
    Hashtbl.find_opt cache (owner, name, hash)
    |> function
      | Some s -> s
      | None -> empty_elt

  let find_build_status ~owner ~name ~hash = (find ~owner ~name ~hash).build_status

  let find_summary ~owner ~name ~hash = (find ~owner ~name ~hash).summary

  let remove ~owner ~name ~hash =
    let key = (owner, name, hash) in
    Hashtbl.find_opt cache key
    |> Option.iter (fun status ->
      Metrics.modify_n_per_status (fun x -> x - 1) status.build_status;
      Hashtbl.remove cache key)

  let get_jobs_per_ref ~owner ~name refs =
    Hashtbl.fold
      (fun (owner', name', hash) status acc ->
        if String.equal owner owner' && String.equal name name' then
          List.find_opt (fun (_, hash') -> String.equal hash hash') refs
          |> function
          | None -> acc
          | Some (ref, _) -> (ref, status.summary) :: acc
        else acc)
      cache
      []
end

let get_build_status = Commit_info_cache.find_build_status

let get_summary = Commit_info_cache.find_summary

let set_status ~owner ~name ~hash (build_status, summary) =
  Commit_info_cache.add ~owner ~name ~hash Commit_info_cache.{build_status; summary}

let get_n_per_status () = !Metrics.n_per_status

let get_n_handled () = !Metrics.n_handled

let record ~repo ~hash jobs =
  let { Current_github.Repo_id.owner; name } = repo in
  let t = Lazy.force db in
  let previous = get_job_ids_with_variant t ~owner ~name ~hash in
  let merge variant prev job =
    let set job_id =
      Log.info (fun f -> f "@[<h>Index.record %s/%s %s %s -> %a@]"
                   owner name (get_short_hash hash) variant Fmt.(option ~none:(any "-") string) job_id);
      match job_id with
      | None -> Db.exec t.record_job Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT variant; NULL ]
      | Some id -> Db.exec t.record_job Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT variant; TEXT id ]
    in
    let update j1 j2 =
      match j1, j2 with
      | Some j1, Some j2 when j1 = j2 -> ()
      | None, None -> ()
      | _, j2 -> set j2
    in
    let remove () =
      Log.info (fun f -> f "@[<h>Index.record %s/%s %s %s REMOVED@]"
                   owner name (get_short_hash hash) variant);
      Db.exec t.remove Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT variant ]
    in
    begin match prev, job with
      | Some j1, Some j2 -> update j1 j2
      | None, Some j2 -> set j2
      | Some _, None -> remove ()
      | None, None -> assert false
    end;
    None
  in
  let _ : [`Empty] Job_map.t = Job_map.merge merge previous jobs in
  ()

let is_known_repo ~owner ~name =
  let t = Lazy.force db in
  Log.info (fun f -> f "@[<h>Index.is_known_repo %s/%s@]" owner name);
  match Db.query_one t.repo_exists Sqlite3.Data.[ TEXT owner; TEXT name ] with
  | Sqlite3.Data.[ INT x ] -> x = 1L
  | _ -> failwith "repo_exists failed!"

let get_full_hash ~owner ~name short_hash =
  let t = Lazy.force db in
  if is_valid_hash short_hash then (
    Log.info (fun f -> f "@[<h>Index.get_full_hash %s/%s %s@]" owner name short_hash);
    match Db.query t.full_hash Sqlite3.Data.[ TEXT owner; TEXT name; TEXT (short_hash ^ "%") ] with
    | [] -> Error `Unknown
    | [Sqlite3.Data.[ TEXT hash ]] -> Ok hash
    | [_] -> failwith "get_full_hash: invalid result!"
    | _ :: _ :: _ -> Error `Ambiguous
  ) else Error `Invalid

let get_jobs ~owner ~name hash =
  let t = Lazy.force db in
  Log.info (fun f -> f "@[<h>Index.get_jobs %s/%s %s@]" owner name (get_short_hash hash));
  Db.query t.get_jobs Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ]
  |> List.map @@ function
  | Sqlite3.Data.[ TEXT variant; TEXT job_id; NULL; NULL ] ->
    let outcome = if Current.Job.lookup_running job_id = None then `Aborted else `Active in
    variant, outcome
  | Sqlite3.Data.[ TEXT variant; TEXT _; INT ok; BLOB outcome ] ->
    let outcome =
      if ok = 1L then `Passed else `Failed outcome
    in
    variant, outcome
  | Sqlite3.Data.[ TEXT variant; NULL; NULL; NULL ] ->
    variant, `Not_started
  | row ->
    Fmt.failwith "get_jobs: invalid result: %a" Db.dump_row row

let get_job ~owner ~name ~hash ~variant =
  let t = Lazy.force db in
  Log.info (fun f -> f "@[<h>Index.get_job %s/%s %s %s@]" owner name (get_short_hash hash) variant);
  match Db.query_some t.get_job Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT variant ] with
  | None -> Error `No_such_variant
  | Some Sqlite3.Data.[ TEXT id ] -> Ok (Some id)
  | Some Sqlite3.Data.[ NULL ] -> Ok None
  | _ -> failwith "get_job: invalid result!"

let list_repos owner =
  let t = Lazy.force db in
  Log.info (fun f -> f "@[<h>Index.list_repos %s@]" owner);
  Db.query t.list_repos Sqlite3.Data.[ TEXT owner ]
  |> List.map @@ function
  | Sqlite3.Data.[ TEXT x ] -> x
  | _ -> failwith "list_repos: invalid data returned!"

module Account_set = Set.Make(String)
module Repo_map = Map.Make(Current_github.Repo_id)

let active_accounts = ref Account_set.empty
let set_active_accounts x = active_accounts := x
let get_active_accounts () = !active_accounts

let active_refs : (string * string) list Repo_map.t ref = ref Repo_map.empty

let set_active_refs ~repo (refs : (string * string) list) =
  (* Remove statuses from Commit_info_cache corresponding to removed refs *)
  Repo_map.find_opt repo !active_refs
  |> Option.iter (fun old_refs ->
    (* Set difference: find removed refs that have been merged or closed *)
    let removed_refs =
      List.filter
        (fun k -> not @@ List.mem k refs)
        old_refs
    in
    Metrics.inc_n_handled (List.length removed_refs);
    List.iter (fun (_, hash) ->
      Commit_info_cache.remove
        ~owner:repo.Current_github.Repo_id.owner
        ~name:repo.name ~hash)
        removed_refs);
  active_refs := Repo_map.add repo refs !active_refs

let get_active_refs repo =
  Repo_map.find_opt repo !active_refs |> Option.value ~default:[]

let get_jobs_per_ref repo =
  let active_refs = get_active_refs repo in
  Commit_info_cache.get_jobs_per_ref
    ~owner:repo.Current_github.Repo_id.owner
    ~name:repo.name
    active_refs
