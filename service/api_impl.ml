module Rpc = Current_rpc.Impl(Current)
module Raw = Opam_repo_ci_api.Raw

module String_map = Map.Make(String)
module Index = Opam_repo_ci.Index

open Capnp_rpc_lwt

let make_commit ~engine ~owner ~name hash =
  let module Commit = Raw.Service.Commit in
  Commit.local @@ object
    inherit Commit.service

    method jobs_impl _params release_param_caps =
      let open Commit.Jobs in
      release_param_caps ();
      let jobs = Index.get_jobs ~owner ~name hash in
      let response, results = Service.Response.create Results.init_pointer in
      let arr = Results.jobs_init results (List.length jobs) in
      jobs |> List.iteri (fun i (variant, outcome) ->
          let slot = Capnp.Array.get arr i in
          Raw.Builder.JobInfo.variant_set slot variant;
          let state = Raw.Builder.JobInfo.state_init slot in
          let module S = Raw.Builder.JobInfo.State in
          match outcome with
          | `Not_started -> S.not_started_set state
          | `Passed -> S.passed_set state
          | `Aborted -> S.aborted_set state
          | `Active -> S.active_set state
          | `Failed msg -> S.failed_set state msg
        );
      Service.return response

    method job_of_variant_impl params release_param_caps =
      let open Commit.JobOfVariant in
      let variant = Params.variant_get params in
      release_param_caps ();
      match Index.get_job ~owner ~name ~hash ~variant with
      | Error `No_such_variant -> Service.fail "No such variant %S" variant
      | Ok None -> Service.fail "No job for variant %S yet" variant
      | Ok (Some id) ->
        let job = Rpc.job ~engine id in
        let response, results = Service.Response.create Results.init_pointer in
        Results.job_set results (Some job);
        Capability.dec_ref job;
        Service.return response

    method refs_impl _params release_param_caps =
      let open Commit.Refs in
      release_param_caps ();
      let refs =
        Index.get_active_refs { Current_github.Repo_id.owner; name }
        |> List.filter_map (fun (name, h) -> if h = hash then Some name else None)
      in
      let response, results = Service.Response.create Results.init_pointer in
      Results.refs_set_list results refs |> ignore;
      Service.return response

    method status_impl _params release_param_caps =
      let open Commit.Status in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Index.get_build_status ~owner ~name ~hash
      |> (function
          | `Not_started -> Results.status_set results NotStarted
          | `Pending -> Results.status_set results Pending
          | `Failed -> Results.status_set results Failed
          | `Passed -> Results.status_set results  Passed
         );
      Service.return response
  end

let make_repo ~engine ~owner ~name =
  let module Repo = Raw.Service.Repo in
  let commits = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_commit hash =
    match String_map.find_opt hash !commits with
    | Some x -> x
    | None ->
      let commit = make_commit ~engine ~owner ~name hash in
      commits := String_map.add hash commit !commits;
      commit
  in
  Repo.local @@ object
    inherit Repo.service

    method refs_impl _params release_param_caps =
      let open Repo.Refs in
      release_param_caps ();
      let refs = Index.get_active_refs { Current_github.Repo_id.owner; name } in
      let response, results = Service.Response.create Results.init_pointer in
      let arr = Results.refs_init results (List.length refs) in
      refs |> List.iteri (fun i (gref, hash) ->
          let slot = Capnp.Array.get arr i in
          Raw.Builder.RefInfo.ref_set slot gref;
          Raw.Builder.RefInfo.hash_set slot hash;
        );
      Service.return response

    method obsolete_refs_of_commit_impl _ release_param_caps =
      release_param_caps ();
      Service.fail "This method no longer exists"

    method commit_of_ref_impl params release_param_caps =
      let open Repo.CommitOfRef in
      let gref = Params.ref_get params in
      release_param_caps ();
      let refs = Index.get_active_refs { Current_github.Repo_id.owner; name } in
      match List.assoc_opt gref refs with
      | None -> Service.fail "@[<v2>Unknown ref %S. Options are:@,%a@]" gref
                  Fmt.(Dump.list string) (List.map fst refs)
      | Some hash ->
        let commit = get_commit hash in
        let response, results = Service.Response.create Results.init_pointer in
        Results.commit_set results (Some commit);
        Service.return response

    method commit_of_hash_impl params release_param_caps =
      let open Repo.CommitOfHash in
      let hash = Params.hash_get params in
      release_param_caps ();
      match Index.get_full_hash ~owner ~name hash with
      | Error `Ambiguous -> Service.fail "Ambiguous commit hash %S" hash
      | Error `Invalid -> Service.fail "Invalid Git hash %S" hash
      | Error `Unknown -> Service.fail "Unknown Git hash %S" hash
      | Ok hash ->
        let commit = get_commit hash in
        let response, results = Service.Response.create Results.init_pointer in
        Results.commit_set results (Some commit);
        Service.return response

    method obsolete_job_of_commit_impl _ release_param_caps =
      release_param_caps ();
      Service.fail "This method no longer exists"

    method obsolete_job_of_ref_impl _ release_param_caps =
      release_param_caps ();
      Service.fail "This method no longer exists"
  end

let make_org ~engine owner =
  let module Org = Raw.Service.Org in
  let repos = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_repo name =
    match String_map.find_opt name !repos with
    | Some repo -> Some repo
    | None ->
      if Index.is_known_repo ~owner ~name then (
        let repo = make_repo ~engine ~owner ~name in
        repos := String_map.add name repo !repos;
        Some repo
      ) else None
  in
  Org.local @@ object
    inherit Org.service

    method repo_impl params release_param_caps =
      let open Org.Repo in
      let name = Params.name_get params in
      release_param_caps ();
      match get_repo name with
      | None -> Service.fail "Invalid GitHub repo %S/%S" owner name
      | Some repo ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.repo_set results (Some repo);
        Service.return response

    method repos_impl _params release_param_caps =
      let open Org.Repos in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.repos_set_list results (Index.list_repos owner) |> ignore;
      Service.return response
  end

let make_ci ~engine =
  let module CI = Raw.Service.CI in
  let orgs = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_org owner =
    match String_map.find_opt owner !orgs with
    | Some org -> Some org
    | None ->
      if Index.Account_set.mem owner (Index.get_active_accounts ()) then (
        let org = make_org ~engine owner in
        orgs := String_map.add owner org !orgs;
        Some org
      ) else None
  in
  CI.local @@ object
    inherit CI.service

    method org_impl params release_param_caps =
      let open CI.Org in
      let owner = Params.owner_get params in
      release_param_caps ();
      match get_org owner with
      | None -> Service.fail "Invalid GitHub owner %S" owner
      | Some org ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.org_set results (Some org);
        Service.return response

    method orgs_impl _params release_param_caps =
      let open CI.Orgs in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let owners = Index.get_active_accounts () |> Index.Account_set.elements in
      Results.orgs_set_list results owners |> ignore;
      Service.return response
  end
