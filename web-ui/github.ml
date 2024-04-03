open Lwt.Infix

module Capability = Capnp_rpc_lwt.Capability
module Client = Opam_repo_ci_api.Client
module Common = Opam_repo_ci_api.Common
module Status_tree = Opam_repo_ci.Status_tree
module Server = Cohttp_lwt_unix.Server
module Response = Cohttp.Response.Make(Server.IO)
module Transfer_IO = Cohttp__Transfer_io.Make(Server.IO)

let headers = Cohttp.Header.init_with "Content-Type" "text/html; charset=utf-8"

let normal_response x =
  x >|= fun x -> `Response x

let respond_error status body =
  Server.respond_error ~status ~headers ~body () |> normal_response

let (>>!=) x f =
  x >>= function
  | Error `Capnp ex -> respond_error `Internal_server_error (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Error `Msg msg -> respond_error `Internal_server_error msg
  | Ok y -> f y

let org_url owner =
  Printf.sprintf "/github/%s" owner

let repo_url ~owner name =
  Printf.sprintf "/github/%s/%s" owner name

let job_url ~owner ~name ~hash variant =
  Printf.sprintf "/github/%s/%s/commit/%s/variant/%s" owner name hash variant

let commit_url ~owner ~name hash =
  Printf.sprintf "/github/%s/%s/commit/%s" owner name hash

let github_branch_url ~owner ~name ref =
  Printf.sprintf "https://github.com/%s/%s/tree/%s" owner name ref

let github_pr_url ~owner ~name id =
  Printf.sprintf "https://github.com/%s/%s/pull/%s" owner name id

let css = {|
  .statuses {
    list-style: none;
  }
  .statuses > li:before {
    display: inline-block;
    width: 1em;
    margin-right: 0.5em;
    margin-left: -1.5em;
    text-align: center;
    line-height: 1.1em;
  }
  .statuses > li.not-started:before { content: "●"; color:grey; }
  .statuses > li.aborted:before { content: "A"; color:red; }
  .statuses > li.failed:before { content: "×"; color:red; }
  .statuses > li.passed:before { content: "✓"; color:green; }
  .statuses > li.active:before { content: "●"; color:orange; }
  .statuses > li.undefined:before { content: "?"; color:grey; }
  .statuses > li.skipped:before { content: "–"; color:grey; }
  .statuses > li.none:before { content: "○"; }
|}

let breadcrumbs steps page_title =
  let open Tyxml.Html in
  let add (prefix, results) (label, link) =
    let prefix = Printf.sprintf "%s/%s" prefix link in
    let link = li [a ~a:[a_href prefix] [txt label]] in
    (prefix, link :: results)
  in
  let _, steps = List.fold_left add ("", []) steps in
  let steps = li [b [txt page_title]] :: steps in
  ol ~a:[a_class ["breadcrumbs"]] (
    List.rev steps
  )

let format_refs ~owner ~name refs =
  let open Tyxml.Html in
  ul (
    Client.Ref_map.to_seq refs |> List.of_seq |> List.map @@ fun (branch, commit) ->
    li [a ~a:[a_href (commit_url ~owner ~name commit)] [txt branch]]
  )

let rec intersperse ~sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: intersperse ~sep xs

let link_github_refs ~owner ~name =
  let open Tyxml.Html in
  function
  | [] -> txt "(not at the head of any monitored branch or PR)"
  | refs ->
    p (
      txt "(for " ::
      (
        intersperse ~sep:(txt ", ") (
          refs |> List.map @@ fun r ->
          match Astring.String.cuts ~sep:"/" r with
          | "refs"::"heads"::branch ->
            let branch = String.concat "/" branch in
            span [txt "branch "; a ~a:[a_href (github_branch_url ~owner ~name branch)] [ txt branch ]]
          | ["refs"; "pull"; id; "head"] ->
            span [txt "PR "; a ~a:[a_href (github_pr_url ~owner ~name id)] [ txt ("#" ^ id) ]]
          | _ ->
            txt (Printf.sprintf "Bad ref format %S" r)
        )
      ) @
      [txt ")"]
    )

let is_error = function
  | { Client.outcome = Failed msg; _ } -> not (Status_tree.is_skip msg)
  | _ -> false

let show_status =
  let open Tyxml.Html in
  function
  | `Not_started -> [p [txt "Job not yet started"]]
  | `Pending -> [p [txt "Testing in progress..."]]
  | `Passed -> [p ~a:[a_class ["ok"]] [txt "Passed"]]
  | `Failed -> []

let link_jobs ~owner ~name ~hash ?selected jobs =
  let open Tyxml.Html in
  let errors = List.filter is_error jobs in
  let render_job trees { Client.variant; outcome } =
    let uri = job_url ~owner ~name ~hash variant in
    let k = String.split_on_char Common.status_sep variant in
    let x =
      let label_txt = List.hd (List.rev k) in
      let label = txt (Fmt.str "%s (%a)" label_txt Client.State.pp outcome) in
      let label = if selected = Some variant then b [label] else label in
      outcome, [a ~a:[a_href uri] [label]]
    in
    Status_tree.add k x trees
  in
  let status_tree = List.fold_left render_job [] jobs in
  let revdeps_tree, main_tree =
    Status_tree.partition (String.equal "revdeps") status_tree
  in
  let error_tree =
    if errors = [] then []
    else [
      h2 [txt "Summary of errors"];
      Status_tree.render (List.fold_left render_job [] errors);
    ]
  in
  error_tree @ [
    details
      (summary [b [txt "Main results"]])
      [Status_tree.render main_tree];
    details
      (summary [b [txt "Reverse dependencies"]])
      [Status_tree.render revdeps_tree];
  ]

let short_hash = Astring.String.with_range ~len:6

let stream_logs job ~owner ~name ~refs ~hash ~variant ~status (data, next) writer =
  let header, footer =
    let can_rebuild = status.Current_rpc.Job.can_rebuild in
    let buttons =
      if can_rebuild then Tyxml.Html.[
          form ~a:[a_action (variant ^ "/rebuild"); a_method `Post] [
            input ~a:[a_input_type `Submit; a_value "Rebuild"] ()
          ]
        ] else []
    in
    let body = Template.instance Tyxml.Html.[
        breadcrumbs ["github", "github";
                     owner, owner;
                     name, name;
                     short_hash hash, "commit/" ^ hash;
                    ] variant;
        link_github_refs ~owner ~name refs;
        div buttons;
        pre [txt "@@@"]
      ] in
    Astring.String.cut ~sep:"@@@" body |> Option.get
  in
  let ansi = Current_ansi.create () in
  Transfer_IO.write writer (header ^ (Current_ansi.process ansi data)) >>= fun () ->
  let rec aux next =
    Current_rpc.Job.log job ~start:next >>= function
    | Ok ("", _) ->
      Transfer_IO.write writer footer
    | Ok (data, next) ->
      Transfer_IO.write writer (Current_ansi.process ansi data) >>= fun () ->
      aux next
    | Error (`Capnp ex) ->
      Log.warn (fun f -> f "Error fetching logs: %a" Capnp_rpc.Error.pp ex);
      Transfer_IO.write writer (Fmt.str "opam-repo-ci error: %a@." Capnp_rpc.Error.pp ex)
  in
  aux next

let can_cancel job_info =
  match job_info.Client.outcome with
  | Active | NotStarted -> true
  | Aborted | Failed _ | Passed | Undefined _ -> false

let can_rebuild job_info =
  match job_info.Client.outcome with
  | Aborted -> true
  | Failed m when not (Status_tree.is_skip m) -> true
  | Failed _ | Active | NotStarted | Passed | Undefined _ -> false

module Repo_handle = struct
  let get ~owner ~name ~repo =
    Client.Repo.refs repo >>!= fun refs ->
    let body = Template.instance [
        breadcrumbs ["github", "github";
                      owner, owner] name;
        format_refs ~owner ~name refs
      ] in
    Server.respond_string ~status:`OK ~body () |> normal_response

  let get_commit ~owner ~name ~repo hash =
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
    let refs = Client.Commit.refs commit in
    let commit_status = Client.Commit.status commit in
    Client.Commit.jobs commit >>!= fun jobs ->
    refs >>!= fun refs ->
    commit_status >>!= fun commit_status ->
    let can_cancel = List.exists can_cancel jobs in
    let can_rebuild = List.exists can_rebuild jobs in
    let buttons = Tyxml.Html.[
      form ~a:[a_action (hash ^ "/rebuild-all"); a_method `Post] [
        input ~a:[a_input_type `Submit; a_value "Rebuild All"] ()
      ]
    ] in
    let buttons =
      if can_cancel then Tyxml.Html.(
          form ~a:[a_action (hash ^ "/cancel"); a_method `Post] [
            input ~a:[a_input_type `Submit; a_value "Cancel"] ()
          ]
      ) :: buttons else buttons
    in
    let buttons =
      if can_rebuild then Tyxml.Html.(
          form ~a:[a_action (hash ^ "/rebuild-failed"); a_method `Post] [
            input ~a:[a_input_type `Submit; a_value "Rebuild Failed"] ()
          ]
      ) :: buttons else buttons
    in
    let body = Template.instance Tyxml.Html.([
        breadcrumbs ["github", "github";
                      owner, owner;
                      name, name] (short_hash hash);
        link_github_refs ~owner ~name refs;
        div buttons;
      ] @ show_status commit_status @ link_jobs ~owner ~name ~hash jobs)
    in
    Server.respond_string ~status:`OK ~body () |> normal_response

  let get_variant ~owner ~name ~repo hash variant =
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
      let refs = Client.Commit.refs commit in
      Capability.with_ref (Client.Commit.job_of_variant commit variant) @@ fun job ->
      let status = Current_rpc.Job.status job in
      Current_rpc.Job.log job ~start:0L >>!= fun chunk ->
      (* (these will have resolved by now) *)
      refs >>!= fun refs ->
      status >>!= fun status ->
      let headers =
        (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
        Cohttp.Header.init_with "X-Accel-Buffering" "no"
      in
      let res = Cohttp.Response.make ~status:`OK ~flush:true ~encoding:Cohttp.Transfer.Chunked ~headers () in
      let write _ic oc =
        let flush = Cohttp.Response.flush res in
        let writer = Transfer_IO.make_writer ~flush Cohttp.Transfer.Chunked oc in
        Lwt.finalize
          (fun () ->
             stream_logs job ~owner ~name ~refs ~hash ~variant ~status chunk writer >>= fun () ->
             Server.IO.write oc "0\r\n\r\n"
          )
          (fun () ->
             Capability.dec_ref job;
             Lwt.return_unit
          )
      in
      Capability.inc_ref job;
      Lwt.return (`Expert (res, write))

  let post_rebuild ~owner ~name ~repo hash variant =
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
    Capability.with_ref (Client.Commit.job_of_variant commit variant) @@ fun job ->
    Capability.with_ref (Current_rpc.Job.rebuild job) @@ fun new_job ->
    Capability.await_settled new_job >>= function
    | Ok () ->
      let uri = job_url ~owner ~name ~hash variant |> Uri.of_string in
      Server.respond_redirect ~uri () |> normal_response
    | Error { Capnp_rpc.Exception.reason; _ } ->
      Server.respond_error ~body:reason () |> normal_response

  let post_rebuild_mode ~owner ~name ~repo hash rebuild_mode =
    let rebuild_all =
      match rebuild_mode with
      | "rebuild-all" -> true
      | "rebuild-failed" -> false
      | _ -> assert false
    in
    let is_a_job_triggering_other_jobs variant =
      (* TODO: Remove the (analysis) magic string *)
      (* TODO: Same for revdeps *)
      String.equal variant "(analysis)" ||
      Astring.String.is_suffix ~affix:",revdeps" variant
    in
    let can_rebuild (commit: Client.Commit.t) (job_i: Client.job_info) =
      if (rebuild_all && not (is_a_job_triggering_other_jobs job_i.Client.variant)) || can_rebuild job_i then
        let variant = job_i.variant in
        Capability.with_ref (Client.Commit.job_of_variant commit variant) @@ fun job ->
        Lwt.return (Some (job_i, job))
      else Lwt.return None
    in
    let rebuild_many (commit: Client.Commit.t) (job_infos : Client.job_info list) =
      Lwt_list.fold_left_s (fun (success, failed) job_info ->
        can_rebuild commit job_info >>= function
        | None -> Lwt.return (success, failed)
        | Some (ji, j) ->
            Capability.with_ref (Current_rpc.Job.rebuild j) @@ fun new_job ->
            Capability.await_settled new_job >|= function
            | Ok () -> (ji :: success, failed)
            | Error { Capnp_rpc.Exception.reason; _ } ->
                Log.err (fun f -> f "Error rebuilding job: %s" reason);
                (success, succ failed)
      ) ([], 0) job_infos
    in
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
    Client.Commit.refs commit >>!= fun refs ->
    Client.Commit.jobs commit >>!= fun jobs ->
    rebuild_many commit jobs >>= fun (success, failed) ->
    let open Tyxml.Html in
    let uri = commit_url ~owner ~name hash in
    let format_job_info ji =
      li [span [txt @@ Fmt.str "Rebuild job: %s" ji.Client.variant]]
    in
    let success_msg =
      match success with
      | [] -> div [span [txt "No jobs were rebuilt."]]
      | success -> ul (List.map format_job_info success)
    in
    let fail_msg = match failed with
      | 0 -> div []
      | n -> div [span [txt @@ Fmt.str "%d job%s could not be rebuilt. Check logs for more detail." n (if n >= 1 then "s" else "")]]
    in
    let return_link =
      a ~a:[a_href uri] [txt @@ Fmt.str "Return to %s" (short_hash hash)]
    in
    let body = Template.instance [
      breadcrumbs ["github", "github";
                    owner, owner;
                    name, name] (short_hash hash);
      link_github_refs ~owner ~name refs;
      success_msg;
      fail_msg;
      return_link;
    ] in
    Server.respond_string ~status:`OK ~headers ~body () |> normal_response

  let post_cancel ~owner ~name ~repo hash =
    let can_cancel (commit: Client.Commit.t) (job_i: Client.job_info) =
      if can_cancel job_i then
        let variant = job_i.variant in
        Capability.with_ref (Client.Commit.job_of_variant commit variant) @@ fun job ->
        Lwt.return (Some (job_i, job))
      else Lwt.return None
    in
    let cancel_many (commit: Client.Commit.t) (job_infos : Client.job_info list) =
      Lwt_list.fold_left_s (fun (success, failed) job_info ->
        can_cancel commit job_info >>= function
        | None -> Lwt.return (success, failed)
        | Some (ji, j) ->
            Current_rpc.Job.cancel j >|= function
            | Ok () -> (ji :: success, failed)
            | Error (`Capnp ex) ->
                Log.err (fun f -> f "Error cancelling job: %a" Capnp_rpc.Error.pp ex);
                (success, succ failed)
      ) ([], 0) job_infos
    in
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
    Client.Commit.refs commit >>!= fun refs ->
    Client.Commit.jobs commit >>!= fun jobs ->
    cancel_many commit jobs >>= fun (success, failed) ->
    let open Tyxml.Html in
    let uri = commit_url ~owner ~name hash in
    let format_job_info ji =
      li [span [txt @@ Fmt.str "Cancelling job: %s" ji.Client.variant]]
    in
    let success_msg =
      match success with
      | [] -> div [span [txt "No jobs were cancelled."]]
      | success -> ul (List.map format_job_info success)
    in
    let fail_msg = match failed with
      | 0 -> div []
      | n -> div [span [txt @@ Fmt.str "%d job%s could not be cancelled. Check logs for more detail." n (if n >= 1 then "s" else "")]]
    in
    let return_link =
      a ~a:[a_href uri] [txt @@ Fmt.str "Return to %s" (short_hash hash)]
    in
    let body = Template.instance [
        breadcrumbs ["github", "github";
                      owner, owner;
                      name, name] (short_hash hash);
        link_github_refs ~owner ~name refs;
        success_msg;
        fail_msg;
        return_link;
      ] in
    Server.respond_string ~status:`OK ~headers ~body () |> normal_response

  let v ~meth ~owner ~name ~repo path =
    match meth, path with
    | `GET, [] -> get ~owner ~name ~repo
    | `GET, ["commit"; hash] -> get_commit ~owner ~name ~repo hash
    | `GET, ["commit"; hash; "variant"; variant] -> get_variant ~owner ~name ~repo hash variant
    | `POST, ["commit"; hash; "variant"; variant; "rebuild"] -> post_rebuild ~owner ~name ~repo hash variant
    | `POST, ["commit"; hash; ("rebuild-all" as mode)]
    | `POST, ["commit"; hash; ("rebuild-failed" as mode)] -> post_rebuild_mode ~owner ~name ~repo hash mode
    | `POST, ["commit"; hash; "cancel"] -> post_cancel ~owner ~name ~repo hash
    | _ -> Server.respond_not_found () |> normal_response
end

let format_org org =
  let open Tyxml.Html in
  li [a ~a:[a_href (org_url org)] [txt org]]

let list_orgs ci =
  Client.CI.orgs ci >>!= fun orgs ->
  let body = Template.instance Tyxml.Html.[
      breadcrumbs [] "github";
      ul (List.map format_org orgs)
    ] in
  Server.respond_string ~status:`OK ~body () |> normal_response

let format_repo ~owner name =
  let open Tyxml.Html in
  li [a ~a:[a_href (repo_url ~owner name)] [txt name]]

let list_repos ~owner org =
  Client.Org.repos org >>!= fun repos ->
  let body = Template.instance Tyxml.Html.[
      breadcrumbs ["github", "github"] owner;
      ul (List.map (format_repo ~owner) repos)
    ] in
  Server.respond_string ~status:`OK ~body () |> normal_response

let handle ~backend ~meth path =
  Backend.ci backend >>= fun ci ->
  match meth, path with
  | `GET, [] -> list_orgs ci
  | `GET, [owner] ->
    Capability.with_ref (Client.CI.org ci owner) @@ list_repos ~owner
  | meth, (owner :: name :: path) ->
    Capability.with_ref (Client.CI.org ci owner) @@ fun org ->
    Capability.with_ref (Client.Org.repo org name) @@ fun repo ->
    Repo_handle.v ~meth ~owner ~name ~repo path
  | _ ->
    Server.respond_not_found () |> normal_response
