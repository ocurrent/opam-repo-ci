let main dump_specs =
  Result.ok @@
  Lwt_main.run @@
  if dump_specs then
    Lwt.bind (Dump.v ()) (fun _ -> Lwt.return_unit)
  else
    Alcotest_lwt.run "opam-repo-ci"
      [ ("index", Test_index.tests) ]

open Cmdliner

let dump_specs =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"The path of the local Git repository to monitor"
    ~docv:"DUMP_SPECS"
    ["dump-specs"]

let cmd =
  let doc = "Test opam-repo-ci on a local Git repository" in
  let info = Cmd.info "opam-repo-ci-local" ~doc in
  Cmd.v info
    Term.(term_result (const main $ dump_specs))

let () = exit @@ Cmd.eval cmd
