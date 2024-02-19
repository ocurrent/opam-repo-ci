let () =
  Logs.set_level (Some Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "opam-repo-ci" [
      ("index", Test_index.tests);
      ("lint", Test_lint.tests);
    ]
