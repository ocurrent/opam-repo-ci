let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "opam-repo-ci"
       [ ("index", Test_index.tests) ]
