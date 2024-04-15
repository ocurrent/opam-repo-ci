module S = Opam_repo_ci.Status_tree

let status_tree =
  let state f t = Fmt.pf f "%a" (S.pp Fmt.int) t in
  Alcotest.testable state (S.equal Int.equal)

let test_with_revdeps () =
  let s = [
    (S.Leaf ("(analysis)", 0));
    (S.Leaf ("(lint)", 0));
    (S.Branch ("compilers", None, [
      (S.Branch ("5.1", None, [
        (S.Branch ("a-1.0.0.3", (Some 0), [
          (S.Leaf ("lower-bounds", 0));
          (S.Branch ("revdeps", None, [
            S.Leaf ("b-1.0.0.1", 0);
            S.Leaf ("b-1.0.0.2", 0);
            S.Leaf ("b-1.0.0.3", 0);
          ]))
        ]))
      ]))
    ]))
  ] in
  let revdeps, main =
    S.partition (String.equal "revdeps") s
  in
  let revdeps_expected = [
    (S.Branch ("compilers", None, [
      (S.Branch ("5.1", None, [
        (S.Branch ("a-1.0.0.3", (Some 0), [
          (S.Branch ("revdeps", None, [
            S.Leaf ("b-1.0.0.1", 0);
            S.Leaf ("b-1.0.0.2", 0);
            S.Leaf ("b-1.0.0.3", 0);
          ]))
        ]))
      ]))
    ]))
  ] in
  Alcotest.(check status_tree) "Revdeps" revdeps revdeps_expected;
  let main_expected = [
    (S.Leaf ("(analysis)", 0));
    (S.Leaf ("(lint)", 0));
    (S.Branch ("compilers", None, [
      (S.Branch ("5.1", None, [
        (S.Branch ("a-1.0.0.3", (Some 0), [
          (S.Leaf ("lower-bounds", 0))
        ]))
      ]))
    ]))
  ] in
  Alcotest.(check status_tree) "Main" main main_expected

let test_no_revdeps () =
  let s = [
    (S.Leaf ("(analysis)", 0));
    (S.Leaf ("(lint)", 0));
    (S.Branch ("compilers", None, [
      (S.Branch ("5.1", None, [
        (S.Branch ("a-1.0.0.3", (Some 0), [
          (S.Leaf ("lower-bounds", 0))
        ]))
      ]))
    ]))
  ] in
  let revdeps, main =
    S.partition (String.equal "revdeps") s
  in
  let revdeps_expected = [] in
  Alcotest.(check status_tree) "Revdeps" revdeps revdeps_expected;
  let main_expected = [
    (S.Leaf ("(analysis)", 0));
    (S.Leaf ("(lint)", 0));
    (S.Branch ("compilers", None, [
      (S.Branch ("5.1", None, [
        (S.Branch ("a-1.0.0.3", (Some 0), [
          (S.Leaf ("lower-bounds", 0))
        ]))
      ]))
    ]))
  ] in
  Alcotest.(check status_tree) "Main" main main_expected

let tests = [
    Alcotest_lwt.test_case_sync "with-revdeps" `Quick test_with_revdeps;
    Alcotest_lwt.test_case_sync "no-revdeps" `Quick test_no_revdeps;
  ]
