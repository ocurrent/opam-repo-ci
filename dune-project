(lang dune 2.0)

(name opam-repo-ci)

(generate_opam_files true)

(source (github ocurrent/opam-repo-ci))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name opam-repo-ci-api)
 (synopsis "Cap'n Proto API for opam-repo-ci")
 (depends
  (dune (>= "1.11"))
  current_rpc
  (capnp (>= 3.4.0))
  capnp-rpc-lwt
  dockerfile))

(package
 (name opam-repo-ci-service)
 (synopsis "Test OCaml projects on GitHub")
 (conflicts (ocaml-migrate-parsetree (= "1.7.1")))
 (depends
  (dune (>= "1.11"))
  current_git
  current_github
  current_docker
  current_web
  current_rpc
  (capnp-rpc-unix (>= 0.5.0))
  opam-repo-ci-api
  (routes (< 0.8.0))
  (opam-format (>= 2.0.0))
  conf-libev
  (dockerfile (>= 6.3.0))
  (dockerfile-opam (>= 6.3.0))
  (ocaml-version (>= 2.4.0))
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))
))

(package
 (name opam-repo-ci-web)
 (synopsis "Web-server frontend for opam-repo-ci")
 (conflicts (ocaml-migrate-parsetree (= "1.7.1")))
 (depends
  (dune (>= "1.11"))
  current_rpc
  current_ansi
  prometheus-app
  cmdliner
  lwt
  (cohttp-lwt-unix (>= 2.2.0))
  tyxml
  capnp-rpc-unix
  opam-repo-ci-api
  conf-libev
  dockerfile
  ppx_deriving_yojson
))

(package
 (name opam-repo-ci-client)
 (synopsis "Command-line client for opam-repo-ci")
 (depends
  (dune (>= "1.11"))
  current_rpc
  capnp-rpc-unix
  opam-repo-ci-api
  dockerfile))

(formatting disabled)
