(lang dune 3.15)

(name opam-ci-check)

(generate_opam_files true)

(source
 (github ocurrent/opam-ci-check))

(authors
 "Puneeth Chaganti <punchagan@muse-amuse.in>"
 "Shon Feder <shon.feder@gmail.com>")

(maintainers
 "Puneeth Chaganti <punchagan@muse-amuse.in>"
 "Shon Feder <shon.feder@gmail.com>")

; TODO: Add a license field
;(license LICENSE)

(documentation
 https://www.ocurrent.org/opam-ci-check/opam-ci-check/index.html)

(package
 (name opam-ci-check)
 (synopsis
  "CLI tool that tests whether packages are publishable on the opam repository.")
 (description
  "opam-ci-check is used in the opam repo CI and can be used locally and in other CI pipelines to test the deliverability of packages.")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  sexplib
  (cmdliner
   (>= 1.1.1))
  (opam-client
   (>= 2.2))
  (mula
   (>= 0.1.2)))
 (tags
  ("opam" "ci" "lint" "reverse dependency")))
