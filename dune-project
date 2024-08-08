(lang dune 3.15)

(name opam-ci-check)

(generate_opam_files true)

(source
 (github ocurrent/opam-ci-check))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name opam-ci-check)
 (synopsis "A short synopsis")
 (description "A longer description")
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
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
