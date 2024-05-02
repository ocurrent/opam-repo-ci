(lang dune 3.15)

(name revdeps_prototype)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name revdeps_prototype)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (opam-client
   (>= 2.2)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
