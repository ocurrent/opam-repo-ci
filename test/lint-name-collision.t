Tests the package name collision detection by adding four versions
of a package [a_1] that conflicts with the existing [a-1] package

  $ sh "scripts/setup_repo.sh"
  $ git apply "patches/a_1-name-collision.patch"
  $ git add .
  $ git commit -qm a_1-name-collision
  $ opam-repo-ci-local --repo="." --branch=new-branch --lint-only --port=8082
  Error "4 errors"
