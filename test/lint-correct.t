  $ sh "scripts/setup_repo.sh"
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ opam-repo-ci-local --repo="." --branch=new-branch --lint-only --port=8080
  Ok ()
