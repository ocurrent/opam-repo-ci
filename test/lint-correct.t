  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ opam-repo-ci-local --repo="." --branch=new-branch --lint-only --no-web-server
  Ok ()
