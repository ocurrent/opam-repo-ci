  $ sh "scripts/setup_repo.sh"
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ git checkout -qb new-branch
  $ git apply "patches/a-1-update.patch"
  $ git add .
  $ git commit -qm a-1-update
  $ opam-repo-ci-local --repo="." --branch=new-branch --revdeps-only
  Ok ()
