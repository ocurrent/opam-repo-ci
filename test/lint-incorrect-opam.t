Tests the following:
- [b.0.0.1] is missing the [author] field
- [b.0.0.2] has an extra unknown field
- [b.0.0.3] is correct

  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch
  $ git apply "patches/b-incorrect-opam.patch"
  $ git add .
  $ git commit -qm b-incorrect-opam
  $ opam-repo-ci-local --repo="." --branch=new-branch --lint-only --no-web-server
  Error "2 errors:
  Error in b.0.0.2:              error  3: File format error in 'unknown-field' at line 11, column 0: Invalid field unknown-field
  Error in b.0.0.1:            warning 25: Missing field 'authors'"
