Tests linting of correctly formatted opam packages

  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch-1
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ git log --graph --pretty=format:'%s%d'
  * b-correct (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --lint-only --no-web-server
  Error "Error in b.0.0.3: No package source directory provided.
  Error in b.0.0.2: No package source directory provided.
  Error in b.0.0.1: No package source directory provided."

Reset commit and clear build cache

  $ git reset -q --hard HEAD~1
  $ rm -rf var

Tests the following:
- [b.0.0.1] is missing the [author] field
- [b.0.0.2] has an extra unknown field
- [b.0.0.3] has a pin-depends present, and a conflict class without the required prefix
- [system-b.0.0.1] is using a restricted prefix in its name

  $ git apply "patches/b-incorrect-opam.patch"
  $ git add .
  $ git commit -qm b-incorrect-opam
  $ git log --graph --pretty=format:'%s%d'
  * b-incorrect-opam (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --lint-only --no-web-server
  Error "Warning in system-b.0.0.1: package name has restricted prefix 'system-'
  Error in system-b.0.0.1: package with prefix 'system-' requires conflict class 'ocaml-system'
  Error in system-b.0.0.1: No package source directory provided.
  Error in b.0.0.3: package with conflict class 'ocaml-host-arch' requires name prefix 'host-arch-'
  Error in b.0.0.3: pin-depends present. This is not allowed in the opam-repository.
  Error in b.0.0.3: No package source directory provided.
  Error in b.0.0.2:              error  3: File format error in 'unknown-field' at line 11, column 0: Invalid field unknown-field
  Error in b.0.0.2: No package source directory provided.
  Error in b.0.0.1:            warning 25: Missing field 'authors'
  Error in b.0.0.1: No package source directory provided."
