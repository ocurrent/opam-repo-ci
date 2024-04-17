Tests linting of correctly formatted opam packages

  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch-1
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ git log --graph --pretty=format:'%s%d'
  * b-correct (HEAD -> new-branch-1)
  * a-1 (master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --lint-only --no-web-server
  Ok ()

Reset commit and clear build cache

  $ git reset -q --hard HEAD~1
  $ rm -rf var

Tests the following:
- [b.0.0.1] is missing the [author] field
- [b.0.0.2] has an extra unknown field
- [b.0.0.3] is correct

  $ git apply "patches/b-incorrect-opam.patch"
  $ git add .
  $ git commit -qm b-incorrect-opam
  $ git log --graph --pretty=format:'%s%d'
  * b-incorrect-opam (HEAD -> new-branch-1)
  * a-1 (master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --lint-only --no-web-server
  Error "2 errors:
  Error in b.0.0.1:            warning 25: Missing field 'authors'
  Error in b.0.0.2:              error  3: File format error in 'unknown-field' at line 11, column 0: Invalid field unknown-field"

Reset commit and clear build cache

  $ git reset -q --hard HEAD~1
  $ rm -rf var

Tests the package name collision detection by adding four versions
of a package [a_1] that conflicts with the existing [a-1] package

  $ git apply "patches/a_1-name-collision.patch"
  $ git add .
  $ git commit -qm a_1-name-collision
  $ git log --graph --pretty=format:'%s%d'
  * a_1-name-collision (HEAD -> new-branch-1)
  * a-1 (master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --lint-only --no-web-server
  Error "Warning in a_1.0.0.1: Possible name collision with package 'a-1'"

Delete OCurrent cache

  $ rm -rf var/

Tests the package name collisions detection by adding initial
packages [field] and [fieldfind] to master, and new packages
[fielf], [fielffind], and [fielffinder] to the new branch to
test various positive and negative cases

  $ git checkout -q master
  $ git apply "patches/levenshtein-1.patch"
  $ git add .
  $ git commit -qm levenshtein-1
  $ git checkout -qb new-branch-2
  $ git apply "patches/levenshtein-2.patch"
  $ git add .
  $ git commit -qm levenshtein-2
  $ git log --graph --pretty=format:'%s%d'
  * levenshtein-2 (HEAD -> new-branch-2)
  * levenshtein-1 (master)
  * a-1
  $ opam-repo-ci-local --repo="." --branch=new-branch-2 --lint-only --no-web-server
  Error "3 errors:
  Warning in fieffind.0.0.1: Possible name collision with package 'fieffinder'
  Warning in fieffind.0.0.1: Possible name collision with package 'fieldfind'
  Warning in fieffinder.0.0.1: Possible name collision with package 'fieffind'"
