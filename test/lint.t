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
  Ok ()

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
  Error "Command "opam-ci-check" "lint" "--opam-repository" "." "--newly-published" 
  "b.0.0.1,b.0.0.2,b.0.0.3,system-b.0.0.1" exited with status 1"

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
  * a-1 (tag: initial-state, master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --lint-only --no-web-server
  Error "Command "opam-ci-check" "lint" "--opam-repository" "." "--newly-published" 
  "a_1.0.0.1" exited with status 1"

Delete OCurrent cache

  $ rm -rf var/

Tests the package name collisions detection by adding initial
packages [field], [field1] and [fieldfind] to master, and new packages
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
  * a-1 (tag: initial-state)
  $ opam-repo-ci-local --repo="." --branch=new-branch-2 --lint-only --no-web-server
  Error "Command "opam-ci-check" "lint" "--opam-repository" "." "--changed-packages" 
  "field1.0.0.2" "--newly-published" "fieffind.0.0.1,fieffinder.0.0.1,fielf.0.0.1" exited with status 1"
