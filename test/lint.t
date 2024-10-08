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
  Error "Warning in system-b.0.0.1: package name has restricted prefix 'system-'
  Error in system-b.0.0.1: package with prefix 'system-' requires conflict class 'ocaml-system'
  Error in system-b.0.0.1: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer
  Error in b.0.0.3: package with conflict class 'ocaml-host-arch' requires name prefix 'host-arch-'
  Error in b.0.0.3: pin-depends present. This is not allowed in the opam-repository.
  Error in b.0.0.3: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer
  Error in b.0.0.2: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer
  Error in b.0.0.2:              error  3: File format error in 'unknown-field' at line 11, column 0: Invalid field unknown-field
  Error in b.0.0.1: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer
  Error in b.0.0.1:            warning 25: Missing field 'authors'"

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
  Error "Warning in a_1.0.0.1: Possible name collision with package 'a-1'
  Error in a_1.0.0.1: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer"

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
  Error "Error in field1.0.0.2: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer
  Warning in fielf.0.0.1: Possible name collision with package 'field1'
  Error in fielf.0.0.1: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer
  Warning in fieffinder.0.0.1: Possible name collision with package 'fieffind'
  Error in fieffinder.0.0.1: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer
  Warning in fieffind.0.0.1: Possible name collision with package 'fieldfind'
  Warning in fieffind.0.0.1: Possible name collision with package 'fieffinder'
  Error in fieffind.0.0.1: Maintainer email missing. Please add a maintainer email to the opam file. Maintainer: Maintainer"
