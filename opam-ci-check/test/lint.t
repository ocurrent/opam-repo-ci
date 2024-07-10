Setup test opam-repository directory

  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch-1

Tests linting of correctly formatted opam packages

  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ git log --graph --pretty=format:'%s%d'
  * b-correct (HEAD -> new-branch-1)
  * a-1 (master)
  $ revdeps_prototype lint -r . a-1.0.0.2
  Linting a-1.0.0.2 in $TESTCASE_ROOT/. ...
  No errors
  $ revdeps_prototype lint -r . b.0.0.3
  Linting b.0.0.3 in $TESTCASE_ROOT/. ...
  No errors

Setup repo for incorrect b package tests

  $ git reset -q --hard HEAD~1
  $ git apply "patches/b-incorrect-opam.patch"
  $ git add .
  $ git commit -qm b-incorrect-opam
  $ git log --graph --pretty=format:'%s%d'
  * b-incorrect-opam (HEAD -> new-branch-1)
  * a-1 (master)


Test the following:
- [b.0.0.1] is missing the [author] field
- [b.0.0.2] has an extra unknown field
- [b.0.0.3] has a pin-depends present, and a conflict class without the required prefix
- [system-b.0.0.1] is using a restricted prefix in its name

  $ revdeps_prototype lint -r . b.0.0.1
  Linting b.0.0.1 in $TESTCASE_ROOT/. ...
  Error in b.0.0.1:            warning 25: Missing field 'authors'
  [1]
  $ revdeps_prototype lint -r . b.0.0.2
  Linting b.0.0.2 in $TESTCASE_ROOT/. ...
  Error in b.0.0.2:              error  3: File format error in 'unknown-field' at line 11, column 0: Invalid field unknown-field
  [1]
  $ revdeps_prototype lint -r . b.0.0.3
  Linting b.0.0.3 in $TESTCASE_ROOT/. ...
  Error in b.0.0.3: pin-depends present. This is not allowed in the opam-repository.
  Error in b.0.0.3: package with conflict class 'ocaml-host-arch' requires name prefix 'host-arch-'
  [1]
  $ revdeps_prototype lint -r . system-b.0.0.1
  Linting system-b.0.0.1 in $TESTCASE_ROOT/. ...
  Error in system-b.0.0.1: package with prefix 'system-' requires conflict class 'ocaml-system'
  [1]

Setup repo for name collision tests

  $ git reset -q --hard HEAD~1
  $ git apply "patches/a_1-name-collision.patch"
  $ git add .
  $ git commit -qm a_1-name-collision
  $ git log --graph --pretty=format:'%s%d'
  * a_1-name-collision (HEAD -> new-branch-1)
  * a-1 (master)

Tests the package name collision detection by adding a version of a package
[a_1] that conflicts with the existing [a-1] package

  $ revdeps_prototype lint -r . --newly-published a_1.0.0.1
  Linting a_1.0.0.1 in $TESTCASE_ROOT/. ...
  Warning in a_1.0.0.1: Possible name collision with package 'a-1'
  [1]

Setup repo for more name collision tests

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

Tests the package name collisions detection by adding initial packages [field],
[field1] and [fieldfind] to master, and new packages [fielf], [fielffind], and
[fielffinder] to the new branch to test various positive and negative cases

  $ revdeps_prototype lint -r . --newly-published fielf.0.0.1
  Linting fielf.0.0.1 in $TESTCASE_ROOT/. ...
  Warning in fielf.0.0.1: Possible name collision with package 'field1'
  [1]
  $ revdeps_prototype lint -r . --newly-published field1.0.0.2
  Linting field1.0.0.2 in $TESTCASE_ROOT/. ...
  Warning in field1.0.0.2: Possible name collision with package 'fielf'
  [1]
  $ revdeps_prototype lint -r . --newly-published fieffinder.0.0.1
  Linting fieffinder.0.0.1 in $TESTCASE_ROOT/. ...
  Warning in fieffinder.0.0.1: Possible name collision with package 'fieffind'
  [1]
  $ revdeps_prototype lint -r . --newly-published fieffind.0.0.1
  Linting fieffind.0.0.1 in $TESTCASE_ROOT/. ...
  Warning in fieffind.0.0.1: Possible name collision with package 'fieffinder'
  Warning in fieffind.0.0.1: Possible name collision with package 'fieldfind'
  [1]
