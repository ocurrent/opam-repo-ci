Tests the package name collision detection by adding four versions
of a package [a_1] that conflicts with the existing [a-1] package

  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch-1
  $ git apply "patches/a_1-name-collision.patch"
  $ git add .
  $ git commit -qm a_1-name-collision
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --lint-only --no-web-server
  Error "4 errors:
  Warning in a_1.0.1.1: Possible name collision with package 'a-1'
  Warning in a_1.0.1.0: Possible name collision with package 'a-1'
  Warning in a_1.0.0.2: Possible name collision with package 'a-1'
  Warning in a_1.0.0.1: Possible name collision with package 'a-1'"

Delete OCurrent cache

  $ rm -rf var/

Adds initial packages [field] and [fieldfind] to master, and new
packages [fielf], [fielffind], and [fielffinder] to the new branch
to test various positive and negative cases

  $ git checkout -q master
  $ git apply "patches/levenshtein-1.patch"
  $ git add .
  $ git commit -qm levenshtein-1
  $ git checkout -qb new-branch-2
  $ git apply "patches/levenshtein-2.patch"
  $ git add .
  $ git commit -qm levenshtein-2
  $ opam-repo-ci-local --repo="." --branch=new-branch-2 --lint-only --no-web-server
  Error "4 errors:
  Warning in fielf.0.0.1: Possible name collision with package 'field'
  Warning in fieffinder.0.0.1: Possible name collision with package 'fieffind'
  Warning in fieffind.0.0.1: Possible name collision with package 'fieffinder'
  Warning in fieffind.0.0.1: Possible name collision with package 'fieldfind'"
