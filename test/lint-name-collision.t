Tests the package name collision detection by adding four versions
of a package [a_1] that conflicts with the existing [a-1] package

  $ sh "scripts/setup_repo.sh"
  $ git apply "patches/a_1-name-collision.patch"
  $ git add .
  $ git commit -qm a_1-name-collision
  $ opam-repo-ci-local --repo="." --branch=new-branch --lint-only --no-web-server
  Error "4 errors:
  Warning in a_1.0.1.1: Possible name collision with package 'a-1'
  Warning in a_1.0.1.0: Possible name collision with package 'a-1'
  Warning in a_1.0.0.2: Possible name collision with package 'a-1'
  Warning in a_1.0.0.1: Possible name collision with package 'a-1'"
