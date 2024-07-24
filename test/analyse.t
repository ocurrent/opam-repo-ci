Test adding new packages

  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch-1
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ git log --graph --pretty=format:'%s%d'
  * b-correct (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --analyse-only --no-web-server
  {
    "packages": [
      [ "b.0.0.1", { "kind": [ "New" ], "has_tests": false } ],
      [ "b.0.0.2", { "kind": [ "New" ], "has_tests": false } ],
      [ "b.0.0.3", { "kind": [ "New" ], "has_tests": false } ]
    ]
  }

Reset commit and clear build cache

  $ git reset -q --hard initial-state
  $ rm -rf var

Test packages with insignificant changes

  $ git apply "patches/a-1-insignificant-change.patch"
  $ git add .
  $ git commit -qm a-1-modified
  $ git log --graph --pretty=format:'%s%d'
  * a-1-modified (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --analyse-only --no-web-server
  {
    "packages": [
      [
        "a-1.0.0.1",
        { "kind": [ "InsignificantlyChanged" ], "has_tests": false }
      ],
      [
        "a-1.0.0.2",
        { "kind": [ "InsignificantlyChanged" ], "has_tests": false }
      ]
    ]
  }

Reset commit and clear build cache

  $ git reset -q --hard initial-state
  $ rm -rf var

Test package with significant changes

  $ git checkout -q master
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-1-correct
  $ git checkout -qb new-branch-2
  $ git apply "patches/b-significant-change.patch"
  $ git add .
  $ git commit -qm a-1-modified
  $ git log --graph --pretty=format:'%s%d'
  * a-1-modified (HEAD -> new-branch-2)
  * b-1-correct (master)
  * a-1 (tag: initial-state, new-branch-1)
  $ opam-repo-ci-local --repo="." --branch=new-branch-2 --analyse-only --no-web-server
  {
    "packages": [
      [ "b.0.0.1", { "kind": [ "SignificantlyChanged" ], "has_tests": false } ]
    ]
  }

Reset commits on [master] and [new-branch-2] and clear build cache

  $ git reset -q --hard initial-state
  $ git checkout -q master
  $ git reset -q --hard initial-state
  $ git checkout -q new-branch-2
  $ rm -rf var

Test adding new packages

  $ git apply "patches/a_1-name-collision.patch"
  $ git add .
  $ git commit -qm a_1-name-collision
  $ git log --graph --pretty=format:'%s%d'
  * a_1-name-collision (HEAD -> new-branch-2)
  * a-1 (tag: initial-state, new-branch-1, master)
  $ opam-repo-ci-local --repo="." --branch=new-branch-2 --analyse-only --no-web-server
  {
    "packages": [ [ "a_1.0.0.1", { "kind": [ "New" ], "has_tests": false } ] ]
  }

Clean up the build cache

  $ rm -rf ./var

Syntactically invalid opam files are handled correctly.
See https://github.com/ocurrent/opam-repo-ci/issues/291

  $ git checkout -q -b analyze-invalid-opam-file initial-state
  $ sed 's/depends: \[\]/depends:\[ ocaml {>= 4.15} \]/' packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ git commit -q -m "Add invalid syntax in version constraint" packages/a-1/a-1.0.0.1/opam
  $ git diff initial-state | tail -n 3
   build: []
  -depends: []
  +depends:[ ocaml {>= 4.15} ]
  $ opam-repo-ci-local --repo="." --branch=analyze-invalid-opam-file --analyse-only --no-web-server
  Error ""packages/a-1/a-1.0.0.1/opam" failed to be parsed: '.' is not a valid token"

Clean up the build cache

  $ rm -rf ./var
