Test adding new packages

  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch-1
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --analyse-only --no-web-server
  {
    "packages": [
      [ "b.0.0.1", { "kind": [ "New" ], "has_tests": false } ],
      [ "b.0.0.2", { "kind": [ "New" ], "has_tests": false } ],
      [ "b.0.0.3", { "kind": [ "New" ], "has_tests": false } ]
    ]
  }

Reset commit and clear build cache

  $ git reset -q --hard HEAD~1
  $ rm -rf var

Test packages with insignificant changes

  $ git apply "patches/a-1-insignificant-change.patch"
  $ git add .
  $ git commit -qm a-1-modified
  $ opam-repo-ci-local --repo="." --branch=new-branch-1 --analyse-only --no-web-server
  {
    "packages": [
      [
        "a-1.0.0.1",
        { "kind": [ "UnsignificantlyChanged" ], "has_tests": false }
      ],
      [
        "a-1.0.0.2",
        { "kind": [ "UnsignificantlyChanged" ], "has_tests": false }
      ]
    ]
  }

Reset commit and clear build cache

  $ git reset -q --hard HEAD~1
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
  $ opam-repo-ci-local --repo="." --branch=new-branch-2 --analyse-only --no-web-server
  {
    "packages": [
      [ "b.0.0.1", { "kind": [ "SignificantlyChanged" ], "has_tests": false } ]
    ]
  }

Reset commits on [master] and [new-branch-2] and clear build cache

  $ git reset -q --hard HEAD~2
  $ git checkout -q master
  $ git reset -q --hard HEAD~1
  $ git checkout -q new-branch-2
  $ rm -rf var

Test adding new packages

  $ git apply "patches/a_1-name-collision.patch"
  $ git add .
  $ git commit -qm a_1-name-collision
  $ opam-repo-ci-local --repo="." --branch=new-branch-2 --analyse-only --no-web-server
  {
    "packages": [
      [ "a_1.0.0.1", { "kind": [ "New" ], "has_tests": false } ],
      [ "a_1.0.0.2", { "kind": [ "New" ], "has_tests": false } ],
      [ "a_1.0.1.0", { "kind": [ "New" ], "has_tests": false } ],
      [ "a_1.0.1.1", { "kind": [ "New" ], "has_tests": false } ]
    ]
  }
