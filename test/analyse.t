  $ sh "scripts/setup_repo.sh"
  $ git apply "patches/b-correct.patch"
  $ git add .
  $ git commit -qm b-correct
  $ opam-repo-ci-local --repo="." --branch=new-branch --analyse-only
  {
    "packages": [
      [ "b.0.0.1", { "kind": [ "New" ], "has_tests": false } ],
      [ "b.0.0.2", { "kind": [ "New" ], "has_tests": false } ],
      [ "b.0.0.3", { "kind": [ "New" ], "has_tests": false } ]
    ]
  }
