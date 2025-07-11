# Error handling of CLI parsing

Test for an invalid package spec

  $ opam-ci-check lint -r . '=-~!:new=true'
  opam-ci-check: invalid value '=-~!', expected opam package spec in the form
                 <name.version>
  Usage: opam-ci-check lint [--checks=VAL] [--quiet] [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for an invalid attributes

Test for invalid attributes that are properly formed

  $ opam-ci-check lint -r . 'foo.0.1.0:bar=baz'
  opam-ci-check: invalid element in list ('bar=baz'): bar=baz is an not a valid
                 attribute. Only [src=<path>] or [new=<true|false>] allowed
  Usage: opam-ci-check lint [--checks=VAL] [--quiet] [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for an invalid values to a valid key

  $ opam-ci-check lint -r . 'foo.0.1.0:new=invalid'
  opam-ci-check: invalid element in list ('new=invalid'): invalid must be
                 [true] or [false]
  Usage: opam-ci-check lint [--checks=VAL] [--quiet] [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for a missing value

  $ opam-ci-check lint -r . 'foo.0.1.0:src='
  opam-ci-check: invalid element in list ('src='): src= is an not a valid
                 attribute. Only [src=<path>] or [new=<true|false>] allowed
  Usage: opam-ci-check lint [--checks=VAL] [--quiet] [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for a valid key with no value

  $ opam-ci-check lint -r . 'foo.0.1.0:src'
  opam-ci-check: invalid element in list ('src'): src is an not a valid
                 attribute. Only [src=<path>] or [new=<true|false>] allowed
  Usage: opam-ci-check lint [--checks=VAL] [--quiet] [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for `src` with a non-existent directory

  $ opam-ci-check lint -r . 'foo.0.1.0:src=./not/a/dir'
  opam-ci-check: invalid element in list ('src=./not/a/dir'): ./not/a/dir: No
                 such file or directory
  Usage: opam-ci-check lint [--checks=VAL] [--quiet] [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for invalid extra colons

  $ opam-ci-check lint -r . 'foo.0.1.0:bing-bong:bang'
  opam-ci-check: Invalid argument spec foo.0.1.0:bing-bong:bang. Argument specs
                 should be of the form arg[:k1=v1[,k2=v2]]
  Usage: opam-ci-check lint [--checks=VAL] [--quiet] [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

# Setup test opam-repository directory

  $ sh "scripts/setup_repo.sh"
  $ git checkout -qb new-branch-1

# Test linting

Tests linting of correctly formatted opam packages

  $ git apply "patches/b.0.0.1-correct.patch"
  $ echo "(lang dune 3.16)" > dune-project
  $ sh "scripts/setup_sources.sh" b 0.0.1 dune-project
  Created tarball b.0.0.1.tgz
  Updated checksum for b.0.0.1.tgz in b.0.0.1's opam file
  $ git add .
  $ git commit -qm b.0.0.1-correct
  $ opam-ci-check lint -r . b.0.0.1 # Lint b.0.0.1 (new package) with inference
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors
  $ git apply "patches/b.0.0.3-correct.patch"
  $ sh "scripts/setup_sources.sh" b 0.0.3 dune-project
  Created tarball b.0.0.3.tgz
  Updated checksum for b.0.0.3.tgz in b.0.0.3's opam file
  $ git add .
  $ git commit -qm b.0.0.3-correct
  $ opam-ci-check lint -r . b.0.0.3   # Lint b.0.0.3 (old package) with inference
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors
  $ git log --graph --pretty=format:'%s%d'
  * b.0.0.3-correct (HEAD -> new-branch-1)
  * b.0.0.1-correct
  * a-1 (tag: initial-state, master)
  $ opam-ci-check lint -r . a-1.0.0.2:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.2: No package source directory provided.
  [1]
  $ opam-ci-check lint -r . b.0.0.3:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors

The quiet flag suppresses noisy output when everything is correct:

  $ opam-ci-check lint -r . --quiet b.0.0.3:new=false

Setup repo for incorrect b package tests

  $ git reset -q --hard initial-state
  $ git apply "patches/b-incorrect-opam.patch"
  $ git add packages/
  $ echo "(lang dune 3.16)" > dune-project
  $ sh "scripts/setup_sources.sh" b 0.0.2 dune-project
  Created tarball b.0.0.2.tgz
  Updated checksum for b.0.0.2.tgz in b.0.0.2's opam file
  $ sh "scripts/setup_sources.sh" b 0.0.3 dune-project
  Created tarball b.0.0.3.tgz
  Updated checksum for b.0.0.3.tgz in b.0.0.3's opam file
  $ echo "foo" > bar
  $ echo "(lang dune 3.16)" > dune-project
  $ ln -s /tmp/non-existant-link random-link
  $ git commit -qm b-incorrect-opam
  $ git log --graph --pretty=format:'%s%d'
  * b-incorrect-opam (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)


Test the following:
- [b.0.0.1] is missing the [author] field
- [b.0.0.1] is missing the maintainer's email in the [maintainer] field
- [b.0.0.1] has default, example tags
- [b.0.0.2] has an extra unknown field
- [b.0.0.3] has a pin-depends present, and a conflict class without the required prefix; use of extra-files and a weak checksum algorithm
- [system-b.0.0.1] is using a restricted prefix in its name
- [b.0.0.6] has a incorrectly formatted opam file
- [b.0.0.7] has opam lint errors/warnings

  $ opam-ci-check lint -r . b.0.0.1:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in b.0.0.1:            warning 25: Missing field 'authors'
  Error in b.0.0.1: No package source directory provided.
  Warning in b.0.0.1: The package has not replaced the following default, example tags: topics, project
  [1]
  $ opam-ci-check lint -r . b.0.0.2:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in b.0.0.2:              error  3: File format error in 'unknown-field' at line 11, column 0: Invalid field unknown-field
  Warning in b.0.0.2: Dubious use of 'dune subst'. 'dune subst' should always only be called with {dev} (i.e. ["dune" "subst"] {dev}) If your opam file has been autogenerated by dune, you need to upgrade your dune-project to at least (lang dune 2.7).
  Warning in b.0.0.2: The package tagged dune as a build dependency. Due to a bug in dune (https://github.com/ocaml/dune/issues/2147) this should never be the case. Please remove the {build} tag from its filter.
  [1]
  $ opam-ci-check lint -r . b.0.0.3:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in b.0.0.3: Weak checksum algorithm(s) provided. Please use SHA-256 or SHA-512. Details: opam field extra-files contains only MD5 as checksum for 0install.install
  Error in b.0.0.3: pin-depends present. This is not allowed in the opam-repository.
  Error in b.0.0.3: extra-files present. This is not allowed in the opam-repository. Please use extra-source instead.
  Error in b.0.0.3: package with conflict class 'ocaml-host-arch' requires name prefix 'host-arch-'
  [1]
  $ opam-ci-check lint -r . system-b.0.0.1:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in system-b.0.0.1: No package source directory provided.
  Error in system-b.0.0.1: package with prefix 'system-' requires conflict class 'ocaml-system'
  [1]
  $ opam-ci-check lint -r . b.0.0.6:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in $TESTCASE_ROOT/./packages/b/b.0.0.6/opam: failed to parse opam file:
  'At ./<none>:13:12-13:13::
  Parse error'
  [1]
  $ opam-ci-check lint -r . b.0.0.7:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in b.0.0.7:              error 23: Missing field 'maintainer'
  Error in b.0.0.7:            warning 25: Missing field 'authors'
  Error in b.0.0.7: No package source directory provided.
  [1]

The quiet flag still allows output when there is an error:

  $ opam-ci-check lint -r . --quiet b.0.0.6:new=false
  Error in $TESTCASE_ROOT/./packages/b/b.0.0.6/opam: failed to parse opam file:
  'At ./<none>:13:12-13:13::
  Parse error'
  [1]

Test that we can run just the opam-file tests (i.e., the checks for maintainer
and authors fields are not run):

  $ opam-ci-check lint -r . --checks=opam-file b.0.0.7:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in b.0.0.7:              error 23: Missing field 'maintainer'
  Error in b.0.0.7:            warning 25: Missing field 'authors'
  [1]


Test that a package with no constraint on the dune version in the opam file
passes linting when in has (lang dune 1.0) in its dune-project:

  $ git reset -q --hard initial-state
  $ git apply "patches/b.0.0.1-no-dune-lower-bound.patch"
  $ echo "(lang dune 1.0)" > dune-project
  $ sh "scripts/setup_sources.sh" b 0.0.1 dune-project
  Created tarball b.0.0.1.tgz
  Updated checksum for b.0.0.1.tgz in b.0.0.1's opam file
  $ git add .
  $ git commit -qm b.0.0.1-no-dune-lower-bound
  $ opam-ci-check lint -r . b.0.0.1 # Lint b.0.0.1 (new package) with inference
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors

Setup repo for name collision tests

  $ git reset -q --hard initial-state
  $ git apply "patches/a_1-name-collision.patch"
  $ git add .
  $ git commit -qm a_1-name-collision
  $ git log --graph --pretty=format:'%s%d'
  * a_1-name-collision (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)

Tests the package name collision detection by adding a version of a package
[a_1] that conflicts with the existing [a-1] package

  $ opam-ci-check lint -r . a_1.0.0.1   # inferring that the package is new
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a_1.0.0.1: No package source directory provided.
  Warning in a_1.0.0.1: Possible name collision with package 'a-1'
  [1]

Setup repo for unnecessary fields tests

  $ git reset -q --hard initial-state
  $ git apply "patches/a-1-unnecessary-fields.patch"
  $ git add .
  $ git commit -qm unnecessary-fields-a-1
  $ git log --graph --pretty=format:'%s%d'
  * unnecessary-fields-a-1 (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)

Test presence of unnecessary fields in a-1.0.0.2 package

  $ opam-ci-check lint -r . a-1.0.0.2:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Warning in a-1.0.0.2: Unnecessary field 'name'. It is suggested to remove it.
  Warning in a-1.0.0.2: Unnecessary field 'version'. It is suggested to remove it.
  Error in a-1.0.0.2: No package source directory provided.
  [1]

Setup repo for unmatched name and version test

  $ git reset -q --hard initial-state
  $ git apply "patches/a-1-unmatched-name-version.patch"
  $ git add .
  $ git commit -qm unmatched-name-version-fields-a-1
  $ git log --graph --pretty=format:'%s%d'
  * unmatched-name-version-fields-a-1 (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)

Test presence of unnecessary fields in a-1.0.0.2 package

  $ opam-ci-check lint -r . a-1.0.0.2:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.2: The field 'name' that doesn't match its context. Field 'name' has value 'b-1' but was expected of value 'a-1'.
  Error in a-1.0.0.2: The field 'version' that doesn't match its context. Field 'version' has value '0.0.1' but was expected of value '0.0.2'.
  Error in a-1.0.0.2: No package source directory provided.
  [1]

Setup repo for unexpected file

  $ git reset -q --hard initial-state
  $ git apply "patches/a-1-unexpected-file.patch"
  $ git add .
  $ git commit -qm unexpected-file-a-1
  $ git log --graph --pretty=format:'%s%d'
  * unexpected-file-a-1 (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)

Test presence of unexpected files in a-1.0.0.2 package

  $ opam-ci-check lint -r . a-1.0.0.2:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.2: Unexpected file in packages/a-1/a-1.0.0.2/files
  Error in a-1.0.0.2: No package source directory provided.
  [1]

Setup repo for Forbidden perm file

  $ git reset -q --hard initial-state
  $ chmod 500 packages/a-1/a-1.0.0.2/opam
  $ git add .
  $ git commit -qm forbidden-perm-file-a-1
  $ git log --graph --pretty=format:'%s%d'
  * forbidden-perm-file-a-1 (HEAD -> new-branch-1)
  * a-1 (tag: initial-state, master)

Test presence of unexpected files in a-1.0.0.2 package

  $ opam-ci-check lint -r . a-1.0.0.2:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.2: Forbidden permission for file packages/a-1/a-1.0.0.2/opam. All files should have permissions 644.
  Error in a-1.0.0.2: No package source directory provided.
  [1]

# Maintainer contact lint

The maintainer contact lint requires that a package EITHER provide a URL for the
`bug-tracker` OR that at least one email is provided in the `maintainer` field.
If neither of there requirements are met, the check should fail, otherwise it
should  passes.

Add multiple maintainers with no email and remove the bug-reports field from a
valid package:

  $ git reset -q --hard initial-state
  $ sed \
  > -e 's/maintainer.*/maintainer: ["Maintainer1" "Maintaner2"]/' \
  > -e '/bug-reports.*/d' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ git diff packages/a-1/a-1.0.0.1/opam | grep '^[+-][^+-]'
  -maintainer: "Maintainer <me@example.com>"
  +maintainer: ["Maintainer1" "Maintaner2"]
  -bug-reports: "https://github.com/ocurrent/opam-repo-ci/issues"

Test that we report the expected linting error:

  $ opam-ci-check lint -r . a-1.0.0.1:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1:            warning 36: Missing field 'bug-reports'
  Error in a-1.0.0.1: No package source directory provided.
  Error in a-1.0.0.1: There is no way to contact the maintainer(s) 'Maintainer1, Maintaner2'. A package must either specify a url for 'bug-reports' or provide an email address in the 'maintainer' field.
  [1]

Add one email to the maintainers, and ensure it now passes the maintainer
contact lint:

  $ sed \
  > -e 's/"Maintaner2"/"Maintaner2 <me@example.com>"/' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ git diff packages/a-1/a-1.0.0.1/opam | grep '^[+-][^+-]'
  -maintainer: "Maintainer <me@example.com>"
  +maintainer: ["Maintainer1" "Maintaner2 <me@example.com>"]
  -bug-reports: "https://github.com/ocurrent/opam-repo-ci/issues"
  $ opam-ci-check lint -r . a-1.0.0.1:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1:            warning 36: Missing field 'bug-reports'
  Error in a-1.0.0.1: No package source directory provided.
  [1]

Just remove the email address, leaving the bug-reports and ensure that it now
passes linting:

  $ git reset -q --hard initial-state
  $ sed \
  > -e 's/maintainer.*/maintainer: ["Maintainer1" "Maintaner2"]/' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ git diff packages/a-1/a-1.0.0.1/opam | grep '^[+-][^+-]'
  -maintainer: "Maintainer <me@example.com>"
  +maintainer: ["Maintainer1" "Maintaner2"]
  $ opam-ci-check lint -r . a-1.0.0.1:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1: No package source directory provided.
  [1]

# Opam Archive linting tests

These linting checks automate enforcement of the opam archiving policy found at
https://github.com/ocaml/opam-repository/blob/master/governance/policies/archiving.md#archiving-a-package

  $ git reset -q --hard initial-state

Test that we do not report an error on a minimal well-formed package:

  $ echo 'x-reason-for-archiving: [ "source-unavailable" ]' \
  > >> packages/a-1/a-1.0.0.1/opam
  $ echo 'x-opam-repository-commit-hash-at-time-of-archiving: "de786e28dbea73843ad5e5f0290a4e81fba39370"' \
  > >> packages/a-1/a-1.0.0.1/opam
  $ cat packages/a-1/a-1.0.0.1/opam
  opam-version: "2.0"
  synopsis: "Synopsis"
  description: "Description"
  maintainer: "Maintainer <me@example.com>"
  author: "Author"
  license: "Apache-2.0"
  homepage: "https://github.com/ocurrent/opam-repo-ci"
  bug-reports: "https://github.com/ocurrent/opam-repo-ci/issues"
  dev-repo: "git+https://github.com/ocurrent/opam-repo-ci.git"
  doc: "https://ocurrent.github.io/ocurrent/"
  build: []
  depends: []
  x-reason-for-archiving: [ "source-unavailable" ]
  x-opam-repository-commit-hash-at-time-of-archiving: "de786e28dbea73843ad5e5f0290a4e81fba39370"
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors

## Upper bounds checks

All dependencies

Test that we report errors when a package has dependencies without an upper bound:

  $ sed \
  > -e 's/depends.*/depends: [ "foo" {with-test} "bar" {>= "0.0.1"} "baz" ]/' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1: An upper bound constraint is missing on dependency 'baz'
  Error in a-1.0.0.1: An upper bound constraint is missing on dependency 'bar'
  Error in a-1.0.0.1: An upper bound constraint is missing on dependency 'foo'
  [1]

Test that we do NOT report errors when all a packages dependencies have an upper bound:

  $ sed \
  > -e 's/depends.*/depends: ["foo" {with-test \& <= "0.1.0"} "bar" {>= "0.0.1" \& = "0.1.0"} "baz" {< "0.0.1"}]/' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors

Test that we do NOT report errors when the compiler dependency has no upper bound:

  $ sed \
  > -e 's/depends.*/depends: ["ocaml"]/' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors

## x-reason-for-archiving checks

Test we report an error when the x-reason-for-archiving is missing:

  $ sed \
  > -e '/x-reason-for-archiving/d' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1: The field 'x-reason-for-archiving' must be present and hold a nonempty list of one or more of the valid reasons ocaml-version, source-unavailable, maintenance-intent, uninstallable
  [1]

Test we report an error when the x-reason-for-archiving has an invalid value:

  $ echo 'x-reason-for-archiving: "not a list"' >> packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1: The field 'x-reason-for-archiving' must be present and hold a nonempty list of one or more of the valid reasons ocaml-version, source-unavailable, maintenance-intent, uninstallable
  [1]

Test we report an error when the x-reason-for-archiving has an empty list:

  $ sed \
  > -e 's/x-reason-for-archiving:.*/x-reason-for-archiving: []/' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1: The field 'x-reason-for-archiving' must be present and hold a nonempty list of one or more of the valid reasons ocaml-version, source-unavailable, maintenance-intent, uninstallable
  [1]

Test we report an error when the x-reason-for-archiving has an invalid reason:

  $ sed \
  > -e 's/x-reason-for-archiving:.*/x-reason-for-archiving: ["an indalid reason"]/' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1: The field 'x-reason-for-archiving' must be present and hold a nonempty list of one or more of the valid reasons ocaml-version, source-unavailable, maintenance-intent, uninstallable
  [1]

Test we do NOT report an error when the x-reason-for-archiving has multiple invalid reasons:

  $ sed \
  > -e 's/x-reason-for-archiving:.*/x-reason-for-archiving: ["source-unavailable" "maintenance-intent"]/' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors

# x-opam-repository-commit-hash-at-time-of-archiving checks

Test we report an error when the x-opam-repository-commit-hash-at-time-of-archiving
is missing:

  $ sed \
  > -e '/x-opam-repository-commit-hash-at-time-of-archiving/d' \
  > packages/a-1/a-1.0.0.1/opam > opam.new
  $ mv opam.new packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1: The field 'x-opam-repository-commit-hash-at-time-of-archiving' must be present and hold a string recording the commit hash of the primary repo at the time the package version is archived.
  [1]

Test we report an error when the x-opam-repository-commit-hash-at-time-of-archiving
has an invald value:

  $ echo 'x-opam-repository-commit-hash-at-time-of-archiving: false' \
  > >> packages/a-1/a-1.0.0.1/opam
  $ opam-ci-check lint -r . --check=archive-repo a-1.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in a-1.0.0.1: The field 'x-opam-repository-commit-hash-at-time-of-archiving' must be present and hold a string recording the commit hash of the primary repo at the time the package version is archived.
  [1]

## conf- package checks

Reset the project repo

  $ git reset -q --hard initial-state

Test that a package with a conf- prefix but missing the `conf` flag and `depext`
field triggers the expected lint errors:

  $ export INVALID_CONF_DIR=packages/conf-invalid/conf-invalid.0.0.1/
  $ mkdir -p $INVALID_CONF_DIR
  $ cat > $INVALID_CONF_DIR/opam <<EOF
  > opam-version: "2.0"
  > synopsis: "Synopsis"
  > description: "Description"
  > maintainer: "Maintainer <me@example.com>"
  > author: "Author"
  > license: "Apache-2.0"
  > homepage: "https://github.com/ocurrent/opam-repo-ci"
  > bug-reports: "https://github.com/ocurrent/opam-repo-ci/issues"
  > dev-repo: "git+https://github.com/ocurrent/opam-repo-ci.git"
  > doc: "https://ocurrent.github.io/ocurrent/"
  > build: []
  > depends: []
  > EOF
  $ opam-ci-check lint -r . conf-invalid.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in conf-invalid.0.0.1: No package source directory provided.
  Error in conf-invalid.0.0.1: conf packages should always use the 'conf-' name prefix, the 'conf' flag, and the 'depext' field all together, but this package only has the 'conf-' name prefix
  [1]
  $ git reset -q --hard initial-state

Test that a package with a conf- prefix and `depext` feild, but missing the
`conf` flag, triggers the expected lint errors:

  $ export INVALID_CONF_DIR=packages/conf-invalid/conf-invalid.0.0.2/
  $ mkdir -p $INVALID_CONF_DIR
  $ cat > $INVALID_CONF_DIR/opam <<EOF
  > opam-version: "2.0"
  > synopsis: "Synopsis"
  > description: "Description"
  > maintainer: "Maintainer <me@example.com>"
  > author: "Author"
  > license: "Apache-2.0"
  > homepage: "https://github.com/ocurrent/opam-repo-ci"
  > bug-reports: "https://github.com/ocurrent/opam-repo-ci/issues"
  > dev-repo: "git+https://github.com/ocurrent/opam-repo-ci.git"
  > doc: "https://ocurrent.github.io/ocurrent/"
  > build: []
  > depends: []
  > depexts: [ "curl" ]
  > EOF
  $ opam-ci-check lint -r . conf-invalid.0.0.2
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in conf-invalid.0.0.2: No package source directory provided.
  Error in conf-invalid.0.0.2: conf packages should always use the 'conf-' name prefix, the 'conf' flag, and the 'depext' field all together, but this package only has the 'conf-' name prefix and a non-empty 'depext' field
  [1]
  $ git reset -q --hard initial-state

Test that a valid conf- package triggers no linting errors (in particular, a
valid conf package should not trigger 'No package source directory provided'):

  $ export VALID_CONF_DIR=packages/conf-valid/conf-valid.0.0.1/
  $ mkdir -p $VALID_CONF_DIR
  $ cat > $VALID_CONF_DIR/opam <<EOF
  > opam-version: "2.0"
  > synopsis: "Synopsis"
  > description: "Description"
  > maintainer: "Maintainer <me@example.com>"
  > author: "Author"
  > license: "Apache-2.0"
  > homepage: "https://github.com/ocurrent/opam-repo-ci"
  > bug-reports: "https://github.com/ocurrent/opam-repo-ci/issues"
  > dev-repo: "git+https://github.com/ocurrent/opam-repo-ci.git"
  > doc: "https://ocurrent.github.io/ocurrent/"
  > build: []
  > depends: []
  > depexts: [ "curl" ]
  > flags: [ conf ]
  > EOF
  $ opam-ci-check lint -r . conf-valid.0.0.1
  Linting opam-repository at $TESTCASE_ROOT/. ...
  No errors
  $ git reset -q --hard initial-state
