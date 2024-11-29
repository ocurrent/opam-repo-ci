# Error handling of CLI parsing

Test for an invalid package spec

  $ opam-ci-check lint -r . '=-~!:new=true'
  opam-ci-check: invalid value '=-~!', expected opam package spec in the form
                 <name.version>
  Usage: opam-ci-check lint [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for an invalid attributes

Test for invalid attributes that are properly formed

  $ opam-ci-check lint -r . 'foo.0.1.0:bar=baz'
  opam-ci-check: invalid element in list ('bar=baz'): bar=baz is an not a valid
                 attribute. Only [src=<path>] or [new=<true|false>] allowed
  Usage: opam-ci-check lint [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for an invalid values to a valid key

  $ opam-ci-check lint -r . 'foo.0.1.0:new=invalid'
  opam-ci-check: invalid element in list ('new=invalid'): invalid must be
                 [true] or [false]
  Usage: opam-ci-check lint [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for a missing value

  $ opam-ci-check lint -r . 'foo.0.1.0:src='
  opam-ci-check: invalid element in list ('src='): src= is an not a valid
                 attribute. Only [src=<path>] or [new=<true|false>] allowed
  Usage: opam-ci-check lint [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for a valid key with no value

  $ opam-ci-check lint -r . 'foo.0.1.0:src'
  opam-ci-check: invalid element in list ('src'): src is an not a valid
                 attribute. Only [src=<path>] or [new=<true|false>] allowed
  Usage: opam-ci-check lint [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for `src` with a non-existent directory

  $ opam-ci-check lint -r . 'foo.0.1.0:src=./not/a/dir'
  opam-ci-check: invalid element in list ('src=./not/a/dir'): ./not/a/dir: No
                 such file or directory
  Usage: opam-ci-check lint [--opam-repository=VAL] [OPTION]… [ARG]…
  Try 'opam-ci-check lint --help' or 'opam-ci-check --help' for more information.
  [124]

Test for invalid extra colons

  $ opam-ci-check lint -r . 'foo.0.1.0:bing-bong:bang'
  opam-ci-check: Invalid argument spec foo.0.1.0:bing-bong:bang. Argument specs
                 should be of the form arg[:k1=v1[,k2=v2]]
  Usage: opam-ci-check lint [--opam-repository=VAL] [OPTION]… [ARG]…
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
  $ sh "scripts/setup_sources.sh" b 0.0.4 bar
  Created tarball b.0.0.4.tgz
  Updated checksum for b.0.0.4.tgz in b.0.0.4's opam file
  $ echo "(lang dune 3.16)" > dune-project
  $ ln -s /tmp/non-existant-link random-link
  $ sh "scripts/setup_sources.sh" b 0.0.5 dune-project random-link
  Created tarball b.0.0.5.tgz
  Updated checksum for b.0.0.5.tgz in b.0.0.5's opam file
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
- [b.0.0.4] has a missing dune-project file
- [b.0.0.5] has a dune-project file, but no explicit dependency on dune
- [b.0.0.6] has a incorrectly formatted opam file
- [b.0.0.7] has opam lint errors/warnings

  $ opam-ci-check lint -r . b.0.0.1:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in b.0.0.1: No package source directory provided.
  Error in b.0.0.1:            warning 25: Missing field 'authors'
  Warning in b.0.0.1: The package has not replaced the following default, example tags: topics, project
  [1]
  $ opam-ci-check lint -r . b.0.0.2:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Warning in b.0.0.2: Dubious use of 'dune subst'. 'dune subst' should always only be called with {dev} (i.e. ["dune" "subst"] {dev}) If your opam file has been autogenerated by dune, you need to upgrade your dune-project to at least (lang dune 2.7).
  Warning in b.0.0.2: The package tagged dune as a build dependency. Due to a bug in dune (https://github.com/ocaml/dune/issues/2147) this should never be the case. Please remove the {build} tag from its filter.
  Warning in b.0.0.2: The package has a dune dependency without a lower bound.
  Error in b.0.0.2:              error  3: File format error in 'unknown-field' at line 11, column 0: Invalid field unknown-field
  [1]
  $ opam-ci-check lint -r . b.0.0.3:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in b.0.0.3: Your dune-project file indicates that this package requires at least dune 3.16 but your opam file only requires dune >= 3.15.0. Please check which requirement is the right one, and fix the other.
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
  $ opam-ci-check lint -r . b.0.0.4:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Warning in b.0.0.4: The package seems to use dune but the dune-project file is missing.
  [1]
  $ opam-ci-check lint -r . b.0.0.5:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Warning in b.0.0.5: The package has a dune-project file but no explicit dependency on dune was found.
  [1]
  $ opam-ci-check lint -r . b.0.0.6:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in $TESTCASE_ROOT/./packages/b/b.0.0.6/opam: Failed to parse the opam file due to 'Parse error'
  [1]
  $ opam-ci-check lint -r . b.0.0.7:new=false
  Linting opam-repository at $TESTCASE_ROOT/. ...
  Error in b.0.0.7: No package source directory provided.
  Error in b.0.0.7:              error 23: Missing field 'maintainer'
  Error in b.0.0.7:            warning 25: Missing field 'authors'
  [1]

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
  Error in a-1.0.0.2: No package source directory provided.
  Error in a-1.0.0.2: Unexpected file in packages/a-1/a-1.0.0.2/files
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
  Error in a-1.0.0.2: No package source directory provided.
  Error in a-1.0.0.2: Forbidden permission for file packages/a-1/a-1.0.0.2/opam. All files should have permissions 644.
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
  Error in a-1.0.0.1: No package source directory provided.
  Error in a-1.0.0.1:            warning 36: Missing field 'bug-reports'
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
  Error in a-1.0.0.1: No package source directory provided.
  Error in a-1.0.0.1:            warning 36: Missing field 'bug-reports'
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
