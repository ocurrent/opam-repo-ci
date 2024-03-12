The lint check requires created opam files to have permissions 644.

Requiring the user to change their [umask] is not ideal, it would be better to
find a way to change the permissions of the files in the cram tests. [chmod]
works on the files in the test directory, but the [opam-repo-ci-local] binary
appears to use files copied to a new directory with [umask] permissions.

  $ umask
  0022
