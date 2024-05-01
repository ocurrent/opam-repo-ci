#!/usr/bin/env bash

set -xeuo pipefail

PKG="${1:-}"

if [ -z "${PKG}" ]; then
  echo "Need a pkg name with version!"
  exit 1
fi

PREFIX="opam exec --switch=dune-test --"

VERSION="${PKG#*.}"

$PREFIX opam repository set-url --strict default ../opam-repository/

REVDEPS=$($PREFIX opam list -s --verbose --debug --color=never --depends-on "${PKG}" --coinstallable-with "${PKG}" --installable --all-versions --depopts;
          $PREFIX opam list -s --verbose --color=never --depends-on "${PKG}" --coinstallable-with "${PKG}" --installable --all-versions --recursive;
          $PREFIX opam list -s --verbose --color=never --depends-on "${PKG}" --coinstallable-with "${PKG}" --installable --all-versions --with-test --depopts)
REVDEPS=$(echo "$REVDEPS"|sort|uniq)

# $PREFIX opam update --depexts || true

$PREFIX opam pin add -k version -yn "${PKG}" "${VERSION}"
$PREFIX opam reinstall "${PKG}"  # FIXME: CI has a bunch of code to figure out what exactly failed, if so.

for package in ${REVDEPS}; do
  $PREFIX opam reinstall --verbose --with-test "${package}"
  break
done
