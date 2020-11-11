#!/usr/bin/env bash

set -e

if test "$#" != 2; then
    echo "Usage: $0 personal-ocluster.cap opam-repo-ci.pem"
    exit 1
fi

CAP_FILE=$1
GITHUB_PRIVATE_KEY=$2

mkdir -p capnp-secrets

dune exec -- opam-repo-ci-service --confirm=harmless --submission-service "$CAP_FILE" \
    --github-app-id=85861 --github-private-key-file "$GITHUB_PRIVATE_KEY" \
    --github-account-allowlist=ocaml --capnp-address=tcp:127.0.0.1:9000 &

sleep 5
if test "$(jobs | wc -l)" != 1; then
    echo "Something wrong happened"
    exit 1
fi

dune exec -- opam-repo-ci-web --backend capnp-secrets/opam-repo-ci-admin.cap &

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT
xdg-open 'http://localhost:8090/github/ocaml/opam-repository/'

sleep 3
echo
echo '---------------------------------------------'
echo 'A new tab has been opened in your web browser'
echo '---------------------------------------------'
echo

wait
