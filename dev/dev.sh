#!/bin/bash
set -u
source dev/conf.env

trap "trap - SIGTERM && sudo kill 0" SIGINT SIGTERM EXIT

ulimit -n 102400

mkdir capnp-secrets

# Start ocluster
mkdir /tmp/ocluster-state

ocluster-scheduler \
  --capnp-secret-key-file=capnp-secrets/secret-key.pem \
  --capnp-listen-address=tcp:0.0.0.0:9001 \
  --capnp-public-address=tcp:127.0.0.1:9001 \
  --state-dir=/tmp/ocluster-state \
  --default-clients=opam-repo-ci \
  --pools=linux-x86_64,linux-arm64,linux-ppc64,linux-s390x \
  --verbose &

until [ -f capnp-secrets/pool-linux-x86_64.cap ]; do sleep 1; done

# Start an ocluster worker
mkdir /tmp/obuilder-state
mkdir /tmp/obuilder-store

WORKER=$(whereis -b ocluster-worker | cut -f 2 -d ' ')
sudo "$WORKER" \
  --store=rsync:/tmp/obuilder-store \
  --rsync-mode=hardlink_unsafe \
  --state-dir=/tmp/obuilder-state \
  --name=local_linux-x86_64 \
  --connect=capnp-secrets/pool-linux-x86_64.cap \
  --capacity=1 \
  --verbose &

# Public IP to receive GitHub webhooks on port 8080
ngrok authtoken "$NGROK_AUTH"
ngrok http 8080 --log=stdout > /tmp/ngrok.log &
URL=$(tail -F /tmp/ngrok.log | grep -m 1 -o -E 'https://[^ ]*.ngrok.io$')
WEBHOOK="${URL}/webhooks/github"

# Update GitHub webhook url
SECRET=mysecret
echo -n "$SECRET" > /tmp/github.secret
JWT=$(dune exec --root=. --display=quiet ./dev/jwt.exe "$GITHUB_APP_ID" "$GITHUB_PRIVATE_KEY_FILE")
curl \
  -X PATCH \
  -H "Authorization: Bearer $JWT" \
  -H "Accept: application/vnd.github.v3+json" \
  https://api.github.com/app/hook/config \
  -d "{\"url\":\"${WEBHOOK}\",\"secret\":\"${SECRET}\"}"

function build_and_run() {
  local EXE=$1
  local ARGS=${@: 2}
  dune build --watch --terminal-persistence=preserve "./$EXE" &
  while :
  do
    until [ -f "_build/default/$EXE" ]; do sleep 1; done
    _build/default/$EXE $ARGS &
    local PID=$!
    inotifywait "_build/default/$EXE" | grep -q "$EXE"
    kill "$PID"
    wait "$PID"
  done
}

# Run the pipeline
build_and_run service/main.exe \
  "--github-app-id=${GITHUB_APP_ID}" \
  "--github-account-allowlist=${GITHUB_ACCOUNT}" \
  "--github-private-key-file=${GITHUB_PRIVATE_KEY_FILE}" \
  --github-webhook-secret-file=/tmp/github.secret \
  --submission-service=capnp-secrets/submit-opam-repo-ci.cap \
  --confirm=none \
  --port=8080 \
  --capnp-address=tcp:127.0.0.1:9000 &

# Run the frontend
until [ -f capnp-secrets/opam-repo-ci-admin.cap ]; do sleep 1; done

build_and_run web-ui/main.exe \
  --backend capnp-secrets/opam-repo-ci-admin.cap
