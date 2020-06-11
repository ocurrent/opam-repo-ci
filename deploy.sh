#!/bin/bash -ex

docker build -t opam-repo-ci-web -f Dockerfile.web .
docker build -t opam-repo-ci-service -f Dockerfile .
docker stack rm opam-ci
sleep 15
docker stack deploy -c stack.yml opam-ci
