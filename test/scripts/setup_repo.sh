#!/bin/sh

mkdir "capnp-secrets"
git init -q .
echo "var/" > .gitignore
git config --local user.email test@test.com
git config --local user.name Test
git checkout -qb master
git apply "patches/a-1.patch"
git add .
git commit -qm a-1
git tag 'initial-state'
