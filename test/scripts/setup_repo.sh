#!/bin/sh

git init -q .
git config --local user.email test@test.com
git config --local user.name Test
git checkout -qb master
git apply "patches/a-1.patch"
touch repo
git add .
git commit -qm a-1
