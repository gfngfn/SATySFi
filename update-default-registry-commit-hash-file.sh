#!/bin/sh

HASH_FILE=default-registry-commit-hash.txt
REPO_URL=https://github.com/SATySFi/default-registry
BRANCH=temp-dev-saphe

git ls-remote "${REPO_URL}" "refs/heads/${BRANCH}" > "$HASH_FILE"
