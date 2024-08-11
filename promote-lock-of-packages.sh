#!/bin/sh

for FILE in $(find lib-satysfi -name satysfi.yaml); do
    DIR="$(dirname "$FILE")"
    cp "$DIR/package.satysfi-lock" "$DIR/package.satysfi-lock-expected"
done
