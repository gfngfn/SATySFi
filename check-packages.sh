#!/bin/sh
set -xe
for FILE in $(find lib-satysfi -name satysfi.yaml); do
    DIR="$(dirname "$FILE")"
    satysfi solve "$DIR"
    satysfi build "$DIR"
done
