#!/bin/sh

FAILED=0
for FILE in $(find lib-satysfi -name satysfi.yaml); do
    DIR="$(dirname "$FILE")"
    satysfi solve "$DIR"
    if diff "$DIR/package.satysfi-lock" "$DIR/package.satysfi-lock-expected"; then
        satysfi build "$DIR"
    else
        echo "! FAILED: $DIR"
        FAILED=1
    fi
done
if [ $FAILED -ne 0 ]; then
    exit 1
fi
