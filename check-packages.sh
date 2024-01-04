#!/bin/sh

FAILED=0
for FILE in $(find lib-satysfi -name saphe.yaml); do
    DIR="$(dirname "$FILE")"
    ./saphe solve "$DIR"
    if diff "$DIR/saphe.lock.yaml" "$DIR/saphe.lock.yaml.expected"; then
        if diff "$DIR/satysfi-envelope.yaml" "$DIR/satysfi-envelope.yaml.expected"; then
          echo "(TODO: build)"
          #saphe build "$DIR"
          echo "* OK: $DIR"
        else
          echo "! FAILED (envelope config mismatch): $DIR"
          FAILED=1
        fi
    else
        echo "! FAILED (lock config mismatch): $DIR"
        FAILED=1
    fi
done
if [ $FAILED -ne 0 ]; then
    exit 1
fi
