#!/bin/sh

SAPHE="${1:-saphe}"

FAILED=0
FIRST=1
for FILE in $(find lib-satysfi -name saphe.yaml); do
    DIR="$(dirname "$FILE")"
    if [ $FIRST -eq 1 ]; then
        echo "UPDATING..."
        "$SAPHE" update "$DIR"
        FIRST=0
        echo "UPDATED."
    fi
    echo " ==== $DIR ===="
    "$SAPHE" solve "$DIR"
    if [ $? -ne 0 ]; then
        echo "! FAILED (not solved)"
        FAILED=1
    fi
    if diff "$DIR/saphe.lock.yaml" "$DIR/saphe.lock.yaml.expected"; then
        if diff "$DIR/satysfi-envelope.yaml" "$DIR/satysfi-envelope.yaml.expected"; then
            "$SAPHE" build "$DIR"
            if [ $? -ne 0 ]; then
                echo "! FAILED (cannot build)"
                FAILED=1
            else
                echo "* OK: $DIR"
            fi
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
    echo "! some test has failed."
    exit 1
fi
