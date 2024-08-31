#!/bin/bash

SAPHE="${1:-saphe}"

FAILS=()
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
        FAILS+=("$DIR (not solved)")
    fi
    if diff "$DIR/saphe.lock.yaml" "$DIR/saphe.lock.yaml.expected"; then
        if diff "$DIR/satysfi-envelope.yaml" "$DIR/satysfi-envelope.yaml.expected"; then
            "$SAPHE" build "$DIR"
            if [ $? -ne 0 ]; then
                echo "! FAILED (cannot build)"
                FAILS+=("$DIR (cannot build)")
            else
                "$SAPHE" test "$DIR"
                if [ $? -ne  0 ]; then
                    echo "! FAILED (test failed)"
                    FAILS+=("$DIR (test failed)")
                else
                    echo "* OK: $DIR"
                fi
            fi
        else
            echo "! FAILED (envelope config mismatch): $DIR"
            FAILS+=("$DIR (envelope config mismatch)")
        fi
    else
        echo "! FAILED (lock config mismatch): $DIR"
        FAILS+=("$DIR (lock config mismatch)")
    fi
done

RET=0
for FAIL in "${FAILS[@]}"; do
    RET=1
    echo "$FAIL"
done

if [ $RET -ne 0 ]; then
    echo "! some test(s) have failed."
else
    echo "All tests have passed."
fi

exit $RET
