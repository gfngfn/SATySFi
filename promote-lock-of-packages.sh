#!/bin/sh

for FILE in $(find lib-satysfi -name saphe.yaml); do
    DIR="$(dirname "$FILE")"
    cp "$DIR/saphe.lock.yaml" "$DIR/saphe.lock.yaml.expected"
    cp "$DIR/satysfi-envelope.yaml" "$DIR/satysfi-envelope.yaml.expected"
done
