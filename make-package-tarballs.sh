#!/bin/sh

TARGET_DIR=temp/tarballs

echo "Deleting copied font files ..."
for FILE in $(find lib-satysfi -name \*.ttf -or -name \*.otf | grep '^lib-satysfi/packages/'); do
    rm "$FILE"
done

mkdir -p "$TARGET_DIR"
for FILE in $(find lib-satysfi -name satysfi.yaml); do
    DIR="$(dirname "$FILE")"
    NAME="$(basename "$DIR")"
    TARGET="$TARGET_DIR/$NAME.tar.gz"
    echo "Compressing $DIR to $TARGET ..."
    tar -czf "$TARGET" "$DIR"
    echo "Checksum:"
    md5sum "$TARGET"
done

echo "Done."
