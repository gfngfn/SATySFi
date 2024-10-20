#!/bin/sh

TARGET_DIR=temp/tarballs
REPO_ROOT="$(pwd)/$(dirname "$0")"

echo "Deleting copied font files ..."
for FILE in $(find lib-satysfi -name \*.ttf -or -name \*.otf | grep '^lib-satysfi/packages/'); do
    rm "${FILE}"
done

mkdir -p "${TARGET_DIR}"
for FILE in $(find lib-satysfi -name saphe.yaml); do
    VERSIONED_DIR="$(dirname "${FILE}")"
    PACKAGE_DIR="$(dirname "${VERSIONED_DIR}")"
    VERSIONED_NAME="$(basename "${VERSIONED_DIR}")"

    ABS_TARGET="${REPO_ROOT}/${TARGET_DIR}/${VERSIONED_NAME}.tar.gz"

    echo "Compressing ${VERSIONED_NAME} at ${PACKAGE_DIR} to ${ABS_TARGET} ..."
    (cd "${PACKAGE_DIR}" && tar -czf "${ABS_TARGET}" --exclude "*expected" "${VERSIONED_NAME}")
    echo "Checksum:"
    md5sum "${ABS_TARGET}"
done

echo "Done."
