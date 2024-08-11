#!/usr/bin/env bash

LIBDIR=${1:-/usr/local/share/satysfi}

install -d "${LIBDIR}"
install -m 644 lib-satysfi/satysfi-library-root.yaml "${LIBDIR}/satysfi-library-root.yaml"
install -d "${LIBDIR}/unidata"
install -m 644 lib-satysfi/unidata/*.txt "${LIBDIR}/unidata"
install -d "${LIBDIR}/hyph"
install -m 644 lib-satysfi/hyph/* "${LIBDIR}/hyph"

# It is no longer necessary to install packages beforehand due to the package system:
#
# install -d "${LIBDIR}/packages"
# (cd lib-satysfi && find packages -type f -exec install -Dm 644 "{}" "${LIBDIR}/{}" \;)
# install -d "${LIBDIR}/registries"
# (cd lib-satysfi && find registries -type f -exec install -Dm 644 "{}" "${LIBDIR}/{}" \;)
