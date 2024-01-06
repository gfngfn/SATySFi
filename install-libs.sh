#!/bin/sh

LIBDIR=${1:-/usr/local/share/satysfi}
INSTALL=${2:-install}

"${INSTALL}" -d "${LIBDIR}"
"${INSTALL}" -d "${LIBDIR}/unidata"
"${INSTALL}" -m 644 lib-satysfi/unidata/*.txt "${LIBDIR}/unidata"
"${INSTALL}" -d "${LIBDIR}/hyph"
"${INSTALL}" -m 644 lib-satysfi/hyph/* "${LIBDIR}/hyph"

# It is no longer necessary to install packages beforehand due to the package system:
#
# "${INSTALL}" -d "${LIBDIR}/packages"
# (cd lib-satysfi && find packages -type f -exec "${INSTALL}" -Dm 644 "{}" "${LIBDIR}/{}" \;)
# "${INSTALL}" -d "${LIBDIR}/registries"
# (cd lib-satysfi && find registries -type f -exec "${INSTALL}" -Dm 644 "{}" "${LIBDIR}/{}" \;)
