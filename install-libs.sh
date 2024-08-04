#!/usr/bin/env bash

LIBDIR=${1:-/usr/local/share/satysfi}

install -d "${LIBDIR}"
install -d "${LIBDIR}/dist"
install -d "${LIBDIR}/dist/unidata"
install -m 644 lib-satysfi/dist/unidata/*.txt "${LIBDIR}/dist/unidata"
install -d "${LIBDIR}/dist/fonts"
install -m 644 lib-satysfi/dist/fonts/* "${LIBDIR}/dist/fonts"
install -d "${LIBDIR}"/dist/hash
install -m 644 lib-satysfi/dist/hash/* "${LIBDIR}/dist/hash"
install -d "${LIBDIR}/dist/hyph"
install -m 644 lib-satysfi/dist/hyph/* "${LIBDIR}/dist/hyph"
install -d "${LIBDIR}"/dist/packages
install -m 644 lib-satysfi/dist/packages/* "${LIBDIR}/dist/packages"
install -d "${LIBDIR}"/dist/md
install -m 644 lib-satysfi/dist/md/* "${LIBDIR}/dist/md"
