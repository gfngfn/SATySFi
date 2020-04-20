#!/bin/sh

CACHE=temp
MESSAGE_PREFIX="[download-fonts.sh]"
SHA1SUM=sha1sum\ --ignore-missing\ --check

cd "$(dirname "$0")" || exit 1

# Latin Modern
NAME=lm2.004otf
cd ${CACHE}/ || exit 1; RESULT=$(${SHA1SUM} ${NAME}.sha1); cd ../ || exit 1
if test "${RESULT}" = "${NAME}.zip: OK"; then
  echo "${MESSAGE_PREFIX} '${NAME}.zip' found in '${CACHE}/'."
else
  echo "${MESSAGE_PREFIX} downloading '${NAME}.zip' ..."
  wget -O ${CACHE}/${NAME}.zip http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip
  echo "${MESSAGE_PREFIX} finished downloading '${NAME}.zip'."
fi
unzip -o ${CACHE}/${NAME}.zip "*.otf" -d lib-satysfi/dist/fonts/

# Latin Modern Math
NAME=latinmodern-math-1959
cd ${CACHE}/ || exit 1; RESULT=$(${SHA1SUM} ${NAME}.sha1); cd ../ || exit 1
if test "${RESULT}" = "${NAME}.zip: OK"; then
  echo "${MESSAGE_PREFIX} '${NAME}.zip' found in '${CACHE}/'."
else
  echo "${MESSAGE_PREFIX} downloading '${NAME}.zip' ..."
  wget -O ${CACHE}/${NAME}.zip http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip
  echo "${MESSAGE_PREFIX} finished downloading '${NAME}.zip'."
fi
unzip -o ${CACHE}/${NAME}.zip "*.otf" -d ${CACHE}/
cp ${CACHE}/latinmodern-math-1959/otf/latinmodern-math.otf lib-satysfi/dist/fonts/

# Junicode
NAME=junicode-1.002
cd ${CACHE}/ || exit 1; RESULT=$(${SHA1SUM} ${NAME}.sha1); cd ../ || exit 1
if test "${RESULT}" = "${NAME}.zip: OK"; then
  echo "${MESSAGE_PREFIX} '${NAME}.zip' found in '${CACHE}/'."
else
  echo "${MESSAGE_PREFIX} downloading '${NAME}.zip' ..."
  wget -O ${CACHE}/${NAME}.zip http://downloads.sourceforge.net/project/junicode/junicode/junicode-1.002/junicode-1.002.zip
  echo "${MESSAGE_PREFIX} finished downloading '${NAME}.zip'."
fi
unzip -o ${CACHE}/${NAME}.zip "*.ttf" -d lib-satysfi/dist/fonts/

# IPAexfont
NAME=IPAexfont00401
cd ${CACHE}/ || exit 1; RESULT=$(${SHA1SUM} ${NAME}.sha1); cd ../ || exit 1
if test "${RESULT}" = "${NAME}.zip: OK"; then
  echo "${MESSAGE_PREFIX} '${NAME}.zip' found in '${CACHE}/'."
else
  echo "${MESSAGE_PREFIX} downloading '${NAME}.zip' ..."
  wget -O ${CACHE}/${NAME}.zip https://ipafont.ipa.go.jp/IPAexfont/IPAexfont00401.zip
  echo "${MESSAGE_PREFIX} finished downloading '${NAME}.zip'."
fi
unzip -o ${CACHE}/${NAME}.zip "*.ttf" -d ${CACHE}/
cp ${CACHE}/IPAexfont00301/ipaexg.ttf lib-satysfi/dist/fonts/
cp ${CACHE}/IPAexfont00301/ipaexm.ttf lib-satysfi/dist/fonts/
echo "${MESSAGE_PREFIX} end."
