#!/usr/bin/env bash

set -ue

CACHE=temp
MESSAGE_PREFIX="[download-fonts.sh]"
cd "$(dirname "$0")"
mkdir -p "$CACHE"

show_message() {
  echo "$MESSAGE_PREFIX $1."
}

if command shasum --version >/dev/null 2>&1; then
  show_message "Using shasum"
  SHA1SUM=shasum
elif command sha1sum --version >/dev/null 2>&1; then
  show_message "Using sha1sum"
  SHA1SUM=sha1sum
else
  echo "No SHA checksum checkers found.  Please install shasum or sha1sum" >&2
  exit 1
fi

validate_file() {
  (
    NAME="$1"
    cd "$CACHE"
    $SHA1SUM --check "$NAME.sha1"
  )
}

download_file() {
  (
    NAME="$1"
    URL="$2"
    if [ -f "$CACHE/$NAME" ] && validate_file "$NAME"; then
      show_message "'$NAME' found in '$CACHE/'."
    else
      show_message "downloading '$NAME' ..."
      wget -O "$CACHE/$NAME" "$URL"
      show_message "finished downloading '$NAME'."
    fi
  )
}

# Latin Modern
NAME=lm2.004otf
download_file "$NAME.zip" "https://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip"
unzip -o "$CACHE/$NAME.zip" "*.otf" -d lib-satysfi/dist/fonts/

# Latin Modern Math
NAME=latinmodern-math-1959
download_file "$NAME.zip" "https://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip"
unzip -o "$CACHE/$NAME.zip" "*.otf" -d "$CACHE/"
cp "$CACHE"/latinmodern-math-1959/otf/latinmodern-math.otf lib-satysfi/dist/fonts/

# Junicode
NAME=junicode-1.002
download_file "$NAME.zip" "http://downloads.sourceforge.net/project/junicode/junicode/junicode-1.002/junicode-1.002.zip"
unzip -o "$CACHE/$NAME.zip" "*.ttf" -d lib-satysfi/dist/fonts/

# IPAexfont
NAME=IPAexfont00401
download_file "$NAME.zip" "https://moji.or.jp/wp-content/ipafont/IPAexfont/IPAexfont00401.zip"
unzip -o "$CACHE/$NAME.zip" "*.ttf" -d "$CACHE/"
cp "$CACHE"/IPAexfont00401/ipaexg.ttf lib-satysfi/dist/fonts/
cp "$CACHE"/IPAexfont00401/ipaexm.ttf lib-satysfi/dist/fonts/
show_message "end."
