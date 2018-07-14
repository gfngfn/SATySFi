#!/usr/bin/env bash
set -ue

sudo apt-get install \
  fonts-ipaexfont-mincho \
  fonts-ipaexfont-gothic \
  fonts-lmodern \
  fonts-junicode \
  texlive-fonts-recommended \
  texlive-fonts-extra

RUNTIME="$HOME/.satysfi"

mkdir -p "$RUNTIME"
cp -r lib-satysfi/* "$RUNTIME"/

mkdir -p "$RUNTIME/dist/fonts/"
cp \
  /usr/share/fonts/opentype/ipaexfont-mincho/ipaexm.ttf \
  /usr/share/fonts/opentype/ipaexfont-gothic/ipaexg.ttf \
  /usr/share/texmf/fonts/opentype/public/lm/lmroman10-regular.otf \
  /usr/share/texmf/fonts/opentype/public/lm/lmroman10-bold.otf \
  /usr/share/texmf/fonts/opentype/public/lm/lmroman10-italic.otf \
  /usr/share/texmf/fonts/opentype/public/lm/lmmono10-regular.otf \
  /usr/share/texmf/fonts/opentype/public/lm/lmsans10-regular.otf \
  /usr/share/fonts/truetype/junicode/Junicode.ttf \
  /usr/share/fonts/truetype/junicode/Junicode-Bold.ttf \
  /usr/share/fonts/truetype/junicode/Junicode-Italic.ttf \
  /usr/share/texlive/texmf-dist/fonts/opentype/public/lm-math/latinmodern-math.otf \
  "$RUNTIME/dist/fonts/"

cp \
  /usr/share/texlive/texmf-dist/fonts/opentype/public/Asana-Math/Asana-Math.otf \
  "$RUNTIME/dist/fonts/Asana-math.otf"
