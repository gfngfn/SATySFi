* Should be refactored. Get the importants bits during 
  init (e.g. loca format for TTF, index of important tables). 
  
* Use bigarrays of bytes rather than strings, this would 
  allow to use mmap.
  
* Support for TTC should be easy, detect and offer to decode
  only one.
  
* Harmonize style to start with >>=.
* The mutable fields in the decoder could lead to problems 
  if someone tries to do something with the decoder during
  folds (e.g. looking up the outlines during the cmap table fold).


* testing 
`mdfind -0 "kMDItemContentType = "public.truetype-ttf-font"" \
 | xargs -0 ./otftrip.native > /dev/null`

