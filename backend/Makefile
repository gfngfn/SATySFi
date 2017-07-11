OCBFLAGS = ocamlbuild -use-ocamlfind -pkgs "core,otfm" -tag thread

fontInfo.native: fontInfo.ml
	$(OCBFLAGS) fontInfo.native

horzBox.native: horzBox.ml
	$(OCBFLAGS) horzBox.native

lineBreak.native: lineBreak.ml flowGraph.ml
	$(OCBFLAGS) lineBreak.native

