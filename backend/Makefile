OCBFLAGS = ocamlbuild -use-ocamlfind -pkgs "core,otfm,camlpdf,ctypes" -tag thread

fontInfo.native: fontInfo.ml
	$(OCBFLAGS) fontInfo.native

horzBox.native: horzBox.ml
	$(OCBFLAGS) horzBox.native

lineBreak.native: lineBreak.ml flowGraph.ml horzBox.ml handlePdf.ml fontInfo.ml base85.ml mutableSet.ml
	$(OCBFLAGS) lineBreak.native

