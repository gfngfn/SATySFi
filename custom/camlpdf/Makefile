# Build the camlpdf library as byte code and native code
PDFMODS = pdfutil pdfio pdftransform pdfunits pdfpaper pdfcryptprimitives pdf pdfcrypt \
pdfflate pdfcodec pdfwrite pdfgenlex pdfread pdfjpeg pdfops pdfdest \
pdfmarks pdfpagelabels pdfpage pdfannot pdffun pdfspace pdfimage pdfafm \
pdfafmdata pdfglyphlist pdftext pdfstandard14 pdfgraphics pdfshapes pdfdate \
pdfocg pdfcff pdftype1 pdftruetype pdftype0 pdfmerge

SOURCES = flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c $(foreach x,$(PDFMODS),$(x).ml $(x).mli)

PACKS = bigarray

RESULT = camlpdf

LIBINSTALL_FILES = camlpdf.a camlpdf.cma camlpdf.cmxa libcamlpdf_stubs.a \
dllcamlpdf_stubs.* $(foreach x,$(PDFMODS),$x.mli) \
$(foreach x,$(PDFMODS),$x.cmi) $(foreach x,$(PDFMODS),$x.cmx)

OCAMLNCFLAGS = -g -unsafe-string -annot -w -3
OCAMLBCFLAGS = -g -unsafe-string -annot -w -3
OCAMLLDFLAGS = -g

all : native-code-library byte-code-library htdoc

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf

install : libinstall

-include OCamlMakefile

