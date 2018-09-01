export PACKS = camlpdf
export OCAMLBCFLAGS = -g -w -3
export OCAMLNCFLAGS = -g -w -3
export OCAMLLDFLAGS = -g

define PROJ_pdfhello
  SOURCES=pdfhello.ml
  RESULT=pdfhello
endef
export PROJ_pdfhello

define PROJ_pdfdecomp
  SOURCES=pdfdecomp.ml
  RESULT=pdfdecomp
endef
export PROJ_pdfdecomp

define PROJ_pdfmerge
  SOURCES=pdfmergeexample.ml
  RESULT=pdfmergeexample
endef
export PROJ_pdfmerge

define PROJ_pdfdraft
  SOURCES=pdfdraft.ml
  RESULT=pdfdraft
endef
export PROJ_pdfdraft

define PROJ_pdftest
  SOURCES=pdftest.ml
  RESULT=pdftest
endef
export PROJ_pdftest

define PROJ_pdfencrypt
  SOURCES=pdfencrypt.ml
  RESULT=pdfencrypt
endef
export PROJ_pdfencrypt

ifndef SUBPROJS
  export SUBPROJS = pdfhello pdfdecomp pdfmerge pdfdraft pdftest pdfencrypt
endif

all: native-code

%:
	@make -f OCamlMakefile subprojs SUBTARGET=$@

