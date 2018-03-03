OCBFLAGS = ocamlbuild -use-ocamlfind -pkgs "core,ctypes" -tag thread
CFLAGS = ocamlc.opt -c

pageBreak.native: pageBreak.ml lineBreak.ml flowGraph.ml horzBox.ml handlePdf.ml fontInfo.ml base85.ml mutableSet.ml
	$(OCBFLAGS) pageBreak.native

all:
	cp external/camlpdf/*.c ./
	cp external/camlpdf/*.h ./
#	$(CFLAGS) flatestubs.c
#	$(CFLAGS) rijndael-alg-fst.c
#	$(CFLAGS) stubs-aes.c
#	$(CFLAGS) miniz.c
#	$(CFLAGS) sha2.c
#	$(CFLAGS) stubs-sha2.c
#	ocamlc -c external/camlpdf/flatestubs.c
#	ocamlc -c external/camlpdf/rijndael-alg-fst.c
#	ocamlc -c external/camlpdf/stubs-aes.c
#	ocamlc -c external/camlpdf/miniz.c
#	ocamlc -c external/camlpdf/sha2.c
#	ocamlc -c external/camlpdf/stubs-sha2.c
	mv *.c _build/
	mv *.h _build/
#	ocamlbuild -use-ocamlfind -use-menhir -tag thread -pkgs "core,ctypes,otfm" -lflags "external/camlpdf/flatestubs.o external/camlpdf/rijndael-alg-fst.o external/camlpdf/stubs-aes.o external/camlpdf/miniz.o external/camlpdf/sha2.o external/camlpdf/stubs-sha2.o" -I external/camlpdf/ pageBreak.native
#	ocamlbuild -use-ocamlfind -use-menhir -tag thread -pkgs "core,ctypes,otfm" -lflags "flatestubs.o rijndael-alg-fst.o stubs-aes.o miniz.o sha2.o stubs-sha2.o" -no-hygiene -I external/camlpdf/ pageBreak.native
	ocamlbuild -use-ocamlfind -use-menhir -tag thread -pkgs "core,ctypes,result,uutf" -lflags "flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c" -I external/camlpdf/ -I external/otfm/ pageBreak.native
#	ocamlbuild -use-ocamlfind -use-menhir -tag thread -pkgs "core,ctypes,otfm" -no-hygiene -I external/camlpdf/ pageBreak.native
