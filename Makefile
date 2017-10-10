PREFIX=/usr/local
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
EXTERNAL=external
OCB_FLAGS = -use-ocamlfind -use-menhir -I $(FRONTEND)/ -I $(BACKEND)/ -I $(CHARDECODER)/ -I $(EXTERNAL)/otfm/src/ -I $(EXTERNAL)/camlpdf/ -I $(EXTERNAL)/ucorelib/src/ -pkgs "str,core,ctypes,result,uutf,bitv,batteries" -tag thread -lflags "flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c"
TARGET=satysfi
OCB = ocamlbuild $(OCB_FLAGS)
BINDIR=$(PREFIX)/bin

all:
	cp $(EXTERNAL)/camlpdf/*.c ./
	cp $(EXTERNAL)/camlpdf/*.h ./
	mv *.c _build/
	mv *.h _build/
	$(OCB) main.native
	mv main.native $(TARGET)

install: macrodown
	mkdir -p $(BINDIR)
	install $(TARGET) $(BINDIR)

uninstall:
	rm -rf $(BINDIR)/$(TARGET)

clean:
	$(OCB) -clean

clean-sub:
	rm -f $(FRONTEND)/lexer.ml $(FRONTEND)/parser.mli $(FRONTEND)/parser.ml $(FRONTEND)/*.cmi $(FRONTEND)/*.cmx $(FRONTEND)/*.o

.PHONY: clean clean-sub
