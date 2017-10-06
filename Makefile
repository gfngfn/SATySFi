PREFIX=/usr/local
BACKEND=backend
MAINSRC=src/frontend
EXTERNAL=external
OCB_FLAGS = -use-ocamlfind -use-menhir -I $(MAINSRC)/ -I $(BACKEND)/ -I $(EXTERNAL)/otfm/src/ -I $(EXTERNAL)/camlpdf/ -pkgs "str,core,ctypes,result,uutf,ucorelib" -tag thread -lflags "flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c"
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
	rm -f $(MAINSRC)/lexer.ml $(MAINSRC)/parser.mli $(MAINSRC)/parser.ml $(MAINSRC)/*.cmi $(MAINSRC)/*.cmx $(MAINSRC)/*.o

.PHONY: clean clean-sub
