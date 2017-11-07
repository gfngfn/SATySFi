PREFIX=/usr/local
PREFIX_LIB=/usr/local
SRCROOT=src
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
EXTERNAL=external
OCB_FLAGS = -use-ocamlfind -use-menhir -I $(SRCROOT)/ -I $(FRONTEND)/ -I $(BACKEND)/ -I $(CHARDECODER)/ -I $(EXTERNAL)/otfm/src/ -I $(EXTERNAL)/camlpdf/ -I $(EXTERNAL)/ucorelib/src/ -pkgs "ppx_deriving.show,core,ctypes,result,uutf,bitv,batteries,menhirLib" -tag thread -lflags "flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c" -yaccflags "--table --explain"
TARGET=satysfi
OCB = ocamlbuild $(OCB_FLAGS)
BINDIR=$(PREFIX)/bin

all:
	mkdir -p _build/
	cp $(EXTERNAL)/camlpdf/*.c _build/
	cp $(EXTERNAL)/camlpdf/*.h _build/
	$(OCB) main.native
	mv main.native $(TARGET)

install: $(TARGET)
	mkdir -p $(BINDIR)
	install $(TARGET) $(BINDIR)

install-lib:
	ln -s -i `pwd`/lib-satysfi $(PREFIX_LIB)/lib-satysfi

uninstall:
	rm -rf $(BINDIR)/$(TARGET)

clean:
	$(OCB) -clean

#clean-sub:
#	rm -f $(FRONTEND)/lexer.ml $(FRONTEND)/parser.mli $(FRONTEND)/parser.ml $(FRONTEND)/*.cmi $(FRONTEND)/*.cmx $(FRONTEND)/*.o
#
.PHONY: clean

lb:
	$(OCB) lineBreakDataMap.native

