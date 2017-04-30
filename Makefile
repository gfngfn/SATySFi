OCB_FLAGS = -use-ocamlfind -use-menhir -I src/
OCB = ocamlbuild $(OCB_FLAGS)
BINDIR=$(PREFIX)/bin

all: macrodown

macrodown:
	$(OCB) main.native
	mv main.native macrodown

install: macrodown
	mkdir -p $(BINDIR)
	install ./macrodown $(BINDIR)


clean:
	$(OCB) -clean

clean-sub:
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmx *.o

.PHONY: clean clean-sub
