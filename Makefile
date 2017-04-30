OCB_FLAGS = -use-ocamlfind -use-menhir -I src/
OCB = ocamlbuild $(OCB_FLAGS)
BINDIR=$(PREFIX)/bin

TARGET=macrodown

all: macrodown

macrodown:
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
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmx *.o

.PHONY: clean clean-sub
