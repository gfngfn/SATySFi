BINDIR=$(PREFIX)/bin
OCB_FLAGS = -use-ocamlfind -use-menhir -I src/
TARGET=macrodown
OCB = ocamlbuild $(OCB_FLAGS)

all: $(TARGET)

$(TARGET):
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
	rm -f src/lexer.ml src/parser.mli src/parser.ml src/*.cmi src/*.cmx src/*.o

.PHONY: clean clean-sub
