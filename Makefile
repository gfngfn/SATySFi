OCB_FLAGS = -use-ocamlfind -use-menhir -I src/
OCB = ocamlbuild $(OCB_FLAGS)

all:
	$(OCB) main.native
	mv main.native macrodown

clean:
	$(OCB) -clean

clean-sub:
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmx *.o

.PHONY: clean clean-sub
