SRCS=assoc.mli assoc.ml stacklist.mli stacklist.ml tyvarid.mli tyvarid.ml range.mli range.ml types.ml kindenv.mli kindenv.ml typeenv.mli typeenv.ml display.mli display.ml variantenv.mli variantenv.ml subst.mli subst.ml parser.mli parser.ml lexer.ml primitives.mli primitives.ml typechecker.mli typechecker.ml out.mli out.ml evaluator.mli evaluator.ml files.ml main.mli main.ml

ifeq ($(OS), Windows_NT)
  TARGET=bin/macrodown.exe
else
  TARGET=bin/macrodown
endif
all: lexer.ml parser.mli parser.ml types.cmo parser.output $(TARGET)
lexer.ml: lexer.mll
	ocamllex lexer.mll
parser.mli parser.ml parser.output: parser.mly
	ocamlyacc -v parser.mly
types.cmo: $(SRCS)
	ocamlc -c $^
$(TARGET): $(SRCS)
	ocamlopt -o $(TARGET) $^

clean:
	rm -f $(TARGET) lexer.ml parser.mli parser.ml *.cmi *.cmo *.cmx *.o

clean-sub:
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmx *.o

cp:
ifeq ($(OS), Windows_NT)
else
	sudo cp bin/macrodown /usr/bin/macrodown
endif

.PHONY: clean
