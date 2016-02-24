SRCS=stacklist.mli stacklist.ml types.ml typeenv.mli typeenv.ml display.mli display.ml variantenv.mli variantenv.ml subst.mli subst.ml parser.mli parser.ml lexer.ml primitives.mli primitives.ml typechecker.mli typechecker.ml out.mli out.ml evaluator.mli evaluator.ml files.ml main.mli main.ml

ifeq ($(OS), Windows_NT)
  TARGET=bin/macrodown.exe
else
  TARGET=bin/macrodown
endif
all: lexer.ml parser.mli parser.ml types.cmo $(TARGET)
lexer.ml: lexer.mll
	ocamllex lexer.mll
parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly
types.cmo: $(SRCS)
	ocamlc -c $^
$(TARGET): $(SRCS)
	ocamlopt -o $(TARGET) $^

clean:
	rm -f $(TARGET)

.PHONY: clean
