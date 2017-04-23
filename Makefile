SRCS=src/assoc.mli src/assoc.ml src/stacklist.mli src/stacklist.ml src/range.mli src/range.ml src/types.ml src/typeenv.mli src/typeenv.ml src/variantenv.mli src/variantenv.ml src/display.mli src/display.ml src/parser.mli src/parser.ml src/lexer.ml src/primitives.mli src/primitives.ml src/typechecker.mli src/typechecker.ml src/out.mli src/out.ml src/evaluator.mli src/evaluator.ml src/files.ml src/main.mli src/main.ml

ifeq ($(OS), Windows_NT)
  TARGET=bin/macrodown.exe
else
  TARGET=bin/macrodown
endif
all: src/lexer.ml src/parser.mli src/parser.ml src/types.cmo src/parser.output $(TARGET)
src/lexer.ml: src/lexer.mll
	ocamllex src/lexer.mll
src/parser.mli src/parser.ml src/parser.output: src/parser.mly
	ocamlyacc -v src/parser.mly
src/types.cmo: $(SRCS)
	ocamlc -I src -c $^
$(TARGET): $(SRCS)
	ocamlopt -I src -o $(TARGET) $^

debug: $(SRCS)
	ocamlopt -g -I src -o $(TARGET) -c $^

clean:
	rm -f $(TARGET) src/lexer.ml src/parser.mli src/parser.ml src/*.cmi src/*.cmo src/*.cmx src/*.o

clean-sub:
	rm -f src/lexer.ml src/parser.mli src/parser.ml src/*.cmi src/*.cmx src/*.o

cp:
ifeq ($(OS), Windows_NT)
else
	sudo cp bin/macrodown /usr/bin/macrodown
endif

.PHONY: clean
