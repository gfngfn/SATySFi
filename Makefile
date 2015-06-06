SRCS=types.ml stacklist.mli stacklist.ml parser.mli parser.ml lexer.ml typecheck.ml out.mli out.ml eval.mli eval.ml files.ml main.ml

ifeq ($(OS), Windows_NT)
  TARGET=bin/macrodown.exe
else
  TARGET=bin/macrodown
endif
all: lexer.ml parser.mli parser.ml $(TARGET)
lexer.ml: lexer.mll
	ocamllex lexer.mll
parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly
$(TARGET): $(SRCS)
	ocamlc -o $(TARGET) $^

clean:
	rm -f $(TARGET)

.PHONY: clean
