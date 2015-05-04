SRCS=types.ml files.ml sequence.mli sequence.ml mcdlexer.mli mcdlexer.ml stacklist.mli stacklist.ml mcdparser.mli mcdparser.ml mcdabs.mli mcdabs.ml assoclist.mli assoclist.ml mcdout.mli mcdout.ml mcdsemantics.mli mcdsemantics.ml mcdmain.ml

ifeq ($(OS), Windows_NT)
  TARGET=bin/macrodown.exe
else
  TARGET=bin/macrodown
endif

$(TARGET): $(SRCS)
	ocamlc -o $(TARGET) $^

clean:
	rm -f $(TARGET)

.PHONY: clean
