bin/macrodown.exe : types.ml mcdlexer.mli mcdlexer.ml stacklist.mli stacklist.ml mcdparser.mli mcdparser.ml mcdabs.mli mcdabs.ml assoclist.mli assoclist.ml mcdout.mli mcdout.ml mcdsemantics.mli mcdsemantics.ml mcdmain.ml
	ocamlc.opt -o bin/macrodown.exe types.ml mcdlexer.mli mcdlexer.ml stacklist.mli stacklist.ml mcdparser.mli mcdparser.ml mcdabs.mli mcdabs.ml assoclist.mli assoclist.ml mcdout.mli mcdout.ml mcdsemantics.mli mcdsemantics.ml mcdmain.ml

