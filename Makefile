mcd.exe : types.ml mcdlexer.mli mcdlexer.ml stacklist.mli stacklist.ml mcdparser.mli mcdparser.ml mcdabs.mli mcdabs.ml assoclist.mli assoclist.ml mcdsemantics.mli mcdsemantics.ml mcdout.mli mcdout.ml mcdmain.ml
	ocamlc -o mcd.exe types.ml mcdlexer.mli mcdlexer.ml stacklist.mli stacklist.ml mcdparser.mli mcdparser.ml mcdabs.mli mcdabs.ml assoclist.mli assoclist.ml mcdsemantics.mli mcdsemantics.ml mcdout.mli mcdout.ml mcdmain.ml

