PREFIX=/usr/local/bin
OCB=ocamlbuild -use-ocamlfind

all:
	$(OCB) otftrip.native
	mv otftrip.native otftrip-local

gsub:
	$(OCB) gsubtrip.native
	mv gsubtrip.native gsubtrip

cff:
	$(OCB) cfftrip.native
	mv cfftrip.native cfftrip

examples:
	$(OCB) examples.native

subset:
	$(OCB) otfgensubset.native
	mv otfgensubset.native otfgensubset

install:
	install otftrip-local $(PREFIX)
	install gsubtrip $(PREFIX)

clean:
	$(OCB) -clean
