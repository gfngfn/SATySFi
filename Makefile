PREFIX=/usr/local
PREFIX_LIB=/usr/local
SRCROOT=src
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
EXTERNAL=external
OCB_FLAGS = -use-ocamlfind -use-menhir -I $(SRCROOT)/ -I $(FRONTEND)/ -I $(BACKEND)/ -I $(CHARDECODER)/ -I $(EXTERNAL)/otfm/src/ -I $(EXTERNAL)/camlpdf/ -I $(EXTERNAL)/ucorelib/src/ -pkgs "ppx_deriving.show,core,ctypes,result,uutf,bitv,batteries,menhirLib,yojson,camlimages,camlimages.jpeg" -tag thread -lflags "flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c" -yaccflags "--table --explain"
TARGET=satysfi
OCB = ocamlbuild $(OCB_FLAGS)
BINDIR=$(PREFIX)/bin

all:
	mkdir -p _build/
	cp $(EXTERNAL)/camlpdf/*.c _build/
	cp $(EXTERNAL)/camlpdf/*.h _build/
	$(OCB) main.native
	mv main.native $(TARGET)

install: $(TARGET)
	mkdir -p $(BINDIR)
	install $(TARGET) $(BINDIR)

preliminary:
	opam install -y ppx_deriving
	opam install -y menhir
	opam install -y core.v0.9.1
	opam install -y ctypes
	opam install -y uutf
	opam install -y result
	opam install -y bitv
	opam install -y batteries
	opam install -y yojson
	opam install -y camlimages
	eval `opam config env`
	git submodule update -i

lib:
# -- downloads UNIDATA --
	wget http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt       -O lib-satysfi/dist/unidata/EastAsianWidth.txt
	wget http://www.unicode.org/Public/UNIDATA/LineBreak.txt            -O lib-satysfi/dist/unidata/LineBreak.txt
	wget http://www.unicode.org/Public/UNIDATA/PropList.txt             -O lib-satysfi/dist/unidata/PropList.txt
	wget http://www.unicode.org/Public/UNIDATA/PropertyAliases.txt      -O lib-satysfi/dist/unidata/PropertyAliases.txt
	wget http://www.unicode.org/Public/UNIDATA/PropertyValueAliases.txt -O lib-satysfi/dist/unidata/PropertyValueAliases.txt
	wget http://www.unicode.org/Public/UNIDATA/Scripts.txt              -O lib-satysfi/dist/unidata/Scripts.txt
	wget http://www.unicode.org/Public/UNIDATA/ScriptExtensions.txt     -O lib-satysfi/dist/unidata/ScriptExtensions.txt
	wget http://www.unicode.org/Public/UNIDATA/UnicodeData.txt          -O lib-satysfi/dist/unidata/UnicodeData.txt
# -- downloads fonts --
	mkdir -p temp/
	wget http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip -O temp/lm2.004otf.zip
	unzip temp/lm2.004otf.zip -d lib-satysfi/dist/fonts/
	wget http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip -O temp/latinmodern-math-1959.zip
	unzip temp/latinmodern-math-1959.zip -d temp/
	cp temp/latinmodern-math-1959/otf/latinmodern-math.otf lib-satysfi/dist/fonts/
# -- make a symbolic link for libraries --
	ln -s -i `pwd`/lib-satysfi $(PREFIX_LIB)/lib-satysfi

uninstall:
	rm -rf $(BINDIR)/$(TARGET)

clean:
	$(OCB) -clean

#clean-sub:
#	rm -f $(FRONTEND)/lexer.ml $(FRONTEND)/parser.mli $(FRONTEND)/parser.ml $(FRONTEND)/*.cmi $(FRONTEND)/*.cmx $(FRONTEND)/*.o
#
.PHONY: clean
