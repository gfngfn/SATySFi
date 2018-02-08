PREFIX=/usr/local
PREFIX_LIB=/usr/local
SRCROOT=src
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
EXTERNAL=external
OCB_FLAGS = -use-ocamlfind -use-menhir -I $(SRCROOT)/ -I $(FRONTEND)/ -I $(BACKEND)/ -I $(CHARDECODER)/ -I $(EXTERNAL)/otfm/src/ -I $(EXTERNAL)/camlpdf/ -I $(EXTERNAL)/ucorelib/src/ -pkgs "ppx_deriving.show,core,result,uutf,bitv,batteries,menhirLib,yojson,camlimages,camlimages.jpeg" -tag thread -lflags "flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c" -yaccflags "--table --explain"
TARGET=satysfi
OCB = ocamlbuild $(OCB_FLAGS)
BINDIR=$(PREFIX)/bin

all:
	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"
	mkdir -p _build/
	cp $(EXTERNAL)/camlpdf/*.c _build/
	cp $(EXTERNAL)/camlpdf/*.h _build/
	$(OCB) main.native
	mv main.native $(TARGET)

install: $(TARGET)
	mkdir -p $(BINDIR)
	install $(TARGET) $(BINDIR)
	install -d $(PREFIX_LIB)/lib-satysfi
	install -d $(PREFIX_LIB)/lib-satysfi/dist
	install -d $(PREFIX_LIB)/lib-satysfi/dist/unidata
	install -m 644 lib-satysfi/dist/unidata/*.txt $(PREFIX_LIB)/lib-satysfi/dist/unidata
	install -d $(PREFIX_LIB)/lib-satysfi/dist/fonts
	install -m 644 lib-satysfi/dist/fonts/* $(PREFIX_LIB)/lib-satysfi/dist/fonts
	install -d $(PREFIX_LIB)/lib-satysfi/dist/hash
	install -m 644 lib-satysfi/dist/hash/* $(PREFIX_LIB)/lib-satysfi/dist/hash
	install -d $(PREFIX_LIB)/lib-satysfi/dist/hyph
	install -m 644 lib-satysfi/dist/hyph/* $(PREFIX_LIB)/lib-satysfi/dist/hyph
	install -d $(PREFIX_LIB)/lib-satysfi/dist/packages
	install -m 644 lib-satysfi/dist/packages/* $(PREFIX_LIB)/lib-satysfi/dist/packages

#preliminary:
#	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"

lib:
# -- downloads UNIDATA --
	wget -N http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt       -P lib-satysfi/dist/unidata/
	wget -N http://www.unicode.org/Public/UNIDATA/LineBreak.txt            -P lib-satysfi/dist/unidata/
	wget -N http://www.unicode.org/Public/UNIDATA/PropList.txt             -P lib-satysfi/dist/unidata/
	wget -N http://www.unicode.org/Public/UNIDATA/PropertyAliases.txt      -P lib-satysfi/dist/unidata/
	wget -N http://www.unicode.org/Public/UNIDATA/PropertyValueAliases.txt -P lib-satysfi/dist/unidata/
	wget -N http://www.unicode.org/Public/UNIDATA/Scripts.txt              -P lib-satysfi/dist/unidata/
	wget -N http://www.unicode.org/Public/UNIDATA/ScriptExtensions.txt     -P lib-satysfi/dist/unidata/
	wget -N http://www.unicode.org/Public/UNIDATA/UnicodeData.txt          -P lib-satysfi/dist/unidata/
# -- downloads fonts --
	mkdir -p temp/
	wget -N http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip -P temp/
	unzip -o temp/lm2.004otf.zip -d lib-satysfi/dist/fonts/
	wget -N http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip -P temp/
	unzip -o temp/latinmodern-math-1959.zip -d temp/
	cp temp/latinmodern-math-1959/otf/latinmodern-math.otf lib-satysfi/dist/fonts/

uninstall:
	rm -rf $(BINDIR)/$(TARGET)
	rm -rf $(PREFIX_LIB)/lib-satysfi

clean:
	$(OCB) -clean

#clean-sub:
#	rm -f $(FRONTEND)/lexer.ml $(FRONTEND)/parser.mli $(FRONTEND)/parser.ml $(FRONTEND)/*.cmi $(FRONTEND)/*.cmx $(FRONTEND)/*.o
#
.PHONY: clean
