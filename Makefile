PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
SRCROOT=src
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
EXTERNAL=external
OCB_FLAGS = -cflags -unsafe-string -cflag -w -cflag -3 \
	-use-ocamlfind -use-menhir \
	-I $(SRCROOT)/ -I $(FRONTEND)/ -I $(BACKEND)/ -I $(CHARDECODER)/ \
	-I $(EXTERNAL)/otfm/src/ -I $(EXTERNAL)/camlpdf/ \
	-pkgs "ppx_deriving.show,core_kernel,result,uutf,batteries, \
	menhirLib,yojson,camlimages,camlimages.jpeg" \
	-tag thread -yaccflags "--table --explain" \
	-lflags "flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c"
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
	install -d $(LIBDIR)
	install -d $(LIBDIR)/dist
	install -d $(LIBDIR)/dist/unidata
	install -m 644 lib-satysfi/dist/unidata/*.txt $(LIBDIR)/dist/unidata
	install -d $(LIBDIR)/dist/fonts
	install -m 644 lib-satysfi/dist/fonts/* $(LIBDIR)/dist/fonts
	install -d $(LIBDIR)/dist/hash
	install -m 644 lib-satysfi/dist/hash/* $(LIBDIR)/dist/hash
	install -d $(LIBDIR)/dist/hyph
	install -m 644 lib-satysfi/dist/hyph/* $(LIBDIR)/dist/hyph
	install -d $(LIBDIR)/dist/packages
	install -m 644 lib-satysfi/dist/packages/* $(LIBDIR)/dist/packages

#preliminary:
#	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"

lib:
# -- downloads fonts --
	mkdir -p temp/
	wget -N http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip -P temp/
	unzip -o temp/lm2.004otf.zip -d lib-satysfi/dist/fonts/
	wget -N http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip -P temp/
	unzip -o temp/latinmodern-math-1959.zip -d temp/
	cp temp/latinmodern-math-1959/otf/latinmodern-math.otf lib-satysfi/dist/fonts/

uninstall:
	rm -rf $(BINDIR)/$(TARGET)
	rm -rf $(LIBDIR)

clean:
	$(OCB) -clean

#clean-sub:
#	rm -f $(FRONTEND)/lexer.ml $(FRONTEND)/parser.mli $(FRONTEND)/parser.ml $(FRONTEND)/*.cmi $(FRONTEND)/*.cmx $(FRONTEND)/*.o
#
.PHONY: clean
