PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
SRCROOT=src
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
OCB_FLAGS = -cflags -w,-3 \
	-use-ocamlfind -use-menhir \
	-I $(SRCROOT)/ -I $(FRONTEND)/ -I $(BACKEND)/ -I $(CHARDECODER)/ \
	-pkgs "str,ppx_deriving.show,core_kernel,uutf,batteries, \
	menhirLib,yojson,camlimages,camlimages.jpeg,otfm,camlpdf" \
	-tag bin_annot -tag thread -tag unsafe_string -yaccflags "--table --explain"
TARGET=satysfi
OCB = ocamlbuild $(OCB_FLAGS)
BINDIR=$(PREFIX)/bin

all:
	mkdir -p _build/
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
	if [ -x "$$(command -v curl)" ]; then \
	  curl -R -o temp/lm2.004otf.zip http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip; \
	  curl -R -o temp/latinmodern-math-1959.zip http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip; \
	else \
	  wget -N http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip -P temp/; \
	  wget -N http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip -P temp/; \
        fi
	unzip -o temp/lm2.004otf.zip -d lib-satysfi/dist/fonts/
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
