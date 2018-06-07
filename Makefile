PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
SRCROOT=src
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
BYTECOMP=$(FRONTEND)/bytecomp
OCB_FLAGS = -cflags -w,-3 \
	-use-ocamlfind -use-menhir \
	-I $(SRCROOT)/ -I $(FRONTEND)/ -I $(BACKEND)/ -I $(CHARDECODER)/ -I $(BYTECOMP) \
	-pkgs "str,ppx_deriving.show,core_kernel,uutf,batteries, \
	menhirLib,yojson,camlimages,camlimages.jpeg,otfm,camlpdf" \
	-tag bin_annot -tag thread -tag unsafe_string -yaccflags "--table --explain"
TARGET=satysfi
OCB = ocamlbuild $(OCB_FLAGS)
BINDIR=$(PREFIX)/bin
RM=rm -f
RUBY=ruby
GENCODE=./gen_code.rb
INSTDEF=$(BYTECOMP)/vminstdef.yaml

all: $(FRONTEND)/types_.ml $(FRONTEND)/evaluator_.ml $(FRONTEND)/primitives_.ml $(BYTECOMP)/vm_.ml $(BYTECOMP)/ir_.ml
	mkdir -p _build/
	$(OCB) main.native
	mv main.native $(TARGET)

$(FRONTEND)/types_.ml: $(FRONTEND)/types_template.ml $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-insttype $(INSTDEF) > $(FRONTEND)/__insttype.ml
	$(RUBY) $(GENCODE) --gen-attype $(INSTDEF) > $(FRONTEND)/__attype.ml
	$(RUBY) $(GENCODE) --pp-include $(FRONTEND)/types_template.ml > $(FRONTEND)/types_.ml
	$(RM)   $(FRONTEND)/__insttype.ml
	$(RM)   $(FRONTEND)/__attype.ml

$(BYTECOMP)/vm_.ml: $(BYTECOMP)/vm_template.ml $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-vm $(INSTDEF) > $(BYTECOMP)/__vm.ml
	$(RUBY) $(GENCODE) --pp-include $(BYTECOMP)/vm_template.ml > $(BYTECOMP)/vm_.ml
	$(RM)   $(BYTECOMP)/__vm.ml

$(BYTECOMP)/ir_.ml: $(BYTECOMP)/ir_template.ml $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-ir $(INSTDEF) > $(BYTECOMP)/__ir.ml
	$(RUBY) $(GENCODE) --pp-include $(BYTECOMP)/ir_template.ml > $(BYTECOMP)/ir_.ml
	$(RM)   $(BYTECOMP)/__ir.ml

$(FRONTEND)/evaluator_.ml: $(FRONTEND)/evaluator_template.ml $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-interps $(INSTDEF) > $(FRONTEND)/__evaluator.ml
	$(RUBY) $(GENCODE) --pp-include $(FRONTEND)/evaluator_template.ml > $(FRONTEND)/evaluator_.ml
	$(RM)   $(FRONTEND)/__evaluator.ml

$(FRONTEND)/primitives_.ml: $(FRONTEND)/primitives_template.ml $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-prims $(INSTDEF) > $(FRONTEND)/__primitives.ml
	$(RUBY) $(GENCODE) --pp-include $(FRONTEND)/primitives_template.ml > $(FRONTEND)/primitives_.ml
	$(RM)   $(FRONTEND)/__primitives.ml

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
	$(RM)  $(FRONTEND)/types_.ml
	$(RM)  $(FRONTEND)/primitives_.ml
	$(RM)  $(FRONTEND)/evaluator_.ml
	$(RM)  $(BYTECOMP)/vm_.ml
	$(RM)  $(BYTECOMP)/ir_.ml
	$(RM)  $(FRONTEND)/__insttype.ml
	$(RM)  $(FRONTEND)/__attype.ml
	$(RM)  $(FRONTEND)/__primitives.ml
	$(RM)  $(FRONTEND)/__evaluator.ml
	$(RM)  $(BYTECOMP)/__vm.ml
	$(RM)  $(BYTECOMP)/__ir.ml


#clean-sub:
#	rm -f $(FRONTEND)/lexer.ml $(FRONTEND)/parser.mli $(FRONTEND)/parser.ml $(FRONTEND)/*.cmi $(FRONTEND)/*.cmx $(FRONTEND)/*.o
#
.PHONY: clean
