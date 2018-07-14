PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
SRCROOT=src
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
BYTECOMP=$(FRONTEND)/bytecomp
TARGET=satysfi
BINDIR=$(PREFIX)/bin
RM=rm -f
RUBY=ruby
GENCODE=./gen_code.rb
DUNE=dune
INSTDEF=$(BYTECOMP)/vminstdef.yaml
INSTTYPE_GEN=$(FRONTEND)/__insttype.gen.ml
ATTYPE_GEN=$(FRONTEND)/__attype.gen.ml
VM_GEN=$(BYTECOMP)/__vm.gen.ml
IR_GEN=$(BYTECOMP)/__ir.gen.ml
EVAL_GEN=$(FRONTEND)/__evaluator.gen.ml
PRIM_GEN=$(FRONTEND)/__primitives.gen.ml
GENS= \
  $(INSTTYPE_GEN) \
  $(ATTYPE_GEN) \
  $(VM_GEN) \
  $(IR_GEN) \
  $(EVAL_GEN) \
  $(PRIM_GEN)

.PHONY: all gen install lib uninstall clean

all: gen
	$(DUNE) build
	cp _build/install/default/bin/$(TARGET) .

gen: $(GENS)

$(ATTYPE_GEN): $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-attype $(INSTDEF) > $@

$(INSTTYPE_GEN): $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-insttype $(INSTDEF) > $@

$(VM_GEN): $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-vm $(INSTDEF) > $@

$(IR_GEN): $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-ir $(INSTDEF) > $@

$(EVAL_GEN): $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-interps $(INSTDEF) > $@

$(PRIM_GEN): $(INSTDEF) $(GENCODE)
	$(RUBY) $(GENCODE) --gen-prims $(INSTDEF) > $@

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
	  curl -L -R -o temp/lm2.004otf.zip http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip; \
	  curl -L -R -o temp/latinmodern-math-1959.zip http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip; \
	  curl -L -R -o temp/junicode-1.002.zip http://downloads.sourceforge.net/project/junicode/junicode/junicode-1.002/junicode-1.002.zip; \
	  curl -L -R -o temp/IPAexfont00301.zip https://oscdl.ipa.go.jp/IPAexfont/IPAexfont00301.zip; \
	else \
	  wget -N http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip -P temp/; \
	  wget -N http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip -P temp/; \
	  wget -N http://downloads.sourceforge.net/project/junicode/junicode/junicode-1.002/junicode-1.002.zip -P temp/; \
	  wget -N https://oscdl.ipa.go.jp/IPAexfont/IPAexfont00301.zip -P temp/; \
        fi
	unzip -o temp/lm2.004otf.zip -d lib-satysfi/dist/fonts/
	unzip -o temp/latinmodern-math-1959.zip -d temp/
	cp temp/latinmodern-math-1959/otf/latinmodern-math.otf lib-satysfi/dist/fonts/
	unzip -o temp/junicode-1.002.zip -d temp/
	cp temp/Junicode.ttf lib-satysfi/dist/fonts/
	cp temp/Junicode-Bold.ttf lib-satysfi/dist/fonts/
	cp temp/Junicode-Italic.ttf lib-satysfi/dist/fonts/
	cp temp/Junicode-BoldItalic.ttf lib-satysfi/dist/fonts/
	unzip -o temp/IPAexfont00301.zip -d temp/
	cp temp/IPAexfont00301/ipaexg.ttf lib-satysfi/dist/fonts/
	cp temp/IPAexfont00301/ipaexm.ttf lib-satysfi/dist/fonts/

uninstall:
	rm -rf $(BINDIR)/$(TARGET)
	rm -rf $(LIBDIR)

clean:
	$(DUNE) clean
	$(RM) $(GENS)
	$(RM) satysfi
