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
GENCODE_RB=gen_code.rb
INSTDEF_YAML=$(BYTECOMP)/vminstdef.yaml
DUNE=dune
INSTTYPE_GEN=$(FRONTEND)/__insttype.gen.ml
ATTYPE_GEN=$(FRONTEND)/__attype.gen.ml
VM_GEN=$(BYTECOMP)/__vm.gen.ml
IR_GEN=$(BYTECOMP)/__ir.gen.ml
EVAL_GEN=$(FRONTEND)/__evaluator.gen.ml
PRIM_PDF_GEN=$(FRONTEND)/__primitives_pdf_mode.gen.ml
PRIM_TEXT_GEN=$(FRONTEND)/__primitives_text_mode.gen.ml
GENS= \
  $(INSTTYPE_GEN) \
  $(ATTYPE_GEN) \
  $(VM_GEN) \
  $(IR_GEN) \
  $(EVAL_GEN) \
  $(PRIM_PDF_GEN) \
  $(PRIM_TEXT_GEN)
GENCODE_DIR=tools/gencode
GENCODE_EXE=gencode.exe
GENCODE_BIN=$(GENCODE_DIR)/_build/default/$(GENCODE_EXE)
GENCODE=$(DUNE) exec --root $(GENCODE_DIR) ./$(GENCODE_EXE) --
INSTDEF=$(GENCODE_DIR)/vminst.ml

.PHONY: all gen install lib uninstall clean

all: gen
	$(DUNE) build
	cp _build/install/default/bin/$(TARGET) .

$(INSTDEF): $(INSTDEF_YAML)
	$(RUBY) $(GENCODE_RB) --ml $< > $@

gen: $(GENS)

$(ATTYPE_GEN): $(INSTDEF)
	$(GENCODE) --gen-attype > $@

$(INSTTYPE_GEN): $(INSTDEF)
	$(GENCODE) --gen-insttype > $@

$(VM_GEN): $(INSTDEF)
	$(GENCODE) --gen-vm > $@

$(IR_GEN): $(INSTDEF)
	$(GENCODE) --gen-ir > $@

$(EVAL_GEN): $(INSTDEF)
	$(GENCODE) --gen-interps > $@

$(PRIM_PDF_GEN): $(INSTDEF)
	$(GENCODE) --gen-pdf-mode-prims > $@

$(PRIM_TEXT_GEN): $(INSTDEF)
	$(GENCODE) --gen-text-mode-prims > $@

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
	install -d $(LIBDIR)/dist/md
	install -m 644 lib-satysfi/dist/md/* $(LIBDIR)/dist/md

install-fonts:
	mkdir -p temp
# Latin Modern
	wget -O temp/lm2.004otf.zip http://www.gust.org.pl/projects/e-foundry/latin-modern/download/lm2.004otf.zip
	unzip -o temp/lm2.004otf.zip *.otf -d lib-satysfi/dist/fonts/
# Latin Modern Math
	wget -O temp/latinmodern-math-1959.zip http://www.gust.org.pl/projects/e-foundry/lm-math/download/latinmodern-math-1959.zip
	unzip -o temp/latinmodern-math-1959.zip *.otf -d temp/
	cp temp/latinmodern-math-1959/otf/latinmodern-math.otf lib-satysfi/dist/fonts/
# Junicode
	wget -O temp/junicode-1.002.zip http://downloads.sourceforge.net/project/junicode/junicode/junicode-1.002/junicode-1.002.zip
	unzip -o temp/junicode-1.002.zip *.ttf -d lib-satysfi/dist/fonts/
# IPAexfont
	wget -O temp/IPAexfont00301.zip https://oscdl.ipa.go.jp/IPAexfont/IPAexfont00301.zip
	unzip -o temp/IPAexfont00301.zip *.ttf -d temp/
	cp temp/IPAexfont00301/ipaexg.ttf lib-satysfi/dist/fonts/
	cp temp/IPAexfont00301/ipaexm.ttf lib-satysfi/dist/fonts/

#preliminary:
#	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"

uninstall:
	rm -rf $(BINDIR)/$(TARGET)
	rm -rf $(LIBDIR)

clean:
	$(DUNE) clean
	$(RM) $(GENS)
	$(RM) satysfi
