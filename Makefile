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

#preliminary:
#	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"

uninstall:
	rm -rf $(BINDIR)/$(TARGET)
	rm -rf $(LIBDIR)

clean:
	$(DUNE) clean
	$(RM) $(GENS)
	$(RM) satysfi
