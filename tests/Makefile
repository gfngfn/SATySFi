INIT_DOC = init-doc/foo
INIT_MD = init-md/foo
INIT_LIB = init-lib
LOCAL_FIXED = local-fixed/doc

INTERMED_DIR = _build

DOC_TARGETS = \
	clip.pdf \
	glue1.pdf \
	math2.pdf \
        math-typefaces.pdf \
	refactor1.pdf \
	refactor2.pdf \
	refactor3.pdf \
	refactor5.pdf \
	staged1.pdf \
	macro1.pdf \
	rename-dep1.pdf \
	$(INIT_DOC).pdf \
	$(INIT_MD).pdf \
	$(LOCAL_FIXED).pdf \
#	first.pdf \
#	math1.pdf \
#	refactor4.pdf \
#	refactor6.pdf \

LIB_TARGETS = \
	$(INIT_LIB)/$(INTERMED_DIR)/satysfi-deps.yaml \

EXPECTED_LOCKS = $(DOC_TARGETS:.pdf=.saphe.lock.yaml.expected)

SAPHE ?= saphe

.PHONY: all promote clean

.SUFFIXES: .saty .pdf .saphe.lock.yaml .saphe.lock.yaml.expected

# Keeps intermediate results:
.PRECIOUS: %.saphe.lock.yaml

# Generates a lock file from a document and checks that it is as expected:
%.saphe.lock.yaml: %.saty %.saphe.yaml
	$(SAPHE) solve $<
	diff $(<:.saty=.saphe.lock.yaml) $(<:.saty=.saphe.lock.yaml.expected)

# Typesets a document:
%.pdf: %.saty %.saphe.lock.yaml
	$(SAPHE) build $< -o $@

# Promote a lock file to the corresponding expected lock file:
%.saphe.lock.yaml.expected: %.saphe.lock.yaml
	cp $*.saphe.lock.yaml $*.saphe.lock.yaml.expected

# Entrypoint for typesetting documents:
all:: $(DOC_TARGETS) $(LIB_TARGETS)

all::
	(cd images; make)
	(cd text_mode; make)
	(cd md; make)

# Entrypoint for promoting lock files:
promote:: $(EXPECTED_LOCKS)

promote::
	(cd images; make promote)
	(cd text_mode; make promote)
	(cd md; make promote)

clean:: clean-init
	rm -f *.saphe.lock.yaml $(INTERMED_DIR)/*.satysfi-deps.yaml $(INTERMED_DIR)/*.satysfi-aux.yaml *.pdf

clean::
	(cd images; make clean)
	(cd text_mode; make clean)
	(cd md; make clean)

clean-init::
	rm -f $(INIT_DOC).saty $(INIT_DOC).saphe.yaml
	rm -f $(INIT_DOC).saphe.lock.yaml $(INIT_DOC)/$(INTERMED_DIR)/.satysfi-deps.yaml $(INIT_DOC)$(INTERMED_DIR)/.satysfi-aux.yaml $(INIT_DOC).pdf
	rm -f $(INIT_MD).md $(INIT_MD).saphe.yaml
	rm -f $(INIT_MD).saphe.lock.yaml $(INIT_MD)/$(INTERMED_DIR)/.satysfi-deps.yaml $(INIT_MD)/$(INTERMED_DIR).satysfi-aux.yaml $(INIT_MD).pdf
	rm -rf $(INIT_LIB)
	rm -f $(LOCAL_FIXED).saphe.lock.yaml

# Dependencies on program files:
clip.pdf: head.satyh
first.pdf: head.satyh
glue1.pdf: head.satyh
math1.pdf: head.satyh
math2.pdf: head.satyh

# Tests for `saphe init`:
$(INIT_DOC).saty $(INIT_DOC).saphe.yaml:
	$(SAPHE) init document $(INIT_DOC).saty

$(INIT_DOC).saphe.lock.yaml: $(INIT_DOC).saty $(INIT_DOC).saphe.yaml
	$(SAPHE) solve $<
	diff $@ $(INIT_DOC).saphe.lock.yaml.expected

$(INIT_DOC).pdf: $(INIT_DOC).saty $(INIT_DOC).saphe.lock.yaml
	$(SAPHE) build $< -o $@

$(INIT_MD).md $(INIT_MD).saphe.yaml:
	$(SAPHE) init document $(INIT_MD).md

$(INIT_MD).saphe.lock.yaml: $(INIT_MD).md $(INIT_MD).saphe.yaml
	$(SAPHE) solve $<
	diff $@ $(INIT_MD).saphe.lock.yaml.expected

$(INIT_MD).pdf: $(INIT_MD).md $(INIT_MD).saphe.lock.yaml
	$(SAPHE) build $< -o $@

$(INIT_LIB)/saphe.yaml:
	$(SAPHE) init library $(INIT_LIB)

$(INIT_LIB)/saphe.lock.yaml: $(INIT_LIB)/saphe.yaml
	$(SAPHE) solve $(INIT_LIB)

$(INIT_LIB)/$(INTERMED_DIR)/satysfi-deps.yaml: $(INIT_LIB)/saphe.lock.yaml
	$(SAPHE) build $(INIT_LIB)
	$(SAPHE) test $(INIT_LIB)

# Tests for local fixed packages:
$(LOCAL_FIXED).saphe.lock.yaml: $(LOCAL_FIXED).saty $(LOCAL_FIXED).saphe.yaml
	$(SAPHE) solve $<
	diff $@ $(LOCAL_FIXED).saphe.lock.yaml.expected

$(LOCAL_FIXED).pdf: $(LOCAL_FIXED).saty $(LOCAL_FIXED).saphe.lock.yaml
	$(SAPHE) build $< -o $@
