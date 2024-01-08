TARGETS = \
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
	init-doc/foo.pdf \
	init-md/foo.pdf \
#	first.pdf \
#	math1.pdf \
#	refactor4.pdf \
#	refactor6.pdf \

EXPECTED_LOCKS=$(TARGETS:.pdf=.saphe.lock.yaml.expected)

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
all:: $(TARGETS)

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
	rm -f *.pdf *.satysfi-aux *.saphe.lock.yaml *.satysfi-deps.yaml

clean::
	(cd images; make clean)
	(cd text_mode; make clean)
	(cd md; make clean)

clean-init::
	rm -f init-doc/*.saty init-doc/*.saphe.yaml
	rm -f init-md/*.md init-md/*.saphe.yaml

clip.pdf: head.satyh
first.pdf: head.satyh
glue1.pdf: head.satyh
math1.pdf: head.satyh
math2.pdf: head.satyh

init-doc/foo.saty init-doc/foo.saphe.yaml:
	saphe init document init-doc/foo.saty

init-md/foo.md init-md/foo.saphe.yaml:
	saphe init document init-md/foo.md
