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
#	first.pdf \
#	math1.pdf \
#	refactor4.pdf \
#	refactor6.pdf \

EXPECTED_LOCKS=$(TARGETS:.pdf=.satysfi-lock-expected)

SATYSFI ?= satysfi

.PHONY: all promote clean

.SUFFIXES: .saty .pdf

.SUFFIXES: .saty .pdf .satysfi-lock .satysfi-lock-expected

# Keeps intermediate results:
.PRECIOUS: %.satysfi-lock

# Generates a lock file from a document and checks that it is as expected:
.saty.satysfi-lock:
	$(SATYSFI) solve $<
	diff $(<:.saty=.satysfi-lock) $(<:.saty=.satysfi-lock-expected)

# Typesets a document:
%.pdf: %.saty %.satysfi-lock
	$(SATYSFI) build $< -o $@

# Promote a lock file to the corresponding expected lock file:
%.satysfi-lock-expected: %.satysfi-lock
	cp $*.satysfi-lock $*.satysfi-lock-expected

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

clean::
	rm -f *.pdf *.satysfi-aux *.satysfi-lock

clean::
	(cd images; make clean)
	(cd text_mode; make clean)
	(cd md; make clean)

clip.pdf: head.satyh
first.pdf: head.satyh
glue1.pdf: head.satyh
math1.pdf: head.satyh
math2.pdf: head.satyh
