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

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .saty .pdf

.saty.pdf:
	$(SATYSFI) $< -o $@

all:: $(TARGETS)

all::
	(cd images; make)
	(cd text_mode; make)
	(cd md; make)

clean::
	rm -f *.pdf *.satysfi-aux

clean::
	(cd images; make clean)
	(cd text_mode; make clean)
	(cd md; make clean)

clip.pdf: head.satyh
first.pdf: head.satyh
glue1.pdf: head.satyh
math1.pdf: head.satyh
math2.pdf: head.satyh
