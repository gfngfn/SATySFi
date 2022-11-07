TARGETS = \
	doc-primitives.pdf \
	doc-lang.pdf \
	math1.pdf \

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .saty .pdf

.saty.pdf:
	$(SATYSFI) build $< -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.satysfi-aux

doc-primitives.pdf: local-math.satyh local.satyh paren.satyh
doc-lang.pdf: local-math.satyh
