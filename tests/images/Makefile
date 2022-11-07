TARGETS = \
	test.pdf \

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .saty .pdf

.saty.pdf:
	$(SATYSFI) build $< -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.satysfi-aux
