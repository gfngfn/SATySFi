TARGETS = \
	demo.pdf \

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .saty .pdf

.saty.pdf:
	$(SATYSFI) $< -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.satysfi-aux
