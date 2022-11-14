TARGETS = \
	test.pdf \

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .md .pdf

.md.pdf:
	$(SATYSFI) build $< -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.satysfi-aux
