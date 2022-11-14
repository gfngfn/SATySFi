TARGETS = \
	test.pdf \

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .md .pdf

.md.pdf:
	$(SATYSFI) build --markdown $< -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.satysfi-aux
