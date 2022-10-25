TARGETS = \
	test.pdf \

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .md .pdf

.md.pdf:
	$(SATYSFI) --markdown "mdja" $< -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.satysfi-aux
