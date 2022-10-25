TARGETS = \
	test.pdf \

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .saty .pdf

.saty.pdf:
	$(SATYSFI) --markdown "mdja" $< -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.satysfi-aux
