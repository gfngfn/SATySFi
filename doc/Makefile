#!/usr/bin/make -f

SATYSFI = satysfi

all: primitives.pdf lang.pdf math1.pdf

clean:
	$(RM) primitives.pdf lang.pdf math1.pdf $(wildcard *.satysfi-aux)

primitives.pdf:
	$(SATYSFI) doc-primitives.saty -o $@

lang.pdf:
	$(SATYSFI) doc-lang.saty -o $@

math1.pdf:
	$(SATYSFI) math1.saty -o $@

# We currently don't describe dependencies here.
# Instead we always rebuild them.
.PHONY: all clean primitives.pdf lang.pdf math1.pdf

