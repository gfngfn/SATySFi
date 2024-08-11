PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
BINDIR=$(PREFIX)/bin

.PHONY: all test test-packages clean install uninstall

all:
	dune build

test:
	$(DUNE) runtest

test-packages:
	./check-packages.sh

clean:
	dune clean

install:
	dune install --bindir=$(BINDIR)

uninstall:
	dune uninstall --bindir=$(BINDIR)
