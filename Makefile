PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
BINDIR=$(PREFIX)/bin

.PHONY: all clean install uninstall

all:
	dune build

clean:
	dune clean

install:
	dune install --bindir=$(BINDIR)

uninstall:
	dune uninstall --bindir=$(BINDIR)
