PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
SRCROOT=src
BACKEND=src/backend
FRONTEND=src/frontend
CHARDECODER=src/chardecoder
BYTECOMP=$(FRONTEND)/bytecomp
TARGET=satysfi
BINDIR=$(PREFIX)/bin
RM=rm -f
DUNE=dune

.PHONY: all install uninstall clean

all:
	$(DUNE) build
	cp _build/install/default/bin/$(TARGET) .

install: $(TARGET)
	$(DUNE) install --prefix="$(PREFIX)"

#preliminary:
#	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"

uninstall:
	$(DUNE) uninstall --prefix="$(PREFIX)"

clean:
	$(DUNE) clean
	$(RM) satysfi
