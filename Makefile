PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
TARGET=satysfi
BINDIR=$(PREFIX)/bin
RM=rm -f
DUNE=dune

.PHONY: all test test-packages install uninstall clean

all:
	$(DUNE) build --root .
	cp _build/install/default/bin/$(TARGET) .

test:
	$(DUNE) runtest

test-packages:
	./check-packages.sh

install: $(TARGET)
	mkdir -p $(BINDIR)
	install $(TARGET) $(BINDIR)

#preliminary:
#	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"

uninstall:
	rm -rf $(BINDIR)/$(TARGET)
	rm -rf $(LIBDIR)

clean:
	$(DUNE) clean
	$(RM) satysfi
