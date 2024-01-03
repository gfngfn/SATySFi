PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
SATYSFI=satysfi
SAPHE=saphe
BINDIR=$(PREFIX)/bin
RM=rm -f
DUNE=dune

.PHONY: all test test-packages install uninstall clean

all:
	$(DUNE) build --root .
	cp _build/install/default/bin/$(SATYSFI) .
	cp _build/install/default/bin/$(SAPHE) .

test:
	$(DUNE) runtest

test-packages:
	./check-packages.sh

install: $(SATYSFI) $(SAPHE)
	mkdir -p $(BINDIR)
	install $(SATYSFI) $(BINDIR)
	install $(SAPHE) $(BINDIR)

#preliminary:
#	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"

uninstall:
	rm -rf $(BINDIR)/$(SATYSFI)
	rm -rf $(BINDIR)/$(SAPHE)
	rm -rf $(LIBDIR)

clean:
	$(DUNE) clean
	$(RM) $(SATYSFI)
	$(RM) $(SAPHE)
