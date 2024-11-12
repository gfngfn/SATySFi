PREFIX=/usr/local
LIBDIR=$(PREFIX)/share/satysfi
SATYSFI=satysfi
SAPHE=saphe
BINDIR=$(PREFIX)/bin

.PHONY: all test test-packages promote-package-locks update-ci-cache clean install uninstall

all:
	dune build

test:
	dune runtest

test-packages:
	./check-packages.sh

promote-package-locks:
	./promote-lock-of-packages.sh

update-ci-cache:
	./update-default-registry-commit-hash-file.sh

clean:
	dune clean

install:
	dune install --bindir=$(BINDIR)

uninstall:
	dune uninstall --bindir=$(BINDIR)
