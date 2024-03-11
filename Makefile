TARGET=satysfi
DUNE=dune

.PHONY: all install lib uninstall clean

all:
	$(DUNE) build --root .

install: $(TARGET)
	$(DUNE) install

#preliminary:
#	[ -d .git ] && git submodule update -i || echo "Skip git submodule update -i"

uninstall:
	$(DUNE) uninstall

clean:
	$(DUNE) clean
