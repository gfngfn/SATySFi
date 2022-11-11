TARGETS = \
	demo.pdf \

EXPECTED_LOCKS=$(TARGETS:.pdf=.satysfi-lock-expected)

SATYSFI ?= satysfi

.PHONY: all promote clean

.SUFFIXES: .saty .pdf .satysfi-lock .satysfi-lock-expected

# Keeps intermediate results:
.PRECIOUS: %.satysfi-lock

# Generates a lock file from a document and checks that it is as expected:
.saty.satysfi-lock:
	$(SATYSFI) solve $<
	diff $(<:.saty=.satysfi-lock) $(<:.saty=.satysfi-lock-expected)

# Typesets a document:
%.pdf: %.saty %.satysfi-lock
	$(SATYSFI) build $< -o $@

# Promote a lock file to the corresponding expected lock file:
%.satysfi-lock-expected: %.satysfi-lock
	cp $*.satysfi-lock $*.satysfi-lock-expected

# Entrypoint for typesetting documents:
all:: $(TARGETS)

# Entrypoint for promoting lock files:
promote:: $(EXPECTED_LOCKS)

clean:
	rm -f *.pdf *.satysfi-aux *.satysfi-lock
