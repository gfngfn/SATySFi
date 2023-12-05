TARGETS = \
	test.tex \

EXPECTED_LOCKS=$(TARGETS:.tex=.satysfi-lock-expected)

SATYSFI ?= satysfi

.PHONY: all promote clean

.SUFFIXES: .saty .tex .satysfi-lock .satysfi-lock-expected

# Keeps intermediate results:
.PRECIOUS: %.satysfi-lock

# Generates a lock file from a document and checks that it is as expected:
.saty.satysfi-lock:
	$(SATYSFI) solve $<
	diff $(<:.saty=.satysfi-lock) $(<:.saty=.satysfi-lock-expected)

# Outputs a text file:
%.tex: %.saty %.satysfi-lock
	$(SATYSFI) build $< --text-mode "latex" -o $@

# Promote a lock file to the corresponding expected lock file:
%.satysfi-lock-expected: %.satysfi-lock
	cp $*.satysfi-lock $*.satysfi-lock-expected

# Entrypoint for typesetting documents:
all:: $(TARGETS)

# Entrypoint for promoting lock files:
promote:: $(EXPECTED_LOCKS)

clean:
	rm -f *.pdf *.dvi *.log *.fls *.fdb_latexmk *.synctex.gz *.tex *.satysfi-aux *.satysfi-lock

test.saty: head.satyh-latex
