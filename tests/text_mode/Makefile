TARGETS = \
	test.tex \

EXPECTED_LOCKS=$(TARGETS:.tex=.saphe.lock.yaml.expected)

SAPHE ?= saphe

.PHONY: all promote clean

.SUFFIXES: .saty .tex .saphe.lock.yaml .saphe.lock.yaml.expected

# Keeps intermediate results:
.PRECIOUS: %.saphe.lock.yaml

# Generates a lock file from a document and checks that it is as expected:
%.saphe.lock.yaml: %.saty
	$(SAPHE) solve $<
	diff $(<:.saty=.saphe.lock.yaml) $(<:.saty=.saphe.lock.yaml.expected)

# Outputs a text file:
%.tex: %.saty %.saphe.lock.yaml
	$(SAPHE) build $< --text-mode "latex" -o $@

# Promote a lock file to the corresponding expected lock file:
%.saphe.lock.yaml.expected: %.saphe.lock.yaml
	cp $*.saphe.lock.yaml $*.saphe.lock.yaml.expected

# Entrypoint for typesetting documents:
all:: $(TARGETS)

# Entrypoint for promoting lock files:
promote:: $(EXPECTED_LOCKS)

clean:
	rm -f *.pdf *.dvi *.log *.fls *.fdb_latexmk *.synctex.gz *.tex *.satysfi-aux.yaml *.saphe.lock.yaml *.satysfi-deps.yaml

test.saty: head.satyh-latex
