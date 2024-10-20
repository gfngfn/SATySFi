INTERMED_DIR = _build

TARGETS = \
	doc-primitives.pdf \
	doc-lang.pdf \
	math1.pdf \

EXPECTED_LOCKS=$(TARGETS:.pdf=.saphe.lock.yaml.expected)

SAPHE ?= saphe

.PHONY: all promote clean

.SUFFIXES: .saty .pdf .saphe.lock.yaml .saphe.lock.yaml.expected

# Keeps intermediate results:
.PRECIOUS: %.saphe.lock.yaml

# Generates a lock file from a document and checks that it is as expected:
%.saphe.lock.yaml: %.saty %.saphe.yaml
	$(SAPHE) solve $<
	diff $(<:.saty=.saphe.lock.yaml) $(<:.saty=.saphe.lock.yaml.expected)

# Typesets a document:
%.pdf: %.saty %.saphe.lock.yaml
	$(SAPHE) build $< -o $@

# Promote a lock file to the corresponding expected lock file:
%.saphe.lock.yaml.expected: %.saphe.lock.yaml
	cp $*.saphe.lock.yaml $*.saphe.lock.yaml.expected

# Entrypoint for typesetting documents:
all:: $(TARGETS)

# Entrypoint for promoting lock files:
promote:: $(EXPECTED_LOCKS)

clean:
	rm -f *.saphe.lock.yaml $(INTERMED_DIR)/*.satysfi-aux.yaml $(INTERMED_DIR)/*.satysfi-deps.yaml *.pdf

doc-primitives.pdf: local-math.satyh local.satyh paren.satyh
doc-lang.pdf: local-math.satyh
