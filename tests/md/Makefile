TARGETS = \
	test.pdf \

EXPECTED_LOCKS=$(TARGETS:.pdf=.saphe.lock.yaml.expected)

SAPHE ?= saphe

.PHONY: all promote clean

.SUFFIXES: .md .pdf .saphe.lock.yaml .saphe.lock.yaml.expected

# Keeps intermediate results:
.PRECIOUS: %.saphe.lock.yaml

# Generates a lock file from a document and checks that it is as expected:
%.saphe.lock.yaml: %.md
	$(SAPHE) solve $<
	diff $(<:.md=.saphe.lock.yaml) $(<:.md=.saphe.lock.yaml.expected)

# Typesets a document:
%.pdf: %.md %.saphe.lock.yaml
	$(SAPHE) build $< -o $@

# Promote a lock file to the corresponding expected lock file:
%.saphe.lock.yaml.expected: %.saphe.lock.yaml
	cp $*.saphe.lock.yaml $*.saphe.lock.yaml.expected

# Entrypoint for typesetting documents:
all:: $(TARGETS)

# Entrypoint for promoting lock files:
promote:: $(EXPECTED_LOCKS)

clean:
	rm -f *.pdf *.satysfi-aux *.saphe.lock.yaml *.satysfi-deps.yaml
