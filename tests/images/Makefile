TARGETS = \
	test.pdf \

.PHONY: all clean

.SUFFIXES: .saty .pdf

.saty.pdf:
	satysfi $< -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.satysfi-aux
