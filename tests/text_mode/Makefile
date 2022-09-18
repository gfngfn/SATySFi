TARGETS = \
	test.tex \

SATYSFI ?= satysfi

.PHONY: all clean

.SUFFIXES: .saty .tex

.saty.tex:
	$(SATYSFI) $< --text-mode "latex" -o $@

all:: $(TARGETS)

clean:
	rm -f *.pdf *.dvi *.log *.fls *.fdb_latexmk *.synctex.gz *.tex *.satysfi-aux
