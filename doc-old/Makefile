all: output.tex output.dvi output.pdf
output.tex: to_latex.mcrdh intr_style_latex.mcrdh introduction.mcrd
	macrodown $^ -o output.tex
output.dvi: output.tex
	platex output.tex
output.pdf: output.dvi
	dvipdfmx output.dvi

html: output.html
output.html: to_html.mcrdh intr_style_html.mcrdh introduction.mcrd
	macrodown $^ -o output.html

