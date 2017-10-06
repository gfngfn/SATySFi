all: somedoc.tex somedoc.dvi somedoc.pdf somedoc.html
somedoc.tex: tojsarticle.mcrdh somedoc_latex.mcrdh somedoc.mcrd
	macrodown $^ -o somedoc.tex
somedoc.dvi: somedoc.tex
	platex $^
somedoc.pdf: somedoc.dvi
	dvipdfmx $^
somedoc.html: tohtmlja.mcrdh somedoc_html.mcrdh somedoc.mcrd
	macrodown $^ -o somedoc.html

