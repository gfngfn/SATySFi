all: somedoc.tex somedoc.dvi somedoc.pdf somedoc.html
somedoc.tex: tojsarticle.mcrdh somedoc_latex.mcrdh somedoc.mcrd
	macrodown tojsarticle.mcrdh somedoc_latex.mcrdh somedoc.mcrd -o somedoc.tex
somedoc.dvi: somedoc.tex
	platex somedoc.tex
somedoc.pdf: somedoc.dvi
	dvipdfmx somedoc.dvi
somedoc.html: tohtmlja.mcrdh somedoc_html.mcrdh somedoc.mcrd
	macrodown tohtmlja.mcrdh somedoc_html.mcrdh somedoc.mcrd -o somedoc.html

