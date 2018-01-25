DEPS=pervasives.satyh gr.satyh list.satyh tabular.satyh math.satyh color.satyh deco.satyh decoset.satyh vdecoset.satyh stdja.satyh local.satyh

all:
	satysfi $(DEPS) local-math.satyh doc-primitives.saty -o primitives.pdf

lang:
	satysfi $(DEPS) cd.satyh local-math.satyh doc-lang.saty -o lang.pdf

math1:
	satysfi $(DEPS) math1.saty -o math1.pdf
