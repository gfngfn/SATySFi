CamlPDF
=======

CamlPDF is an OCaml library for reading, writing and modifying PDF files. It is
the basis of the commercial "CPDF" command line tool, which is available at
[http://www.coherentpdf.com/](http://www.coherentpdf.com/).

License
---

Copyright Coherent Graphics Ltd 2007 - 2013. Released under the LGPL with
special linking exception. See "LICENCE" for details.

To Build
---

If downloading from Github, obtain the correct source. This means choosing the tag for a
particaular version, such as "v2.1.1". The head of the master branch is
unstable.

1. Run "make". This will build camlpdf.a, camlpdf.cma, camlpdf.cmxa and the
documentation (in doc/camlpdf/html).

2. If your environment has "ocamlfind", "make install" will install the
library. Otherwise, use the built outputs as you will.

Documentation
---

The API documentation, which is built by the makefile in doc/camlpdf/html, can
also be accessed online at
[http://www.coherentpdf.com/camlpdf](http://www.coherentpdf.com/camlpdf).

The file introduction-to-camlpdf.pdf will help the beginner.

Some level of knowledge of the PDF file format itself, which is large, may be
required. The standard texts are the author's book:

[http://shop.oreilly.com/product/0636920021483.do](http://shop.oreilly.com/product/0636920021483.do)

and the ISO standard for PDF:

[http://www.adobe.com/devnet/pdf/pdf_reference.html](http://www.adobe.com/devnet/pdf/pdf_reference.html)

Acknowledgments
---

The file miniz.c is a (very slightly modified) version of the miniz.c zlib
implementation by Rich Geldreich, avalable here:

[http://code.google.com/p/miniz/](http://code.google.com/p/miniz/)

The files flatestubs.c, pdfflate.ml and pdfflate.mli are a slightly modified
version of some parts of CamlZip by Xavier Leroy. The originals are available
here:

[http://pauillac.inria.fr/~xleroy/software.html](http://pauillac.inria.fr/~xleroy/software.html)

OCamlMakefile was written by Markus Mottl. It is available here:

[http://bitbucket.org/mmottl/ocaml-makefile](http://bitbucket.org/mmottl/ocaml-makefile)


These works are also released under the LGPL with special linking exception, as
described in LICENCE.

