(lang dune 3.16)

(using menhir 2.1)

(formatting
 (enabled_for dune))

(name satysfi)

(license LGPL-3.0-only)

(authors "Takashi Suwa")

(maintainers "Takashi Suwa")

(source
 (github gfngfn/SATySFi))

(generate_opam_files true)

(package
 (name satysfi)
 (synopsis "A statically-typed, functional typesetting system")
 (description
  "SATySFi is a typesetting system equipped with a statically-typed, functional programming language. It consists mainly of two “layers” ― the text layer and the program layer. The former is for writing documents in LaTeX-like syntax. The latter, which has OCaml-like syntax, is for defining functions and commands. SATySFi enables you to write documents markuped with flexible commands of your own making. In addition, its informative type error reporting will be a good help to your writing.")
 (depends
  (ocaml
   (and
    (>= 4.12.0)
    (< 5.0.0)))
  (batteries
   (and
    (>= 3.6.0)
    (< 4.0)))
  (camlimages
   (and
    (>= 5.0.1)
    (< 5.0.5)))
  (camlpdf
   (= 2.3.1+satysfi))
  (core_kernel
   (and
    (>= v0.15)
    (< v0.18)))
  (cppo
   (and
    (>= 1.6.4)
    (< 1.7)))
  dune-build-info
  (menhir
   (>= 20231231))
  (menhirLib
   (>= 20231231))
  (omd
   (and
    (>= 1.3.2)
    (< 2.0)))
  (otfed
   (and
    (>= 0.3.1)
    (< 0.4)))
  (ppx_deriving
   (and
    (>= 5.2.1)
    (< 6.0)))
  (re
   (and
    (>= 1.10.4)
    (< 2.0)))
  (uutf
   (and
    (>= 1.0.3)
    (< 2.0)))
  (yojson-with-position
   (= 1.4.2+satysfi))))
