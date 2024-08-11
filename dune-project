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
 (version 0.1.0~alpha.1)
 (synopsis "A statically-typed, functional typesetting system")
 (description
  "SATySFi is a typesetting system equipped with a statically-typed, functional programming language. It consists mainly of two “layers” ― the text layer and the program layer. The former is for writing documents in LaTeX-like syntax. The latter, which has OCaml-like syntax, is for defining functions and commands. SATySFi enables you to write documents markuped with flexible commands of your own making. In addition, its informative type error reporting will be a good help to your writing.")
 (depends
  (ocaml
   (and
    (>= 4.12.0)
    (< 5.0.0)))
  (alcotest
   (and
    :with-test
    (>= 1.4.0)))
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
  (cmdliner
   (and
    (>= 1.1.1)
    (< 2.0)))
  (core_kernel
   (and
    (>= v0.15)
    (< v0.18)))
  core_unix
  (cppo
   (and
    (>= 1.6.4)
    (< 1.7)))
  dune-build-info
  (menhir
   (>= 20231231))
  (menhirLib
   (>= 20231231))
  (ocamlgraph
   (and
    (>= 2.0.0)
    (< 3.0)))
  (omd
   (= 2.0.0~alpha3))
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
  satysfi-common
  satysfi-util
  (semver2
   (and
    (>= 1.2.0)
    (< 2.0)))
  (uri
   (and
    (>= 4.2.0)
    (< 5.0)))
  (uutf
   (and
    (>= 1.0.3)
    (< 2.0)))
  (uunf
   (and
    (>= 15.0.0)
    (< 16.0)))
  (uuseg
   (and
    (>= 15.0.0)
    (< 16.0)))
  (yaml
   (and
    (>= 2.1.0)
    (< 3.0)))
  (yojson-with-position
   (= 1.4.2+satysfi))
  (0install-solver
   (and
    (>= 2.18)
    (< 3.0)))))

(package
 (name saphe)
 (version 0.1.0~alpha.1)
 (synopsis "A prototype ecosystem for SATySFi >= 0.1.0")
 (depends
  (0install-solver
   (>= 2.18))
  (ocaml
   (and
    (>= 4.12.0)
    (< 5.0.0)))
  (alcotest
   (and
    :with-test
    (>= 1.4.0)))
  (cmdliner
   (and
    (>= 1.1.1)
    (< 2.0)))
  (ppx_deriving
   (and
    (>= 5.2.1)
    (< 6.0)))
  satysfi-common
  satysfi-util
  (uri
   (and
    (>= 4.2.0)
    (< 5.0)))))

(package
 (name satysfi-util)
 (version 0.1.0~alpha.1)
 (synopsis "A utility library for SATySFi and Saphe")
 (depends
  (ocaml
   (and
    (>= 4.12.0)
    (< 5.0.0)))
  (alcotest
   (and
    :with-test
    (>= 1.4.0)))
  core_unix
  (ocamlgraph
   (and
    (>= 2.0.0)
    (< 3.0)))
  (ppx_deriving
   (and
    (>= 5.2.1)
    (< 6.0)))
  (semver2
   (and
    (>= 1.2.0)
    (< 2.0)))
  (uutf
   (and
    (>= 1.0.3)
    (< 2.0)))
  (yaml
   (and
    (>= 2.1.0)
    (< 3.0)))))

(package
 (name satysfi-common)
 (version 0.1.0~alpha.1)
 (synopsis "A library of modules used by SATySFi and Saphe in common")
 (depends
  (ocaml
   (and
    (>= 4.12.0)
    (< 5.0.0)))
  (alcotest
   (and
    :with-test
    (>= 1.4.0)))
  (ppx_deriving
   (and
    (>= 5.2.1)
    (< 6.0)))
  satysfi-util))
