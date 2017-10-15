Note: Currently, Macrodown is extended to a new typesetting system *SATySFi*.

![logo1](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo1.png)

[![Build Status](https://travis-ci.org/gfngfn/Macrodown.svg?branch=master)](https://travis-ci.org/gfngfn/Macrodown)

## Summary of Macrodown

Macrodown is a markup language wrapping other markup languages such as TeX/LaTeX or HTML.
It consists mainly of two “layers” ― the text layer and the program layer.
The former is for writing documents in LaTeX-like syntax.
The latter, which has ML-like syntax, is for defining functions and macros
with Hindley-Milner polymorphic static typing.
It enables you to write documents semantically markuped with flexible macros of your own making.

![logo2](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo2.png)

## Install

First, clone/download this repository.

<!--
### Use OPAM
* In the repository, run `opam pin add macrodown`.
* To reinstall, run `opam reinstall macrodown`.
* To uninstall, run `opam uninstall macrodown`.
-->

### Manual build of SATySFi

1. Install ocamlbuild, ocamlfind, and Menhir.
2. In repository, run `make`.
3. `macrodown` should then be available under the diretory.
4. Run `make install` to install `satysfi` as `/usr/local/bin/satysfi`.
5. Run `make install-lib` to create a symbolic link for the library.

You can modify the directory for the installation by specifying `PREFIX` like `sudo make install PREFIX=/usr/bin`. the symbolic link for the SATySFi library will be created as `/usr/local/lib-satysfi -> DIR/lib-satysfi` where `DIR` is the top directory of the repository.

<!--
### Download release from GitHub

See [release page](https://github.com/gfngfn/Macrodown/releases)
-->

## Usage of Macrodown

Type

    macrodown <input files> -o <output file>

in order to convert `<input files>` (file names separated with spaces) into `<output file>`.
For example, when you want to convert `doc.mcrd` into `output.tex` using your own header file `macros.mcrdh`,
the following command will work:

    macrodown macros.mcrdh doc.mcrd -o output.tex

Note that `macros.mcrdh` should precede `doc.mcrd`.
The current specification of Macrodown is written (currently only in Japanese) at `doc/introduction.mcrd`.
