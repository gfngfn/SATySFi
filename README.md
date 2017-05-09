![logo1](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo1.png)

[![Build Status](https://travis-ci.org/gfngfn/Macrodown.svg?branch=master)](https://travis-ci.org/gfngfn/Macrodown)

## Summary

Macrodown is a markup language wrapping other markup languages such as TeX/LaTeX or HTML.
It consists mainly of two “layers” ― the text layer and the program layer.
The former is for writing documents in LaTeX-like syntax.
The latter, which has ML-like syntax, is for defining functions and macros
with Hindley-Milner polymorphic static typing.
It enables you to write documents semantically markuped with flexible macros of your own making.

![logo2](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo2.png)

## Install

First, clone/download this repo

### Use OPAM
* In repositry, run `opam pin add macrodown .`  
* To reinstall, run `opam reinstsall macrodown`  
* To uninstall, run `opam uninstall macrodown`.  

### Manual build
1. Install ocamlbuild, ocamlfind, and Menhir.
2. In repository, run `make`.
3. `macrodown` should then be available under the diretory

### Download release from GitHub

See [release page](https://github.com/gfngfn/Macrodown/releases)

## Usage

Type

    macrodown <input files> -o <output file>

in order to convert `<input files>` (file names separated with spaces) into `<output file>`.
For example, when you want to convert `doc.mcrd` into `output.tex` using your own header file `macros.mcrdh`,
the following command will work:

    macrodown macros.mcrdh doc.mcrd -o output.tex

Note that `macros.mcrdh` should precede `doc.mcrd`.
The current specification of Macrodown is written in Japanese at `doc/introduction.mcrd`.
