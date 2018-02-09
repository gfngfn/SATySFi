![logo1](https://raw.githubusercontent.com/wiki/gfngfn/SATySFi/img/satysfi-logo.png)

[![Build Status](https://travis-ci.org/gfngfn/SATySFi.svg?branch=master)](https://travis-ci.org/gfngfn/SATySFi)

## Summary of SATySFi

*SATySFi* (pronounced in the same way as the verb “satisfy” in English) is a new typesetting system with a static type system. It consists mainly of two “layers” ― the text layer and the program layer. The former is for writing documents in LaTeX-like syntax. The latter, which has ML-like syntax, is for defining functions and commands. SATySFi enables you to write documents markuped with flexible commands of your own making. In addition, its informative type error reporting will be a good help to your writing.

This software is supported by IPA Mitou Project.

## Install

First, clone this repository.

### Using OPAM

* To install SATySFi, in the repository
  1. run `git submodule update -i` to clone submodules.
  2. run `opam pin add satysfi .`.
  3. run `opam install satysfi`.
* To reinstall, run `opam reinstall satysfi`.
* To uninstall, run `opam uninstall satysfi`.

<!--
### Manual build of SATySFi

1. Install ocamlbuild, ocamlfind, and Menhir.
2. In repository, run `make`.
3. `macrodown` should then be available under the diretory.
4. Run `make install` to install `satysfi` as `/usr/local/bin/satysfi`.
5. Run `make install-lib` to create a symbolic link for the library.

You can modify the directory for the installation by specifying `PREFIX` like `sudo make install PREFIX=/usr/bin`. the symbolic link for the SATySFi library will be created as `/usr/local/lib-satysfi -> DIR/lib-satysfi` where `DIR` is the top directory of the repository.
-->

<!--
### Download release from GitHub

See [release page](https://github.com/gfngfn/Macrodown/releases)
-->

## Usage of SATySFi

Type

    satysfi <input files> -o <output file>

in order to convert `<input files>` (file names separated with spaces) into `<output file>`. For example, when you want to convert `doc.saty` into `output.pdf`, the following command will work:

    satysfi doc.saty -o output.pdf

## Command-line options

* `-v`, `--version`: Prints the version.
* `-o`, `--output`: Specify the name of the output PDF file. if this option is not given explicitly, the name of the output file is the concatenation of the base name of the input file and the extension `.pdf`.
* `--full-path`: Displays file names with their absolute path when outputting them to stdout.
