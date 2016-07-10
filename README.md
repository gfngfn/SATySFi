![logo1](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo1.png)

## Summary

Macrodown is a markup language wrapping other markup languages such as TeX/LaTeX or HTML.
It consists mainly of two “layers” ― the text layer and the program layer.
The former is for writing documents in LaTeX-like syntax.
The latter, which has ML-like syntax, is for defining functions and macros
with Hindley-Milner polymorphic static typing.
It enables you to write documents semantically markuped with flexible macros of your own making.

![logo2](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo2.png)

## Install

Download the binary executable file from the Release of Macrodown on GitHub.
Should you want to compile the binary file from the source codes,
install `make` and `ocamlc`/`ocamlopt`, clone this repository, and execute `make`.
Then `macrodown` or `macrodown.exe` is generated in `bin/`.

Finally, add the path to the binary file to `PATH`.

## Usage

Type

    macrodown <input files> -o <output file>

in order to convert `<input files>` (file names separated with spaces) into `<output file>`.
For example, when you want to convert `doc.mcrd` into `output.tex` using your own header file `macros.mcrdh`,
the following command will work:

    macrodown macros.mcrdh doc.mcrd -o output.tex

Note that `macros.mcrdh` should precede `doc.mcrd`.
The current specification of Macrodown is written in Japanese at `doc/introduction.mcrd`.
