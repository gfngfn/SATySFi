# Macrodown (ver. 1.00 beta)

## Summary

Macrodown is a markup language wrapping other markup languages such as TeX/LaTeX or HTML.
It consists of two “layers” ― the text layer and the program layer.
The former is mainly for writing documents in LaTeX-like syntax.
The latter, that has ML-like syntax, is mainly for defining functions and macros
with polymorphic static typing.
It enables you to write documents semantically markuped with flexible macros of your own making.

## Usage

Download the binary executable file from the Release of Macrodown on GitHub.
Should you want to compile from the source codes,
install `make` and `ocamlc`/`ocamlopt`.
Clone this repository, and execute `make`.
Then `macrodown.exe` is generated in `bin/`, and you can type at `bin/` directory

    ./macrodown.exe <input files> -o <output file>

in order to compile `<input files>` (file names separated with spaces) into `<output file>`.

The specification of Macrodown is written in Japanese at `doc/introduction.mcrd`.
