![logo1](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo1.png)

## Summary

Macrodown is a markup language wrapping other markup languages such as TeX/LaTeX or HTML.
It consists mainly of two “layers” ― the text layer and the program layer.
The former is for writing documents in LaTeX-like syntax.
The latter, that has ML-like syntax, is for defining functions and macros
with polymorphic static typing.
It enables you to write documents semantically markuped with flexible macros of your own making.

![logo2](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo2.png)

## Usage

Download the binary executable file from the Release of Macrodown on GitHub.
Should you want to compile from the source codes,
install `make` and `ocamlc`/`ocamlopt`.
Clone this repository, and execute `make`.
Then `macrodown.exe` is generated in `bin/`.

In Windows you can type at `bin/` directory

    macrodown.exe <input files> -o <output file>

or in UNIX enviroment

    ./macrodown <input files> -o <output file>

in order to compile `<input files>` (file names separated with spaces) into `<output file>`.

The specification of Macrodown is written in Japanese at `doc/introduction.mcrd`.
