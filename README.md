# Macrodown (ver. 0.9)

## Summary

Macrodown is a markup language wrapping other markup languages such as TeX/LaTeX or HTML.
It enables you to write documents with macros of your own making,
especially with variadic ones.

## Usage

Install `make` and `ocamlc`.
Clone this repository, make directory `bin/` under `Macrodown/`, and execute `make`.
Then `macrodown.exe` is generated, and you can type at `bin/` directory

    ./macrodown.exe <input files> -o <output file>

in order to compile `<input files>` (file names separated with spaces) into `<output file>`.

The specification of Macrodown is written in Japanese at `doc/introduction.mcrd`.
