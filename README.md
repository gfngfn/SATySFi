![logo1](https://raw.githubusercontent.com/wiki/gfngfn/SATySFi/img/satysfi-logo.png)

[![Build Status](https://travis-ci.org/gfngfn/SATySFi.svg?branch=master)](https://travis-ci.org/gfngfn/SATySFi)

## Summary of SATySFi

*SATySFi* (pronounced in the same way as the verb “satisfy” in English) is a new typesetting system with a static type system. It consists mainly of two “layers” ― the text layer and the program layer. The former is for writing documents in LaTeX-like syntax. The latter, which has ML-like syntax, is for defining functions and commands. SATySFi enables you to write documents markuped with flexible commands of your own making. In addition, its informative type error reporting will be a good help to your writing.

This software is supported by IPA Mitou Project 2017 (see the abstract [here](https://www.ipa.go.jp/jinzai/mitou/2017/gaiyou_t-4.html)).

## Install using Homebrew (for OSX users)

There is a homebrew formula for SATySFi.

```sh
$ brew install --HEAD nyuichi/satysfi/satysfi
```

## Install using OPAM

### Prerequisites

Here is a list of minimally required softwares.

* bzip2
* cc
* git
* m4
* make
* unzip
* wget
* [opam](https://opam.ocaml.org/) 1.2 (Installation instructions are [here](https://opam.ocaml.org/doc/Install.html).)
* ocaml 4.06.0 (installed by OPAM)

#### Example (Ubuntu)

```sh
sudo apt-get update
sudo apt-get install build-essential git m4 unzip wget

# The following command will ask if you allow OPAM to modify some files (e.g. ~/.bash_profile).
# Be sure to read its instructions. Otherwise, some environment variables won't be set.
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin

opam switch 4.06.0
eval `opam config env`
opam update
```

#### Example (OS X Mavericks or later)

```sh
# Before running this scripts, install essential softwares such as GCC and Make. They can be installed from Xcode Command Line Tools.
# Also, install Homebrew.

brew update
brew install wget opam

# The following command will ask if OPAM modifies some files.
# Be sure to read their instructions. Otherwise, some environment variables won't be set.
opam init

opam switch 4.06.0
eval `opam config env`
opam update
```

### Build

First, clone this repository and submodules. Then build SATySFi using OPAM.

```sh
# clone
git clone https://github.com/gfngfn/SATySFi.git
cd SATySFi
git submodule update --init --recursive

# Issue #46: avoid 1.0+beta18 to build core_kernel correctly.
opam pin add -y jbuilder 1.0+beta17
# build
opam pin add satysfi .
opam install satysfi
```

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
