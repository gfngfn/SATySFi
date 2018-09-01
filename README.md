Otfm — OpenType font decoder for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Otfm is an in-memory decoder for the OpenType font data format. It
provides low-level access to font tables and functions to decode some
of them.

Otfm is made of a single module and depends on [Uutf][uutf]. It is distributed 
under the ISC license.

[uutf]: http://erratique.ch/software/uutf
     
> Home page: http://erratique.ch/software/otfm  
> Contact: Daniel Bünzli `<daniel.buenzli@erratique.ch>

The `otfm` package distributed via this repository is an extension of
[D. Bünzli's original version](https://github.com/dbuenzli/otfm).
It newly supports TrueType Collection fonts, and `GPOS`/`GSUB`/`CFF` tables.


## Installation (of the original version)

Otfm can be installed with `opam`:

    opam install otfm

If you don't use `opam` consult the [`opam`](opam) file for build
instructions and a complete specification of the dependencies. 


## Documentation (of the original version)

The documentation and API reference is automatically generated 
from the interfaces. It can be consulted [online][doc] or via
`odig doc otfm`.

[doc]: http://erratique.ch/software/otfm/doc/Otfm


## Sample programs 

Sample programs are located in the `test` directory of the
distribution. They can be built with:

    topkg build --tests true

- `test.byte` tests the library, nothing should fail.
- `otftrip.native`, among other things, reads an OpenType file and
  prints a human readable representation on `stdout`. Invoke with
  `-help` for more information.
