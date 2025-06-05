Tgls — Thin bindings to OpenGL {3,4} and OpenGL ES {2,3} for OCaml
==================================================================

Tgls is a set of independent OCaml libraries providing thin bindings
to OpenGL libraries. It has support for core OpenGL 3.{2,3} and
4.{0,1,2,3,4} and OpenGL ES 2 and 3.{0,1,2}.

Tgls depends on [ocaml-ctypes][ctypes] and the C OpenGL library of your
platform. It is distributed under the ISC license.
          
[ctypes]: https://github.com/ocamllabs/ocaml-ctypes

Home page: <http://erratique.ch/software/tgls>  

## Installation

Tgls can be installed with `opam`:

    opam install tgls

If you don't use `opam` consult the [`opam`](opam) file for
build instructions and a complete specification of the dependencies.


## Supported OpenGL versions 

Tgls provides four libraries:

* `tgls.tgl3`, supports all functions and enumerants to program with a
   core OpenGL 3.2 or OpenGL 3.3 context.

* `tgls.tgl4`, supports all functions and enumerants to program with a
   core OpenGL 4.0 to 4.5 context.

* `tgls.tgles2`, supports all functions and enumerants to program with an
   OpenGL ES 2.0 context.

* `tgls.tgles3`, supports all functions and enumerants to program with an
   OpenGL ES 3.0 to 3.2 context.

Compatibility contexts are not supported. For extensions, most of them
only add few entry points and/or enumerants, as such it seems the
easiest way to access them is to manually use [ocaml-ctypes][ctypes] and
the appropriate constants (the tools in [support](support/) could be
enhanced to support them but it's not planned to do so).


## Documentation

The documentation can be consulted [online] or via `odig doc tgls`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker. 

[online]: https://erratique.ch/software/tgls/doc
[OCaml forum]: https://discuss.ocaml.org/

## Sample programs

A few sample programs can be found in [`test`][test], you need
[`tsdl`] to compile them.

The C file [`assert_sizes.c`](test/assert_sizes.c) is a program that
should exit with 0 on your platform to ensure the bindings will
work correctly. 
  
[`tsdl`]: http://erratique.ch/software/tsdl 
