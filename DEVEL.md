The repo contains generated data. For renegenerating it you will need
to install [xmlm][xmlm] and download a copy of the [OpenGL XML
registry][ogl-reg] to `support/gl.xml` (this will be done
automatically if the file doesn't exist). Type:

   ocaml ./pkg/build_support.ml

This will generate the files `src/tgl{3,4,es2,es3}.{mli,ml}`.

See also [support/README.md](support/README.md) in the source
repository.

[xmlm]: http://erratique.ch/software/xmlm
[ogl-reg]: http://www.opengl.org/registry/
