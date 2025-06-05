This project uses (perhaps the development version of) [`b0`] for
development. Consult [b0 occasionally] for quick hints on how to
perform common development tasks.

[`b0`]: https://erratique.ch/software/b0
[b0 occasionally]: https://erratique.ch/software/b0/doc/occasionally.html

# Generating libraries

The `tgls*` source files in [`src`][src] are generated.

For generating them you need to install [xmlm][xmlm] and download a
copy of the [OpenGL XML registry][ogl-reg] to the path
`support/gl.xml` which is ignored by git. This can be done with:

    b0 -- download-glxml
 
After this the libraries can be generated with: 

    b0 -- generate-libraries

See also [support/README.md](support/README.md) in the source
repository.

[xmlm]: http://erratique.ch/software/xmlm
[ogl-reg]: http://www.opengl.org/registry/
