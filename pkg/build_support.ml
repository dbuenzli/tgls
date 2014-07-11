#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

let str = Printf.sprintf

let glreg_uri =
  "https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/api/gl.xml"
let glxreg_uri =
  "https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/api/glx.xml"

let () =
  Cmd.exec (str "curl -# -S \"%s\" > support/gl.xml" glreg_uri)
  >>& Cmd.exec (str "curl -# -S \"%s\" > support/glx.xml" glxreg_uri)
  >>& fun () -> Cmd.exec "./build support"
  >>& fun () -> ()
