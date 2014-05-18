#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

let str = Printf.sprintf 

let glreg_uri = 
  "https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/api/gl.xml"
  
let () = 
  Cmd.exec (str "curl -# -S \"%s\" > support/gl.xml" glreg_uri)
  >>& fun () -> Cmd.exec "./build support"
  >>& fun () -> ()
