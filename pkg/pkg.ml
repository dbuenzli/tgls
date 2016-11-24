#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let build_support () =
  let ocaml = Conf.tool "ocaml" `Build_os in
  OS.Cmd.run Cmd.(ocaml % "pkg/build_support.ml")

let distrib =
  let exclude_paths () = Pkg.exclude_paths () >>| fun ps -> "support" :: ps in
  Pkg.distrib ~massage:build_support ~exclude_paths ()

let without_gl   =
  Topkg.Conf.(key "without-gl"   bool ~absent:false
              ~doc:"Omit OpenGL (desktop) support")
let without_gles =
  Topkg.Conf.(key "without-gles" bool ~absent:false
              ~doc:"Omit OpenGL ES support")

let () =
  Pkg.describe "tgls" ~distrib @@ fun c ->
  let with_gl   = not (Topkg.Conf.value c without_gl)   in
  let with_gles = not (Topkg.Conf.value c without_gles) in
  Ok [ Pkg.mllib ~cond:with_gl "src/tgl3.mllib";
       Pkg.mllib ~cond:with_gl "src/tgl4.mllib";
       Pkg.mllib ~cond:with_gles "src/tgles2.mllib";
       Pkg.mllib ~cond:with_gles "src/tgles3.mllib";
       Pkg.clib ~cond:with_gl "src/libtgl3.clib";
       Pkg.clib ~cond:with_gl "src/libtgl4.clib";
       Pkg.clib ~cond:with_gles "src/libtgles2.clib";
       Pkg.clib ~cond:with_gles "src/libtgles3.clib";
       Pkg.mllib ~cond:with_gl ~api:[] "src/tgl3_top.mllib";
       Pkg.mllib ~cond:with_gl ~api:[] "src/tgl4_top.mllib";
       Pkg.mllib ~cond:with_gles ~api:[] "src/tgles2_top.mllib";
       Pkg.mllib ~cond:with_gles ~api:[] "src/tgles3_top.mllib";
       Pkg.test ~cond:with_gl ~run:false "test/trigl3";
       Pkg.test ~cond:with_gl ~run:false "test/trigl4";
       Pkg.test ~cond:with_gles ~run:false "test/trigles2";
       Pkg.test ~cond:with_gles ~run:false "test/trigles3";
       Pkg.test ~cond:with_gl ~run:false "test/linkgl3";
       Pkg.test ~cond:with_gl ~run:false "test/linkgl4";
       Pkg.test ~cond:with_gles ~run:false "test/linkgles2";
       Pkg.test ~cond:with_gles ~run:false "test/linkgles3";
       Pkg.doc "test/assert_sizes.c";
       Pkg.doc ~cond:with_gl "test/trigl3.ml";
       Pkg.doc ~cond:with_gl "test/trigl4.ml";
       Pkg.doc ~cond:with_gles "test/trigles2.ml";
       Pkg.doc ~cond:with_gles "test/trigles3.ml";
       Pkg.doc ~cond:with_gl "test/linkgl3.ml";
       Pkg.doc ~cond:with_gl "test/linkgl4.ml";
       Pkg.doc ~cond:with_gles "test/linkgles2.ml";
       Pkg.doc ~cond:with_gles "test/linkgles3.ml";]
