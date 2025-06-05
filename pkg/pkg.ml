#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let with_gl   =
  Topkg.Conf.(key "with-gl"   bool ~absent:true
              ~doc:"Include OpenGL (desktop) support")
let with_gles =
  Topkg.Conf.(key "with-gles" bool ~absent:true
              ~doc:"Include OpenGL ES support")

let () =
  Pkg.describe "tgls" @@ fun c ->
  let with_gl   = Topkg.Conf.value c with_gl   in
  let with_gles = Topkg.Conf.value c with_gles in
  Ok [ Pkg.mllib ~cond:with_gl ~dst_dir:"tgl3" "src/tgl3/tgl3.mllib";
       Pkg.mllib ~cond:with_gl ~dst_dir:"tgl4" "src/tgl4/tgl4.mllib";
       Pkg.mllib ~cond:with_gles ~dst_dir:"tgles2" "src/tgles2/tgles2.mllib";
       Pkg.mllib ~cond:with_gles ~dst_dir:"tgles3" "src/tgles3/tgles3.mllib";
       Pkg.clib ~cond:with_gl
         ~lib_dst_dir:"tgl3" "src/tgl3/libtgl3.clib";
       Pkg.clib ~cond:with_gl
         ~lib_dst_dir:"tgl4" "src/tgl4/libtgl4.clib";
       Pkg.clib ~cond:with_gles
         ~lib_dst_dir:"tgles2" "src/tgles2/libtgles2.clib";
       Pkg.clib ~cond:with_gles
         ~lib_dst_dir:"tgles3" "src/tgles3/libtgles3.clib";

       Pkg.test ~cond:with_gl ~run:false "test/trigl3";
       Pkg.test ~cond:with_gl ~run:false "test/trigl4";
       Pkg.test ~cond:with_gles ~run:false "test/trigles2";
       Pkg.test ~cond:with_gles ~run:false "test/trigles3";
       Pkg.test ~cond:with_gl ~run:false "test/linkgl3";
       Pkg.test ~cond:with_gl ~run:false "test/linkgl4";
       Pkg.test ~cond:with_gl ~run:false "test/dbglifetime4";
       Pkg.test ~cond:with_gles ~run:false "test/linkgles2";
       Pkg.test ~cond:with_gles ~run:false "test/linkgles3" ]
