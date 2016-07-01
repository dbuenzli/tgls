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

let () =
  Pkg.describe "tgls" ~distrib @@ fun c ->
  Ok [ Pkg.mllib "src/tgl3.mllib";
       Pkg.mllib "src/tgl4.mllib";
       Pkg.mllib "src/tgles2.mllib";
       Pkg.mllib "src/tgles3.mllib";
       Pkg.clib "src/libtgl3.clib";
       Pkg.clib "src/libtgl4.clib";
       Pkg.clib "src/libtgles2.clib";
       Pkg.clib "src/libtgles3.clib";
       Pkg.mllib ~api:[] "src/tgl3_top.mllib";
       Pkg.mllib ~api:[] "src/tgl4_top.mllib";
       Pkg.mllib ~api:[] "src/tgles2_top.mllib";
       Pkg.mllib ~api:[] "src/tgles3_top.mllib";
       Pkg.test ~run:false "test/trigl3";
       Pkg.test ~run:false "test/trigl4";
       Pkg.test ~run:false "test/trigles2";
       Pkg.test ~run:false "test/trigles3";
       Pkg.test ~run:false "test/linkgl3";
       Pkg.test ~run:false "test/linkgl4";
       Pkg.test ~run:false "test/linkgles2";
       Pkg.test ~run:false "test/linkgles3";
       Pkg.doc "test/assert_sizes.c";
       Pkg.doc "test/trigl3.ml";
       Pkg.doc "test/trigl4.ml";
       Pkg.doc "test/trigles2.ml";
       Pkg.doc "test/trigles3.ml";
       Pkg.doc "test/linkgl3.ml";
       Pkg.doc "test/linkgl4.ml";
       Pkg.doc "test/linkgles2.ml";
       Pkg.doc "test/linkgles3.ml";]
