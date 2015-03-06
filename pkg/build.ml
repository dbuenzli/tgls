#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let tgl3 = Env.bool "tgl3"
let tgl4 = Env.bool "tgl4"
let tgles2 = Env.bool "tgles2"
let tgles3 = Env.bool "tgles3"

let () =
  Pkg.describe "tgls" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~cond:tgl3 ~exts:Exts.module_library "src/tgl3";
    Pkg.lib ~cond:tgl4 ~exts:Exts.module_library "src/tgl4";
    Pkg.lib ~cond:tgles2 ~exts:Exts.module_library "src/tgles2";
    Pkg.lib ~cond:tgles3 ~exts:Exts.module_library "src/tgles3";
    Pkg.lib ~cond:tgl3 ~exts:Exts.library "src/tgl3_top";
    Pkg.lib ~cond:tgl4 ~exts:Exts.library "src/tgl4_top";
    Pkg.lib ~cond:tgles2 ~exts:Exts.library "src/tgles2_top";
    Pkg.lib ~cond:tgles3 ~exts:Exts.library "src/tgles3_top";
    Pkg.lib ~cond:tgl3 "src/libtgl3.a";
    Pkg.lib ~cond:tgl4 "src/libtgl4.a";
    Pkg.lib ~cond:tgles2 "src/libtgles2.a";
    Pkg.lib ~cond:tgles3 "src/libtgles3.a";
    Pkg.stublibs ~cond:tgl3 "src/dlltgl3.so";
    Pkg.stublibs ~cond:tgl4 "src/dlltgl4.so";
    Pkg.stublibs ~cond:tgles2 "src/dlltgles2.so";
    Pkg.stublibs ~cond:tgles3 "src/dlltgles3.so";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "DEVEL.md";
    Pkg.doc "test/assert_sizes.c";
    Pkg.doc "test/trigl3.ml";
    Pkg.doc "test/trigl4.ml";
    Pkg.doc "test/trigles2.ml";
    Pkg.doc "test/trigles3.ml";
    Pkg.doc "test/linkgl3.ml";
    Pkg.doc "test/linkgl4.ml";
    Pkg.doc "test/linkgles2.ml";
    Pkg.doc "test/linkgles3.ml";]
