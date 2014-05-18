#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let () = 
  Pkg.describe "tgls" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/tgl3";
    Pkg.lib ~exts:Exts.module_library "src/tgl4";
    Pkg.lib ~exts:Exts.module_library "src/tgles2";
    Pkg.lib ~exts:Exts.module_library "src/tgles3";
    Pkg.lib ~exts:Exts.library "src/tgl3_top";
    Pkg.lib ~exts:Exts.library "src/tgl4_top";
    Pkg.lib ~exts:Exts.library "src/tgles2_top";
    Pkg.lib ~exts:Exts.library "src/tgles3_top";
    Pkg.stublibs "src/dlltgl3.so";
    Pkg.stublibs "src/dlltgl4.so";
    Pkg.stublibs "src/dlltgles2.so";
    Pkg.stublibs "src/dlltgles3.so";
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
