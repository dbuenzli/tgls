(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests that the Tgles3 library link flags are correct.

   Compile with:
   ocamlfind ocamlc -linkpkg -package ctypes-foreign,tgls.tgles3 \
                    -o linkgles3.byte linkgles3.ml
   ocamlfind ocamlopt -linkpkg -package ctypes-foreign,tgls.tgles3 \
                      -o linkgles3.native linkgles3.ml

   We try to load a symbol that should only be in the corresponding
   version.  We load directly with ctypes since Tgls functions fail on
   use and we cannot use since we don't have any context (and don't
   want to setup one as this may automatically link other things
   in). *)

open Tgles3
open Ctypes
open Foreign

let str = Printf.sprintf

let lookup symb =
  try
    ignore (foreign_value symb (ptr void));
    Printf.printf "[OK] Found %s for OpenGL ES 3.0\n" symb;
    exit 0
  with
  | Dl.DL_error _ ->
      Printf.eprintf "[FAIL] %s not found for OpenGL ES 3.0\n" symb;
      exit 1

let yes = ref true
let test () =
  let link () = if !yes then () else Gl.viewport 0 0 400 400; in
  link (); (* just make sure the library is linked *)
  lookup "glGetProgramBinary"

let main () =
  let exec = Filename.basename Sys.executable_name in
  let usage = str "Usage: %s [OPTION]\n Tests Tgles3 linking.\nOptions:" exec in
  let options = [] in
  let anon _ = raise (Arg.Bad "no arguments are supported") in
  Arg.parse (Arg.align options) anon usage;
  test ()

let () = main ()
