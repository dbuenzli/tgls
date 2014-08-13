(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Tests that the Tgl4 library link flags are correct.

   Compile with:
   ocamlfind ocamlc -linkpkg -package ctypes.foreign,tgls.tgl4 \
                    -o linkgl4.byte linkgl4.ml
   ocamlfind ocamlopt -linkpkg -package ctypes.foreign,tgls.tgl4 \
                      -o linkgl4.native linkgl4.ml

   We try to load a symbol that should only be in the corresponding
   version.  We load directly with ctypes since Tgls functions fail on
   use and we cannot use since we don't have any context (and don't
   want to setup one as this may automatically link other things
   in). *)

open Tgl4
open Ctypes
open Foreign

let str = Printf.sprintf

let lookup min symb =
  try
    ignore (foreign_value symb (ptr void));
    Printf.printf "[OK] Found %s for OpenGL 4.%d\n" symb min;
    exit 0
  with
  | Dl.DL_error _ ->
      Printf.eprintf "[FAIL] %s not found for OpenGL 4.%d\n" symb min;
      exit 1

let yes = ref true
let test minor =
  let link () = if !yes then () else Gl.viewport 0 0 400 400; in
  link (); (* just make sure the library is linked *)
  match minor with
  | 0 -> lookup minor "glBindTransformFeedback"
  | 1 -> lookup minor "glProgramBinary"
  | 2 -> lookup minor "glDrawTransformFeedbackInstanced"
  | 3 -> lookup minor "glClearBufferData"
  | 4 -> lookup minor "glBindBuffersBase"
  | x -> Printf.eprintf "[FAIL] Unsupported OpenGL version: 4.%d\n" x; exit 1

let main () =
  let exec = Filename.basename Sys.executable_name in
  let usage = str "Usage: %s [OPTION]\n Tests Tgl4 linking.\nOptions:" exec in
  let minor = ref 0 in
  let options =
    [ "-minor", Arg.Set_int minor,
      " <x> use Use an OpenGL 4.x context (default to 4.0)"; ]
  in
  let anon _ = raise (Arg.Bad "no arguments are supported") in
  Arg.parse (Arg.align options) anon usage;
  test !minor

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
