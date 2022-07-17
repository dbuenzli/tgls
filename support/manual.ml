(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp = Format.fprintf
let str = Printf.sprintf
type binding = string * string

let get_uri api f = match Doc.man_uri api f with
| Some doc -> doc | None -> assert false

let glCreateShaderProgramv api = str
"\
val create_shader_programv : enum -> string -> int
(** {{:%s}
    [glCreateShaderProgramv]} [type_ source] *)
"
(get_uri api "glCreateShaderProgramv"),
"\
let create_shader_programv =
  foreign ~stub \"glCreateShaderProgramv\"
    (int_as_uint @-> int @-> ptr string @-> returning int_as_uint)

let create_shader_programv type_ src =
  let src = allocate string src in
  create_shader_programv type_ 1 src
"

let glDebugMessageCallback api = str
"\
val debug_message_callback : debug_proc -> unit
(** {{:%s}
    [glDebugMessageCallback]} [f] *)
"
(get_uri api "glDebugMessageCallback"),
"\
module DebugMessageCallback =
  (val (dynamic_funptr (int_as_uint @-> int_as_uint @-> int_as_uint @->
            int_as_uint @-> int @-> ptr char @-> ptr void @->
            returning void)))

let debug_message_callback =
  foreign ~stub \"glDebugMessageCallback\"
    (DebugMessageCallback.t @-> ptr void @-> returning void)

let debug_message_callback =
  let debug_cb = ref None in
  fun f ->
  let wrap_cb src typ id sev len msg _ =
    let s = Bytes.create len in
    for i = 0 to len - 1 do Bytes.set s i (!@ (msg +@ i)) done;
    f src typ id sev (Bytes.unsafe_to_string s)
  in
  let dyn_wrapped_cb = DebugMessageCallback.of_fun wrap_cb in
  let old_cb = !debug_cb in
  debug_cb := Some dyn_wrapped_cb;
  debug_message_callback dyn_wrapped_cb null;
  (match old_cb with
  | Some old -> DebugMessageCallback.free old
  | None -> ())
"

let glGetUniformIndices api = str
"\
val get_uniform_indices : int -> string list -> uint32_bigarray -> unit
(** {{:%s}
    [glGetUniformIndices]} [program uniformNames uniformIndices] *)"
(get_uri api "glGetUniformIndices"),
"\
let get_uniform_indices =
  foreign ~stub \"glGetUniformIndices\"
    (int_as_uint @-> int @-> ptr string @-> ptr void @-> returning void)

let get_uniform_indices program names indices =
  let count = List.length names in
  let names = CArray.(start (of_list string names)) in
  let indices = to_voidp (bigarray_start array1 indices) in
  get_uniform_indices program count names indices
"

let glMapBuffer api = str
"\
val map_buffer : enum -> int -> enum -> ('a, 'b) Bigarray.kind ->
  ('a, 'b) bigarray
(** {{:%s}
    [glMapBuffer]} [target length access kind]

    {b Note.} [length] is the length, in number of bigarray elements, of the
    mapped buffer.

    {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
    program termination may happen if you don't respect the access policy. *)
"
(get_uri api "glMapBuffer"),
"\
let map_buffer =
  foreign ~stub \"glMapBuffer\"
    (int_as_uint @-> int_as_uint @-> returning (ptr void))

let map_buffer target len access kind =
  let p = map_buffer target access in
  let p = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
  bigarray_of_ptr array1 len kind p
"

let glMapNamedBuffer api = str
"\
val map_named_buffer : enum -> int -> enum -> ('a, 'b) Bigarray.kind ->
  ('a, 'b) bigarray
(** {{:%s}
    [glMapNamedBuffer]} [buffer length access kind]

    {b Note.} [length] is the length, in number of bigarray elements, of the
    mapped buffer.

    {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
    program termination may happen if you don't respect the access policy. *)
"
(get_uri api "glMapNamedBuffer"),
"\
let map_named_buffer =
  foreign ~stub \"glMapNamedBuffer\"
    (int_as_uint @-> int_as_uint @-> returning (ptr void))

let map_named_buffer buffer len access kind =
  let p = map_named_buffer buffer access in
  let p = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
  bigarray_of_ptr array1 len kind p
"

let glMapBufferRange api = str
"\
val map_buffer_range : enum -> int -> int -> enum ->
  ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray
(** {{:%s}
    [glMapBufferRange]} [target offset length access kind]

    {b Note.} [length] is the length in number of bigarray elements of the
    mapped buffer. [offset] is in bytes.

    {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
    program termination may happen if you don't respect the access policy. *)
"
(get_uri api "glMapBufferRange"),
"\
let map_buffer_range =
  foreign ~stub \"glMapBufferRange\"
    (int_as_uint @-> int @-> int @-> int_as_uint @-> returning (ptr void))

let map_buffer_range target offset len access kind =
  let len_bytes = ba_kind_byte_size kind * len in
  let p = map_buffer_range target offset len_bytes access in
  let p = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
  bigarray_of_ptr array1 len kind p
"

let glMapNamedBufferRange api = str
"\
val map_named_buffer_range : enum -> int -> int -> enum ->
  ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray
(** {{:%s}
    [glMapNamedBufferRange]} [buffer offset length access kind]

    {b Note.} [length] is the length in number of bigarray elements of the
    mapped buffer. [offset] is in bytes.

    {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
    program termination may happen if you don't respect the access policy. *)
"
(get_uri api "glMapNamedBufferRange"),
"\
let map_named_buffer_range =
  foreign ~stub \"glMapNamedBufferRange\"
    (int_as_uint @-> int @-> int @-> int_as_uint @-> returning (ptr void))

let map_named_buffer_range buffer offset len access kind =
  let len_bytes = ba_kind_byte_size kind * len in
  let p = map_named_buffer_range buffer offset len_bytes access in
  let p = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
  bigarray_of_ptr array1 len kind p
"

let glShaderSource api = str
"\
val shader_source : int -> string -> unit
(** {{:%s}
    [glShaderSource]} [shader source] *)
"
(get_uri api "glShaderSource"),
"\
let shader_source =
  foreign ~stub \"glShaderSource\"
    (int_as_uint @-> int @-> ptr string @-> ptr void @-> returning void)

let shader_source sh src =
  let src = allocate string src in
  shader_source sh 1 src null
"

let glTransformFeedbackVaryings api = str
"\
val transform_feedback_varyings : int -> string list -> enum -> unit
(** {{:%s}
    [glTransformFeedbackVaryings]} [program varyings bufferMode] *)"
(get_uri api "glTransformFeedbackVaryings"),
"\
let transform_feedback_varyings =
  foreign ~stub \"glTransformFeedbackVaryings\"
    (int_as_uint @-> int @-> ptr string @-> int_as_uint @-> returning void)

let transform_feedback_varyings program varyings mode =
  let count = List.length varyings in
  let varyings = CArray.(start (of_list string varyings)) in
  transform_feedback_varyings program count varyings mode
"

let get api = function
| "glCreateShaderProgramv" -> Some (glCreateShaderProgramv api)
| "glDebugMessageCallback" -> Some (glDebugMessageCallback api)
| "glGetUniformIndices" -> Some (glGetUniformIndices api)
| "glMapBuffer" -> Some (glMapBuffer api)
| "glMapNamedBuffer" -> Some (glMapNamedBuffer api)
| "glMapBufferRange" -> Some (glMapBufferRange api)
| "glMapNamedBufferRange" -> Some (glMapNamedBufferRange api)
| "glShaderSource" -> Some (glShaderSource api)
| "glTransformFeedbackVaryings" -> Some (glTransformFeedbackVaryings api)
| _ -> None

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
