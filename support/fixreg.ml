(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* The registry doesn't provide us that info. *)

let is_arg_nullable f a = match f with
| "glObjectLabel" -> a = "label"
| "glObjectPtrLabel" -> a = "label"
| "glBindImageTextures" -> a = "textures"
| "glBindBuffersBase" -> a = "buffers"
| "glBindBuffersRange" ->
    (match a with "buffers" | "offsets" | "sizes" -> true | _ -> false)
| "glBindSamplers" -> a = "samplers"
| "glBindTextures" -> a = "textures"
| "glBindVertexBuffers" | "glVertexArrayVertexBuffers" ->
    (match a with "buffers" | "offsets" | "strides" -> true | _ -> false)
| "glBufferData"
| "glBufferSubData"
| "glBufferStorage"
| "glClearBufferData"
| "glClearBufferSubData"
| "glClearTexImage"
| "glClearTexSubImage" -> a = "data"
| "glDebugMessageControl" -> a = "ids"
| "glGetDebugMessageLog" ->
    begin match a with
     | "sources" | "types" | "ids" | "severities" | "lengths"
     | "messageLog" -> true
     | _ -> false
    end
| "glGetAttachedShaders" -> a = "count"
| "glGetProgramBinary" | "glGetActiveAttrib" | "glGetActiveSubroutineName"
| "glGetActiveSubroutineUniformName" | "glGetActiveUniform"
| "glGetActiveUniformBlockName" | "glGetActiveUniformName"
| "glGetObjectLabel" | "glGetObjectPtrLabel" | "glGetProgramInfoLog"
| "glGetProgramPipelineInfoLog" | "glGetProgramResourceName"
| "glGetShaderInfoLog" | "glGetShaderSource" | "glGetSynciv"
| "glGetTransformFeedbackVarying" ->
    a = "length"
| _ -> false

let is_ret_nullable = function
| "glGetString" | "glGetStringi" -> true
| _ -> false

let is_arg_voidp_or_index f a = match f with
  | "glTexImage1D" | "glTexImage2D" | "glTexImage3D"
  | "glTexSubImage1D" | "glTexSubImage2D" | "glTexSubImage3D" ->
      a = "pixels"
  | "glCompressedTexImage1D" | "glCompressedTexImage2D"
  | "glCompressedTexImage3D"
  | "glCompressedTexSubImage1D" | "glCompressedTexSubImage2D"
  | "glCompressedTexSubImage3D" ->
      a = "data"
  | "glDrawElements" | "glDrawElementsBaseVertex"
  | "glDrawElementsInstanced" | "glDrawElementsInstancedBaseInstance"
  | "glDrawElementsInstancedBaseVertex"
  | "glDrawElementsInstancedBaseVertexBaseInstance"
  | "glDrawRangeElements" | "glDrawRangeElementsBaseVertex" ->
      a = "indices"
  | "glDrawArraysIndirect" | "glDrawElementsIndirect"
  | "glMultiDrawArraysIndirect" | "glMultiDrawElementsIndirect" ->
      a = "indirect"
  | "glVertexAttribPointer" | "glVertexAttribIPointer"
  | "glVertexAttribLPointer" ->
      a = "pointer"
  | "glGetCompressedTexImage" | "glGetTexImage" -> a = "img"
  | "glReadPixels" -> a = "pixels"
  | _ -> false

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
