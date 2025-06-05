(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated with:
   apiquery -mli -api gles3.2 *)

(** OpenGL ES 3.x thin bindings.

    [Tgles3] can program  OpenGL ES 3.0 to 3.2 contexts.
    Consult the {{!conventions}binding conventions}.

    Open the module use it, this defines only the module [Gl]
    in your scope.

    {b References}
    {ul
    {- {{:http://www.khronos.org/opengles/3_X}OpenGL ES 3.x}}} *)

(** {1 OpenGL ES 3.x} *)

(** OpenGL ES 3.x bindings.
    
    {{!Gl.types}Types}, {{!Gl.funs}functions} and {{!Gl.enums}enumerants}. *)
module Gl : sig

  (** {1:ba Bigarrays} *)

  type ('a, 'b) bigarray = ('a,'b, Bigarray.c_layout) Bigarray.Array1.t

  val bigarray_byte_size : ('a, 'b) bigarray -> int
  (** [bigarray_byte_size ba] is the size of [ba] in bytes. *)

  val string_of_bigarray : (char, Bigarray.int8_unsigned_elt) bigarray -> string
  (** [string_of_bigarray ba] is [ba] until the first ['\x00'], as a string. *)

  (** {1:types Types} *)

  type bitfield = int
  type enum = int
  type enum_bigarray = (int32, Bigarray.int32_elt) bigarray
  type sync
  type uint32_bigarray = (int32, Bigarray.int32_elt) bigarray
  type uint64 = int64
  type debug_proc = enum -> enum -> int -> enum -> string -> unit
  
  (** {1:funs Functions} *)

  val active_shader_program : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glActiveShaderProgram.xhtml}
      [glActiveShaderProgram]} [pipeline program] *)
  
  val active_texture : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glActiveTexture.xhtml}
      [glActiveTexture]} [texture] *)
  
  val attach_shader : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glAttachShader.xhtml}
      [glAttachShader]} [program shader] *)
  
  val begin_query : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBeginQuery.xhtml}
      [glBeginQuery]} [target id] *)
  
  val begin_transform_feedback : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBeginTransformFeedback.xhtml}
      [glBeginTransformFeedback]} [primitiveMode] *)
  
  val bind_attrib_location : int -> int -> string -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindAttribLocation.xhtml}
      [glBindAttribLocation]} [program index name] *)
  
  val bind_buffer : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindBuffer.xhtml}
      [glBindBuffer]} [target buffer] *)
  
  val bind_buffer_base : enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindBufferBase.xhtml}
      [glBindBufferBase]} [target index buffer] *)
  
  val bind_buffer_range : enum -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindBufferRange.xhtml}
      [glBindBufferRange]} [target index buffer offset size] *)
  
  val bind_framebuffer : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindFramebuffer.xhtml}
      [glBindFramebuffer]} [target framebuffer] *)
  
  val bind_image_texture : int -> int -> int -> bool -> int -> enum ->
    enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindImageTexture.xhtml}
      [glBindImageTexture]} [unit texture level layered layer access format] *)
  
  val bind_program_pipeline : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindProgramPipeline.xhtml}
      [glBindProgramPipeline]} [pipeline] *)
  
  val bind_renderbuffer : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindRenderbuffer.xhtml}
      [glBindRenderbuffer]} [target renderbuffer] *)
  
  val bind_sampler : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindSampler.xhtml}
      [glBindSampler]} [unit sampler] *)
  
  val bind_texture : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindTexture.xhtml}
      [glBindTexture]} [target texture] *)
  
  val bind_transform_feedback : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindTransformFeedback.xhtml}
      [glBindTransformFeedback]} [target id] *)
  
  val bind_vertex_array : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindVertexArray.xhtml}
      [glBindVertexArray]} [array] *)
  
  val bind_vertex_buffer : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBindVertexBuffer.xhtml}
      [glBindVertexBuffer]} [bindingindex buffer offset stride] *)
  
  val blend_barrier : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendBarrier.xhtml}
      [glBlendBarrier]} [()] *)
  
  val blend_color : float -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendColor.xhtml}
      [glBlendColor]} [red green blue alpha] *)
  
  val blend_equation : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendEquation.xhtml}
      [glBlendEquation]} [mode] *)
  
  val blend_equation_separate : enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendEquationSeparate.xhtml}
      [glBlendEquationSeparate]} [modeRGB modeAlpha] *)
  
  val blend_equation_separatei : int -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendEquationSeparate.xhtml}
      [glBlendEquationSeparatei]} [buf modeRGB modeAlpha] *)
  
  val blend_equationi : int -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendEquation.xhtml}
      [glBlendEquationi]} [buf mode] *)
  
  val blend_func : enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendFunc.xhtml}
      [glBlendFunc]} [sfactor dfactor] *)
  
  val blend_func_separate : enum -> enum -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendFuncSeparate.xhtml}
      [glBlendFuncSeparate]} [sfactorRGB dfactorRGB sfactorAlpha
        dfactorAlpha] *)
  
  val blend_func_separatei : int -> enum -> enum -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendFuncSeparate.xhtml}
      [glBlendFuncSeparatei]} [buf srcRGB dstRGB srcAlpha dstAlpha] *)
  
  val blend_funci : int -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlendFunc.xhtml}
      [glBlendFunci]} [buf src dst] *)
  
  val blit_framebuffer : int -> int -> int -> int -> int -> int -> int ->
    int -> bitfield -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBlitFramebuffer.xhtml}
      [glBlitFramebuffer]} [srcX0 srcY0 srcX1 srcY1 dstX0 dstY0 dstX1 dstY1
        mask filter] *)
  
  val buffer_data : enum -> int -> ('a, 'b) bigarray option -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBufferData.xhtml}
      [glBufferData]} [target size data usage] *)
  
  val buffer_sub_data : enum -> int -> int -> ('a, 'b) bigarray option ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBufferSubData.xhtml}
      [glBufferSubData]} [target offset size data] *)
  
  val check_framebuffer_status : enum -> enum
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCheckFramebufferStatus.xhtml}
      [glCheckFramebufferStatus]} [target] *)
  
  val clear : bitfield -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClear.xhtml}
      [glClear]} [mask] *)
  
  val clear_bufferfi : enum -> int -> float -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClearBuffer.xhtml}
      [glClearBufferfi]} [buffer drawbuffer depth stencil] *)
  
  val clear_bufferfv : enum -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClearBuffer.xhtml}
      [glClearBufferfv]} [buffer drawbuffer value] *)
  
  val clear_bufferiv : enum -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClearBuffer.xhtml}
      [glClearBufferiv]} [buffer drawbuffer value] *)
  
  val clear_bufferuiv : enum -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClearBuffer.xhtml}
      [glClearBufferuiv]} [buffer drawbuffer value] *)
  
  val clear_color : float -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClearColor.xhtml}
      [glClearColor]} [red green blue alpha] *)
  
  val clear_depthf : float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClearDepth.xhtml}
      [glClearDepthf]} [d] *)
  
  val clear_stencil : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClearStencil.xhtml}
      [glClearStencil]} [s] *)
  
  val client_wait_sync : sync -> bitfield -> uint64 -> enum
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glClientWaitSync.xhtml}
      [glClientWaitSync]} [sync flags timeout] *)
  
  val color_mask : bool -> bool -> bool -> bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glColorMask.xhtml}
      [glColorMask]} [red green blue alpha] *)
  
  val color_maski : int -> bool -> bool -> bool -> bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glColorMask.xhtml}
      [glColorMaski]} [index r g b a] *)
  
  val compile_shader : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCompileShader.xhtml}
      [glCompileShader]} [shader] *)
  
  val compressed_tex_image2d : enum -> int -> enum -> int -> int -> int ->
    int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCompressedTexImage2D.xhtml}
      [glCompressedTexImage2D]} [target level internalformat width height
        border imageSize data] *)
  
  val compressed_tex_image3d : enum -> int -> enum -> int -> int -> int ->
    int -> int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCompressedTexImage3D.xhtml}
      [glCompressedTexImage3D]} [target level internalformat width height
        depth border imageSize data] *)
  
  val compressed_tex_sub_image2d : enum -> int -> int -> int -> int -> int ->
    enum -> int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCompressedTexSubImage2D.xhtml}
      [glCompressedTexSubImage2D]} [target level xoffset yoffset width height
        format imageSize data] *)
  
  val compressed_tex_sub_image3d : enum -> int -> int -> int -> int -> int ->
    int -> int -> enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCompressedTexSubImage3D.xhtml}
      [glCompressedTexSubImage3D]} [target level xoffset yoffset zoffset
        width height depth format imageSize data] *)
  
  val copy_buffer_sub_data : enum -> enum -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCopyBufferSubData.xhtml}
      [glCopyBufferSubData]} [readTarget writeTarget readOffset writeOffset
        size] *)
  
  val copy_image_sub_data : int -> enum -> int -> int -> int -> int -> int ->
    enum -> int -> int -> int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCopyImageSubData.xhtml}
      [glCopyImageSubData]} [srcName srcTarget srcLevel srcX srcY srcZ
        dstName dstTarget dstLevel dstX dstY dstZ srcWidth srcHeight
        srcDepth] *)
  
  val copy_tex_image2d : enum -> int -> enum -> int -> int -> int -> int ->
    int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCopyTexImage2D.xhtml}
      [glCopyTexImage2D]} [target level internalformat x y width height
        border] *)
  
  val copy_tex_sub_image2d : enum -> int -> int -> int -> int -> int ->
    int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCopyTexSubImage2D.xhtml}
      [glCopyTexSubImage2D]} [target level xoffset yoffset x y width height] *)
  
  val copy_tex_sub_image3d : enum -> int -> int -> int -> int -> int ->
    int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCopyTexSubImage3D.xhtml}
      [glCopyTexSubImage3D]} [target level xoffset yoffset zoffset x y width
        height] *)
  
  val create_program : unit -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCreateProgram.xhtml}
      [glCreateProgram]} [()] *)
  
  val create_shader : enum -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCreateShader.xhtml}
      [glCreateShader]} [type_] *)
  
  val create_shader_programv : enum -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCreateShaderProgram.xhtml}
      [glCreateShaderProgramv]} [type_ source] *)
  
  val cull_face : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glCullFace.xhtml}
      [glCullFace]} [mode] *)
  
  val debug_message_callback : debug_proc -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDebugMessageCallback.xhtml}
      [glDebugMessageCallback]} [f] *)
  
  val debug_message_control : enum -> enum -> enum -> int ->
    uint32_bigarray option -> bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDebugMessageControl.xhtml}
      [glDebugMessageControl]} [source type_ severity count ids enabled] *)
  
  val debug_message_insert : enum -> enum -> int -> enum -> int -> string ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDebugMessageInsert.xhtml}
      [glDebugMessageInsert]} [source type_ id severity length buf] *)
  
  val delete_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteBuffers.xhtml}
      [glDeleteBuffers]} [n buffers] *)
  
  val delete_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteFramebuffers.xhtml}
      [glDeleteFramebuffers]} [n framebuffers] *)
  
  val delete_program : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteProgram.xhtml}
      [glDeleteProgram]} [program] *)
  
  val delete_program_pipelines : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteProgramPipelines.xhtml}
      [glDeleteProgramPipelines]} [n pipelines] *)
  
  val delete_queries : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteQueries.xhtml}
      [glDeleteQueries]} [n ids] *)
  
  val delete_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteRenderbuffers.xhtml}
      [glDeleteRenderbuffers]} [n renderbuffers] *)
  
  val delete_samplers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteSamplers.xhtml}
      [glDeleteSamplers]} [count samplers] *)
  
  val delete_shader : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteShader.xhtml}
      [glDeleteShader]} [shader] *)
  
  val delete_sync : sync -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteSync.xhtml}
      [glDeleteSync]} [sync] *)
  
  val delete_textures : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteTextures.xhtml}
      [glDeleteTextures]} [n textures] *)
  
  val delete_transform_feedbacks : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteTransformFeedbacks.xhtml}
      [glDeleteTransformFeedbacks]} [n ids] *)
  
  val delete_vertex_arrays : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDeleteVertexArrays.xhtml}
      [glDeleteVertexArrays]} [n arrays] *)
  
  val depth_func : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDepthFunc.xhtml}
      [glDepthFunc]} [func] *)
  
  val depth_mask : bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDepthMask.xhtml}
      [glDepthMask]} [flag] *)
  
  val depth_rangef : float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDepthRange.xhtml}
      [glDepthRangef]} [n f] *)
  
  val detach_shader : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDetachShader.xhtml}
      [glDetachShader]} [program shader] *)
  
  val disable : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glEnable.xhtml}
      [glDisable]} [cap] *)
  
  val disable_vertex_attrib_array : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glEnableVertexAttribArray.xhtml}
      [glDisableVertexAttribArray]} [index] *)
  
  val disablei : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glEnable.xhtml}
      [glDisablei]} [target index] *)
  
  val dispatch_compute : int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDispatchCompute.xhtml}
      [glDispatchCompute]} [num_groups_x num_groups_y num_groups_z] *)
  
  val dispatch_compute_indirect : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDispatchComputeIndirect.xhtml}
      [glDispatchComputeIndirect]} [indirect] *)
  
  val draw_arrays : enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawArrays.xhtml}
      [glDrawArrays]} [mode first count] *)
  
  val draw_arrays_indirect : enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawArraysIndirect.xhtml}
      [glDrawArraysIndirect]} [mode indirect] *)
  
  val draw_arrays_instanced : enum -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawArraysInstanced.xhtml}
      [glDrawArraysInstanced]} [mode first count instancecount] *)
  
  val draw_buffers : int -> enum_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawBuffers.xhtml}
      [glDrawBuffers]} [n bufs] *)
  
  val draw_elements : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawElements.xhtml}
      [glDrawElements]} [mode count type_ indices] *)
  
  val draw_elements_base_vertex : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawElementsBaseVertex.xhtml}
      [glDrawElementsBaseVertex]} [mode count type_ indices basevertex] *)
  
  val draw_elements_indirect : enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawElementsIndirect.xhtml}
      [glDrawElementsIndirect]} [mode type_ indirect] *)
  
  val draw_elements_instanced : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawElementsInstanced.xhtml}
      [glDrawElementsInstanced]} [mode count type_ indices instancecount] *)
  
  val draw_elements_instanced_base_vertex : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawElementsInstancedBaseVertex.xhtml}
      [glDrawElementsInstancedBaseVertex]} [mode count type_ indices
        instancecount basevertex] *)
  
  val draw_range_elements : enum -> int -> int -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawRangeElements.xhtml}
      [glDrawRangeElements]} [mode start end_ count type_ indices] *)
  
  val draw_range_elements_base_vertex : enum -> int -> int -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glDrawRangeElementsBaseVertex.xhtml}
      [glDrawRangeElementsBaseVertex]} [mode start end_ count type_ indices
        basevertex] *)
  
  val enable : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glEnable.xhtml}
      [glEnable]} [cap] *)
  
  val enable_vertex_attrib_array : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glEnableVertexAttribArray.xhtml}
      [glEnableVertexAttribArray]} [index] *)
  
  val enablei : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glEnable.xhtml}
      [glEnablei]} [target index] *)
  
  val end_query : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBeginQuery.xhtml}
      [glEndQuery]} [target] *)
  
  val end_transform_feedback : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glBeginTransformFeedback.xhtml}
      [glEndTransformFeedback]} [()] *)
  
  val fence_sync : enum -> bitfield -> sync
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFenceSync.xhtml}
      [glFenceSync]} [condition flags] *)
  
  val finish : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFinish.xhtml}
      [glFinish]} [()] *)
  
  val flush : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFlush.xhtml}
      [glFlush]} [()] *)
  
  val flush_mapped_buffer_range : enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFlushMappedBufferRange.xhtml}
      [glFlushMappedBufferRange]} [target offset length] *)
  
  val framebuffer_parameteri : enum -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFramebufferParameteri.xhtml}
      [glFramebufferParameteri]} [target pname param] *)
  
  val framebuffer_renderbuffer : enum -> enum -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFramebufferRenderbuffer.xhtml}
      [glFramebufferRenderbuffer]} [target attachment renderbuffertarget
        renderbuffer] *)
  
  val framebuffer_texture : enum -> enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFramebufferTexture.xhtml}
      [glFramebufferTexture]} [target attachment texture level] *)
  
  val framebuffer_texture2d : enum -> enum -> enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFramebufferTexture.xhtml}
      [glFramebufferTexture2D]} [target attachment textarget texture level] *)
  
  val framebuffer_texture_layer : enum -> enum -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFramebufferTextureLayer.xhtml}
      [glFramebufferTextureLayer]} [target attachment texture level layer] *)
  
  val front_face : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glFrontFace.xhtml}
      [glFrontFace]} [mode] *)
  
  val gen_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenBuffers.xhtml}
      [glGenBuffers]} [n buffers] *)
  
  val gen_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenFramebuffers.xhtml}
      [glGenFramebuffers]} [n framebuffers] *)
  
  val gen_program_pipelines : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenProgramPipelines.xhtml}
      [glGenProgramPipelines]} [n pipelines] *)
  
  val gen_queries : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenQueries.xhtml}
      [glGenQueries]} [n ids] *)
  
  val gen_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenRenderbuffers.xhtml}
      [glGenRenderbuffers]} [n renderbuffers] *)
  
  val gen_samplers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenSamplers.xhtml}
      [glGenSamplers]} [count samplers] *)
  
  val gen_textures : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenTextures.xhtml}
      [glGenTextures]} [n textures] *)
  
  val gen_transform_feedbacks : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenTransformFeedbacks.xhtml}
      [glGenTransformFeedbacks]} [n ids] *)
  
  val gen_vertex_arrays : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenVertexArrays.xhtml}
      [glGenVertexArrays]} [n arrays] *)
  
  val generate_mipmap : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGenerateMipmap.xhtml}
      [glGenerateMipmap]} [target] *)
  
  val get_active_attrib : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetActiveAttrib.xhtml}
      [glGetActiveAttrib]} [program index bufSize length size type_ name] *)
  
  val get_active_uniform : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetActiveUniform.xhtml}
      [glGetActiveUniform]} [program index bufSize length size type_ name] *)
  
  val get_active_uniform_block_name : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetActiveUniformBlockName.xhtml}
      [glGetActiveUniformBlockName]} [program uniformBlockIndex bufSize
        length uniformBlockName] *)
  
  val get_active_uniform_blockiv : int -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetActiveUniformBlock.xhtml}
      [glGetActiveUniformBlockiv]} [program uniformBlockIndex pname params] *)
  
  val get_active_uniformsiv : int -> int -> uint32_bigarray -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetActiveUniformsiv.xhtml}
      [glGetActiveUniformsiv]} [program uniformCount uniformIndices pname
        params] *)
  
  val get_attached_shaders : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetAttachedShaders.xhtml}
      [glGetAttachedShaders]} [program maxCount count shaders] *)
  
  val get_attrib_location : int -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetAttribLocation.xhtml}
      [glGetAttribLocation]} [program name] *)
  
  val get_booleani_v : enum -> int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGet.xhtml}
      [glGetBooleani_v]} [target index data] *)
  
  val get_booleanv : enum -> (int, Bigarray.int8_unsigned_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGet.xhtml}
      [glGetBooleanv]} [pname data] *)
  
  val get_buffer_parameteri64v : enum -> enum ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetBufferParameter.xhtml}
      [glGetBufferParameteri64v]} [target pname params] *)
  
  val get_buffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetBufferParameter.xhtml}
      [glGetBufferParameteriv]} [target pname params] *)
  
  val get_buffer_pointerv : enum -> enum ->
    (nativeint, Bigarray.nativeint_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetBufferPointerv.xhtml}
      [glGetBufferPointerv]} [target pname params] *)
  
  val get_debug_message_log : int -> int -> enum_bigarray -> enum_bigarray ->
    uint32_bigarray option -> enum_bigarray ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray option -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetDebugMessageLog.xhtml}
      [glGetDebugMessageLog]} [count bufSize sources types ids severities
        lengths messageLog] *)
  
  val get_error : unit -> enum
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetError.xhtml}
      [glGetError]} [()] *)
  
  val get_floatv : enum -> (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGet.xhtml}
      [glGetFloatv]} [pname data] *)
  
  val get_frag_data_location : int -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetFragDataLocation.xhtml}
      [glGetFragDataLocation]} [program name] *)
  
  val get_framebuffer_attachment_parameteriv : enum -> enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetFramebufferAttachmentParameter.xhtml}
      [glGetFramebufferAttachmentParameteriv]} [target attachment pname
        params] *)
  
  val get_framebuffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetFramebufferParameter.xhtml}
      [glGetFramebufferParameteriv]} [target pname params] *)
  
  val get_graphics_reset_status : unit -> enum
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetGraphicsResetStatus.xhtml}
      [glGetGraphicsResetStatus]} [()] *)
  
  val get_integer64i_v : enum -> int ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGet.xhtml}
      [glGetInteger64i_v]} [target index data] *)
  
  val get_integer64v : enum -> (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGet.xhtml}
      [glGetInteger64v]} [pname data] *)
  
  val get_integeri_v : enum -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGet.xhtml}
      [glGetIntegeri_v]} [target index data] *)
  
  val get_integerv : enum -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGet.xhtml}
      [glGetIntegerv]} [pname data] *)
  
  val get_internalformativ : enum -> enum -> enum -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetInternalformat.xhtml}
      [glGetInternalformativ]} [target internalformat pname bufSize params] *)
  
  val get_multisamplefv : enum -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetMultisample.xhtml}
      [glGetMultisamplefv]} [pname index val_] *)
  
  val get_object_label : enum -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetObjectLabel.xhtml}
      [glGetObjectLabel]} [identifier name bufSize length label] *)
  
  val get_object_ptr_label : ('a, 'b) bigarray -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetObjectPtrLabel.xhtml}
      [glGetObjectPtrLabel]} [ptr bufSize length label] *)
  
  val get_pointerv : enum -> (nativeint, Bigarray.nativeint_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetPointerv.xhtml}
      [glGetPointerv]} [pname params] *)
  
  val get_program_binary : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option -> enum_bigarray ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramBinary.xhtml}
      [glGetProgramBinary]} [program bufSize length binaryFormat binary] *)
  
  val get_program_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramInfoLog.xhtml}
      [glGetProgramInfoLog]} [program bufSize length infoLog] *)
  
  val get_program_interfaceiv : int -> enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramInterface.xhtml}
      [glGetProgramInterfaceiv]} [program programInterface pname params] *)
  
  val get_program_pipeline_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramPipelineInfoLog.xhtml}
      [glGetProgramPipelineInfoLog]} [pipeline bufSize length infoLog] *)
  
  val get_program_pipelineiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramPipeline.xhtml}
      [glGetProgramPipelineiv]} [pipeline pname params] *)
  
  val get_program_resource_index : int -> enum -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramResourceIndex.xhtml}
      [glGetProgramResourceIndex]} [program programInterface name] *)
  
  val get_program_resource_location : int -> enum -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramResourceLocation.xhtml}
      [glGetProgramResourceLocation]} [program programInterface name] *)
  
  val get_program_resource_name : int -> enum -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramResourceName.xhtml}
      [glGetProgramResourceName]} [program programInterface index bufSize
        length name] *)
  
  val get_program_resourceiv : int -> enum -> int -> int -> enum_bigarray ->
    int -> (int32, Bigarray.int32_elt) bigarray ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgramResource.xhtml}
      [glGetProgramResourceiv]} [program programInterface index propCount
        props bufSize length params] *)
  
  val get_programiv : int -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetProgram.xhtml}
      [glGetProgramiv]} [program pname params] *)
  
  val get_query_objectuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetQueryObject.xhtml}
      [glGetQueryObjectuiv]} [id pname params] *)
  
  val get_queryiv : enum -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetQueryiv.xhtml}
      [glGetQueryiv]} [target pname params] *)
  
  val get_renderbuffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetRenderbufferParameter.xhtml}
      [glGetRenderbufferParameteriv]} [target pname params] *)
  
  val get_sampler_parameter_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetSamplerParameter.xhtml}
      [glGetSamplerParameterIiv]} [sampler pname params] *)
  
  val get_sampler_parameter_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetSamplerParameter.xhtml}
      [glGetSamplerParameterIuiv]} [sampler pname params] *)
  
  val get_sampler_parameterfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetSamplerParameter.xhtml}
      [glGetSamplerParameterfv]} [sampler pname params] *)
  
  val get_sampler_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetSamplerParameter.xhtml}
      [glGetSamplerParameteriv]} [sampler pname params] *)
  
  val get_shader_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetShaderInfoLog.xhtml}
      [glGetShaderInfoLog]} [shader bufSize length infoLog] *)
  
  val get_shader_precision_format : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetShaderPrecisionFormat.xhtml}
      [glGetShaderPrecisionFormat]} [shadertype precisiontype range
        precision] *)
  
  val get_shader_source : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetShaderSource.xhtml}
      [glGetShaderSource]} [shader bufSize length source] *)
  
  val get_shaderiv : int -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetShader.xhtml}
      [glGetShaderiv]} [shader pname params] *)
  
  val get_string : enum -> string option
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetString.xhtml}
      [glGetString]} [name] *)
  
  val get_stringi : enum -> int -> string option
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetString.xhtml}
      [glGetStringi]} [name index] *)
  
  val get_synciv : sync -> enum -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetSync.xhtml}
      [glGetSynciv]} [sync pname bufSize length values] *)
  
  val get_tex_level_parameterfv : enum -> int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetTexLevelParameter.xhtml}
      [glGetTexLevelParameterfv]} [target level pname params] *)
  
  val get_tex_level_parameteriv : enum -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetTexLevelParameter.xhtml}
      [glGetTexLevelParameteriv]} [target level pname params] *)
  
  val get_tex_parameter_iiv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetTexParameter.xhtml}
      [glGetTexParameterIiv]} [target pname params] *)
  
  val get_tex_parameter_iuiv : enum -> enum -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetTexParameter.xhtml}
      [glGetTexParameterIuiv]} [target pname params] *)
  
  val get_tex_parameterfv : enum -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetTexParameter.xhtml}
      [glGetTexParameterfv]} [target pname params] *)
  
  val get_tex_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetTexParameter.xhtml}
      [glGetTexParameteriv]} [target pname params] *)
  
  val get_transform_feedback_varying : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetTransformFeedbackVarying.xhtml}
      [glGetTransformFeedbackVarying]} [program index bufSize length size
        type_ name] *)
  
  val get_uniform_block_index : int -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniformBlockIndex.xhtml}
      [glGetUniformBlockIndex]} [program uniformBlockName] *)
  
  val get_uniform_indices : int -> string list -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniformIndices.xhtml}
      [glGetUniformIndices]} [program uniformNames uniformIndices] *)
  val get_uniform_location : int -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniformLocation.xhtml}
      [glGetUniformLocation]} [program name] *)
  
  val get_uniformfv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniform.xhtml}
      [glGetUniformfv]} [program location params] *)
  
  val get_uniformiv : int -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniform.xhtml}
      [glGetUniformiv]} [program location params] *)
  
  val get_uniformuiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniform.xhtml}
      [glGetUniformuiv]} [program location params] *)
  
  val get_vertex_attrib_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribIiv]} [index pname params] *)
  
  val get_vertex_attrib_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribIuiv]} [index pname params] *)
  
  val get_vertex_attrib_pointerv : int -> enum ->
    (nativeint, Bigarray.nativeint_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetVertexAttribPointerv.xhtml}
      [glGetVertexAttribPointerv]} [index pname pointer] *)
  
  val get_vertex_attribfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribfv]} [index pname params] *)
  
  val get_vertex_attribiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribiv]} [index pname params] *)
  
  val getn_uniformfv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniform.xhtml}
      [glGetnUniformfv]} [program location bufSize params] *)
  
  val getn_uniformiv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniform.xhtml}
      [glGetnUniformiv]} [program location bufSize params] *)
  
  val getn_uniformuiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glGetUniform.xhtml}
      [glGetnUniformuiv]} [program location bufSize params] *)
  
  val hint : enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glHint.xhtml}
      [glHint]} [target mode] *)
  
  val invalidate_framebuffer : enum -> int -> enum_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glInvalidateFramebuffer.xhtml}
      [glInvalidateFramebuffer]} [target numAttachments attachments] *)
  
  val invalidate_sub_framebuffer : enum -> int -> enum_bigarray -> int ->
    int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glInvalidateSubFramebuffer.xhtml}
      [glInvalidateSubFramebuffer]} [target numAttachments attachments x y
        width height] *)
  
  val is_buffer : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsBuffer.xhtml}
      [glIsBuffer]} [buffer] *)
  
  val is_enabled : enum -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsEnabled.xhtml}
      [glIsEnabled]} [cap] *)
  
  val is_enabledi : enum -> int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsEnabled.xhtml}
      [glIsEnabledi]} [target index] *)
  
  val is_framebuffer : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsFramebuffer.xhtml}
      [glIsFramebuffer]} [framebuffer] *)
  
  val is_program : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsProgram.xhtml}
      [glIsProgram]} [program] *)
  
  val is_program_pipeline : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsProgramPipeline.xhtml}
      [glIsProgramPipeline]} [pipeline] *)
  
  val is_query : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsQuery.xhtml}
      [glIsQuery]} [id] *)
  
  val is_renderbuffer : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsRenderbuffer.xhtml}
      [glIsRenderbuffer]} [renderbuffer] *)
  
  val is_sampler : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsSampler.xhtml}
      [glIsSampler]} [sampler] *)
  
  val is_shader : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsShader.xhtml}
      [glIsShader]} [shader] *)
  
  val is_sync : sync -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsSync.xhtml}
      [glIsSync]} [sync] *)
  
  val is_texture : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsTexture.xhtml}
      [glIsTexture]} [texture] *)
  
  val is_transform_feedback : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsTransformFeedback.xhtml}
      [glIsTransformFeedback]} [id] *)
  
  val is_vertex_array : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glIsVertexArray.xhtml}
      [glIsVertexArray]} [array] *)
  
  val line_width : float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glLineWidth.xhtml}
      [glLineWidth]} [width] *)
  
  val link_program : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glLinkProgram.xhtml}
      [glLinkProgram]} [program] *)
  
  val map_buffer_range : enum -> int -> int -> enum ->
    ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glMapBufferRange.xhtml}
      [glMapBufferRange]} [target offset length access kind]
  
      {b Note.} [length] is the length in number of bigarray elements of the
      mapped buffer. [offset] is in bytes.
  
      {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
      program termination may happen if you don't respect the access policy. *)
  
  val memory_barrier : bitfield -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glMemoryBarrier.xhtml}
      [glMemoryBarrier]} [barriers]
      
      {b Warning.} On 32 bits platforms the constant
                {!all_barrier_bits} is represented by 0x7FFFFFFF
                instead of 0xFFFFFFFF, this may result in an OpenGL
                error (or not). *)
  
  val memory_barrier_by_region : bitfield -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glMemoryBarrier.xhtml}
      [glMemoryBarrierByRegion]} [barriers]
      
      {b Warning.} On 32 bits platforms the constant
                {!all_barrier_bits} is represented by 0x7FFFFFFF
                instead of 0xFFFFFFFF, this may result in an OpenGL
                error (or not). *)
  
  val min_sample_shading : float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glMinSampleShading.xhtml}
      [glMinSampleShading]} [value] *)
  
  val object_label : enum -> int -> int -> string option -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glObjectLabel.xhtml}
      [glObjectLabel]} [identifier name length label] *)
  
  val object_ptr_label : ('a, 'b) bigarray -> int -> string option -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glObjectPtrLabel.xhtml}
      [glObjectPtrLabel]} [ptr length label] *)
  
  val patch_parameteri : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glPatchParameter.xhtml}
      [glPatchParameteri]} [pname value] *)
  
  val pause_transform_feedback : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glPauseTransformFeedback.xhtml}
      [glPauseTransformFeedback]} [()] *)
  
  val pixel_storei : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glPixelStore.xhtml}
      [glPixelStorei]} [pname param] *)
  
  val polygon_offset : float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glPolygonOffset.xhtml}
      [glPolygonOffset]} [factor units] *)
  
  val pop_debug_group : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glPopDebugGroup.xhtml}
      [glPopDebugGroup]} [()] *)
  
  val primitive_bounding_box : float -> float -> float -> float -> float ->
    float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glPrimitiveBoundingBox.xhtml}
      [glPrimitiveBoundingBox]} [minX minY minZ minW maxX maxY maxZ maxW] *)
  
  val program_binary : int -> enum -> ('a, 'b) bigarray -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramBinary.xhtml}
      [glProgramBinary]} [program binaryFormat binary length] *)
  
  val program_parameteri : int -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramParameter.xhtml}
      [glProgramParameteri]} [program pname value] *)
  
  val program_uniform1f : int -> int -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform1f]} [program location v0] *)
  
  val program_uniform1fv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform1fv]} [program location count value] *)
  
  val program_uniform1i : int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform1i]} [program location v0] *)
  
  val program_uniform1iv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform1iv]} [program location count value] *)
  
  val program_uniform1ui : int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform1ui]} [program location v0] *)
  
  val program_uniform1uiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform1uiv]} [program location count value] *)
  
  val program_uniform2f : int -> int -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform2f]} [program location v0 v1] *)
  
  val program_uniform2fv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform2fv]} [program location count value] *)
  
  val program_uniform2i : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform2i]} [program location v0 v1] *)
  
  val program_uniform2iv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform2iv]} [program location count value] *)
  
  val program_uniform2ui : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform2ui]} [program location v0 v1] *)
  
  val program_uniform2uiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform2uiv]} [program location count value] *)
  
  val program_uniform3f : int -> int -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform3f]} [program location v0 v1 v2] *)
  
  val program_uniform3fv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform3fv]} [program location count value] *)
  
  val program_uniform3i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform3i]} [program location v0 v1 v2] *)
  
  val program_uniform3iv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform3iv]} [program location count value] *)
  
  val program_uniform3ui : int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform3ui]} [program location v0 v1 v2] *)
  
  val program_uniform3uiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform3uiv]} [program location count value] *)
  
  val program_uniform4f : int -> int -> float -> float -> float -> float ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform4f]} [program location v0 v1 v2 v3] *)
  
  val program_uniform4fv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform4fv]} [program location count value] *)
  
  val program_uniform4i : int -> int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform4i]} [program location v0 v1 v2 v3] *)
  
  val program_uniform4iv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform4iv]} [program location count value] *)
  
  val program_uniform4ui : int -> int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform4ui]} [program location v0 v1 v2 v3] *)
  
  val program_uniform4uiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniform4uiv]} [program location count value] *)
  
  val program_uniform_matrix2fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2fv]} [program location count transpose value] *)
  
  val program_uniform_matrix2x3fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2x3fv]} [program location count transpose value] *)
  
  val program_uniform_matrix2x4fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2x4fv]} [program location count transpose value] *)
  
  val program_uniform_matrix3fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3fv]} [program location count transpose value] *)
  
  val program_uniform_matrix3x2fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3x2fv]} [program location count transpose value] *)
  
  val program_uniform_matrix3x4fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3x4fv]} [program location count transpose value] *)
  
  val program_uniform_matrix4fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4fv]} [program location count transpose value] *)
  
  val program_uniform_matrix4x2fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4x2fv]} [program location count transpose value] *)
  
  val program_uniform_matrix4x3fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4x3fv]} [program location count transpose value] *)
  
  val push_debug_group : enum -> int -> int -> string -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glPushDebugGroup.xhtml}
      [glPushDebugGroup]} [source id length message] *)
  
  val read_buffer : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glReadBuffer.xhtml}
      [glReadBuffer]} [src] *)
  
  val read_pixels : int -> int -> int -> int -> enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glReadPixels.xhtml}
      [glReadPixels]} [x y width height format type_ pixels] *)
  
  val readn_pixels : int -> int -> int -> int -> enum -> enum -> int ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glReadPixels.xhtml}
      [glReadnPixels]} [x y width height format type_ bufSize data] *)
  
  val release_shader_compiler : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glReleaseShaderCompiler.xhtml}
      [glReleaseShaderCompiler]} [()] *)
  
  val renderbuffer_storage : enum -> enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glRenderbufferStorage.xhtml}
      [glRenderbufferStorage]} [target internalformat width height] *)
  
  val renderbuffer_storage_multisample : enum -> int -> enum -> int -> int ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glRenderbufferStorageMultisample.xhtml}
      [glRenderbufferStorageMultisample]} [target samples internalformat
        width height] *)
  
  val resume_transform_feedback : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glResumeTransformFeedback.xhtml}
      [glResumeTransformFeedback]} [()] *)
  
  val sample_coverage : float -> bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glSampleCoverage.xhtml}
      [glSampleCoverage]} [value invert] *)
  
  val sample_maski : int -> bitfield -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glSampleMaski.xhtml}
      [glSampleMaski]} [maskNumber mask] *)
  
  val sampler_parameter_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glSamplerParameter.xhtml}
      [glSamplerParameterIiv]} [sampler pname param] *)
  
  val sampler_parameter_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glSamplerParameter.xhtml}
      [glSamplerParameterIuiv]} [sampler pname param] *)
  
  val sampler_parameterf : int -> enum -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glSamplerParameter.xhtml}
      [glSamplerParameterf]} [sampler pname param] *)
  
  val sampler_parameterfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glSamplerParameter.xhtml}
      [glSamplerParameterfv]} [sampler pname param] *)
  
  val sampler_parameteri : int -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glSamplerParameter.xhtml}
      [glSamplerParameteri]} [sampler pname param] *)
  
  val sampler_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glSamplerParameter.xhtml}
      [glSamplerParameteriv]} [sampler pname param] *)
  
  val scissor : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glScissor.xhtml}
      [glScissor]} [x y width height] *)
  
  val shader_binary : int -> uint32_bigarray -> enum -> ('a, 'b) bigarray ->
    int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glShaderBinary.xhtml}
      [glShaderBinary]} [count shaders binaryformat binary length] *)
  
  val shader_source : int -> string -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glShaderSource.xhtml}
      [glShaderSource]} [shader source] *)
  
  val stencil_func : enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glStencilFunc.xhtml}
      [glStencilFunc]} [func ref mask] *)
  
  val stencil_func_separate : enum -> enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glStencilFuncSeparate.xhtml}
      [glStencilFuncSeparate]} [face func ref mask] *)
  
  val stencil_mask : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glStencilMask.xhtml}
      [glStencilMask]} [mask] *)
  
  val stencil_mask_separate : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glStencilMaskSeparate.xhtml}
      [glStencilMaskSeparate]} [face mask] *)
  
  val stencil_op : enum -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glStencilOp.xhtml}
      [glStencilOp]} [fail zfail zpass] *)
  
  val stencil_op_separate : enum -> enum -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glStencilOpSeparate.xhtml}
      [glStencilOpSeparate]} [face sfail dpfail dppass] *)
  
  val tex_buffer : enum -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexBuffer.xhtml}
      [glTexBuffer]} [target internalformat buffer] *)
  
  val tex_buffer_range : enum -> enum -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexBufferRange.xhtml}
      [glTexBufferRange]} [target internalformat buffer offset size] *)
  
  val tex_image2d : enum -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexImage2D.xhtml}
      [glTexImage2D]} [target level internalformat width height border format
        type_ pixels] *)
  
  val tex_image3d : enum -> int -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexImage3D.xhtml}
      [glTexImage3D]} [target level internalformat width height depth border
        format type_ pixels] *)
  
  val tex_parameter_iiv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexParameter.xhtml}
      [glTexParameterIiv]} [target pname params] *)
  
  val tex_parameter_iuiv : enum -> enum -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexParameter.xhtml}
      [glTexParameterIuiv]} [target pname params] *)
  
  val tex_parameterf : enum -> enum -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexParameter.xhtml}
      [glTexParameterf]} [target pname param] *)
  
  val tex_parameterfv : enum -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexParameter.xhtml}
      [glTexParameterfv]} [target pname params] *)
  
  val tex_parameteri : enum -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexParameter.xhtml}
      [glTexParameteri]} [target pname param] *)
  
  val tex_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexParameter.xhtml}
      [glTexParameteriv]} [target pname params] *)
  
  val tex_storage2d : enum -> int -> enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexStorage2D.xhtml}
      [glTexStorage2D]} [target levels internalformat width height] *)
  
  val tex_storage2d_multisample : enum -> int -> enum -> int -> int ->
    bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexStorage2DMultisample.xhtml}
      [glTexStorage2DMultisample]} [target samples internalformat width
        height fixedsamplelocations] *)
  
  val tex_storage3d : enum -> int -> enum -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexStorage3D.xhtml}
      [glTexStorage3D]} [target levels internalformat width height depth] *)
  
  val tex_storage3d_multisample : enum -> int -> enum -> int -> int -> int ->
    bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexStorage3DMultisample.xhtml}
      [glTexStorage3DMultisample]} [target samples internalformat width
        height depth fixedsamplelocations] *)
  
  val tex_sub_image2d : enum -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexSubImage2D.xhtml}
      [glTexSubImage2D]} [target level xoffset yoffset width height format
        type_ pixels] *)
  
  val tex_sub_image3d : enum -> int -> int -> int -> int -> int -> int ->
    int -> enum -> enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTexSubImage3D.xhtml}
      [glTexSubImage3D]} [target level xoffset yoffset zoffset width height
        depth format type_ pixels] *)
  
  val transform_feedback_varyings : int -> string list -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glTransformFeedbackVaryings.xhtml}
      [glTransformFeedbackVaryings]} [program varyings bufferMode] *)
  val uniform1f : int -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform1f]} [location v0] *)
  
  val uniform1fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform1fv]} [location count value] *)
  
  val uniform1i : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform1i]} [location v0] *)
  
  val uniform1iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform1iv]} [location count value] *)
  
  val uniform1ui : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform1ui]} [location v0] *)
  
  val uniform1uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform1uiv]} [location count value] *)
  
  val uniform2f : int -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform2f]} [location v0 v1] *)
  
  val uniform2fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform2fv]} [location count value] *)
  
  val uniform2i : int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform2i]} [location v0 v1] *)
  
  val uniform2iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform2iv]} [location count value] *)
  
  val uniform2ui : int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform2ui]} [location v0 v1] *)
  
  val uniform2uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform2uiv]} [location count value] *)
  
  val uniform3f : int -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform3f]} [location v0 v1 v2] *)
  
  val uniform3fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform3fv]} [location count value] *)
  
  val uniform3i : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform3i]} [location v0 v1 v2] *)
  
  val uniform3iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform3iv]} [location count value] *)
  
  val uniform3ui : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform3ui]} [location v0 v1 v2] *)
  
  val uniform3uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform3uiv]} [location count value] *)
  
  val uniform4f : int -> float -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform4f]} [location v0 v1 v2 v3] *)
  
  val uniform4fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform4fv]} [location count value] *)
  
  val uniform4i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform4i]} [location v0 v1 v2 v3] *)
  
  val uniform4iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform4iv]} [location count value] *)
  
  val uniform4ui : int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform4ui]} [location v0 v1 v2 v3] *)
  
  val uniform4uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniform4uiv]} [location count value] *)
  
  val uniform_block_binding : int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniformBlockBinding.xhtml}
      [glUniformBlockBinding]} [program uniformBlockIndex
        uniformBlockBinding] *)
  
  val uniform_matrix2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix2fv]} [location count transpose value] *)
  
  val uniform_matrix2x3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix2x3fv]} [location count transpose value] *)
  
  val uniform_matrix2x4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix2x4fv]} [location count transpose value] *)
  
  val uniform_matrix3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix3fv]} [location count transpose value] *)
  
  val uniform_matrix3x2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix3x2fv]} [location count transpose value] *)
  
  val uniform_matrix3x4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix3x4fv]} [location count transpose value] *)
  
  val uniform_matrix4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix4fv]} [location count transpose value] *)
  
  val uniform_matrix4x2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix4x2fv]} [location count transpose value] *)
  
  val uniform_matrix4x3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUniform.xhtml}
      [glUniformMatrix4x3fv]} [location count transpose value] *)
  
  val unmap_buffer : enum -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUnmapBuffer.xhtml}
      [glUnmapBuffer]} [target] *)
  
  val use_program : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUseProgram.xhtml}
      [glUseProgram]} [program] *)
  
  val use_program_stages : int -> bitfield -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glUseProgramStages.xhtml}
      [glUseProgramStages]} [pipeline stages program]
      
      {b Warning.} On 32 bits platforms the constant
                {!all_shader_bits} is represented by 0x7FFFFFFF
                instead of 0xFFFFFFFF, this may result in an OpenGL
                error (or not). *)
  
  val validate_program : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glValidateProgram.xhtml}
      [glValidateProgram]} [program] *)
  
  val validate_program_pipeline : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glValidateProgramPipeline.xhtml}
      [glValidateProgramPipeline]} [pipeline] *)
  
  val vertex_attrib1f : int -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttrib1f]} [index x] *)
  
  val vertex_attrib1fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttrib1fv]} [index v] *)
  
  val vertex_attrib2f : int -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttrib2f]} [index x y] *)
  
  val vertex_attrib2fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttrib2fv]} [index v] *)
  
  val vertex_attrib3f : int -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttrib3f]} [index x y z] *)
  
  val vertex_attrib3fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttrib3fv]} [index v] *)
  
  val vertex_attrib4f : int -> float -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttrib4f]} [index x y z w] *)
  
  val vertex_attrib4fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttrib4fv]} [index v] *)
  
  val vertex_attrib_binding : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttribBinding.xhtml}
      [glVertexAttribBinding]} [attribindex bindingindex] *)
  
  val vertex_attrib_divisor : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttribDivisor.xhtml}
      [glVertexAttribDivisor]} [index divisor] *)
  
  val vertex_attrib_format : int -> int -> enum -> bool -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttribFormat.xhtml}
      [glVertexAttribFormat]} [attribindex size type_ normalized
        relativeoffset] *)
  
  val vertex_attrib_i4i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttribI4i]} [index x y z w] *)
  
  val vertex_attrib_i4iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttribI4iv]} [index v] *)
  
  val vertex_attrib_i4ui : int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttribI4ui]} [index x y z w] *)
  
  val vertex_attrib_i4uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttrib.xhtml}
      [glVertexAttribI4uiv]} [index v] *)
  
  val vertex_attrib_iformat : int -> int -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttribFormat.xhtml}
      [glVertexAttribIFormat]} [attribindex size type_ relativeoffset] *)
  
  val vertex_attrib_ipointer : int -> int -> enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttribPointer.xhtml}
      [glVertexAttribIPointer]} [index size type_ stride pointer] *)
  
  val vertex_attrib_pointer : int -> int -> enum -> bool -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexAttribPointer.xhtml}
      [glVertexAttribPointer]} [index size type_ normalized stride pointer] *)
  
  val vertex_binding_divisor : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glVertexBindingDivisor.xhtml}
      [glVertexBindingDivisor]} [bindingindex divisor] *)
  
  val viewport : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glViewport.xhtml}
      [glViewport]} [x y width height] *)
  
  val wait_sync : sync -> bitfield -> uint64 -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man32/html/glWaitSync.xhtml}
      [glWaitSync]} [sync flags timeout] *)
  
  (** {1:enums Enums} *)

  val active_atomic_counter_buffers : enum
  
  val active_attributes : enum
  
  val active_attribute_max_length : enum
  
  val active_program : enum
  
  val active_resources : enum
  
  val active_texture_enum : enum
  
  val active_uniforms : enum
  
  val active_uniform_blocks : enum
  
  val active_uniform_block_max_name_length : enum
  
  val active_uniform_max_length : enum
  
  val active_variables : enum
  
  val aliased_line_width_range : enum
  
  val aliased_point_size_range : enum
  
  val all_barrier_bits : enum
  
  val all_shader_bits : enum
  
  val alpha : enum
  
  val alpha_bits : enum
  
  val already_signaled : enum
  
  val always : enum
  
  val any_samples_passed : enum
  
  val any_samples_passed_conservative : enum
  
  val array_buffer : enum
  
  val array_buffer_binding : enum
  
  val array_size : enum
  
  val array_stride : enum
  
  val atomic_counter_barrier_bit : enum
  
  val atomic_counter_buffer : enum
  
  val atomic_counter_buffer_binding : enum
  
  val atomic_counter_buffer_index : enum
  
  val atomic_counter_buffer_size : enum
  
  val atomic_counter_buffer_start : enum
  
  val attached_shaders : enum
  
  val back : enum
  
  val blend : enum
  
  val blend_color_enum : enum
  
  val blend_dst_alpha : enum
  
  val blend_dst_rgb : enum
  
  val blend_equation_enum : enum
  
  val blend_equation_alpha : enum
  
  val blend_equation_rgb : enum
  
  val blend_src_alpha : enum
  
  val blend_src_rgb : enum
  
  val block_index : enum
  
  val blue : enum
  
  val blue_bits : enum
  
  val bool : enum
  
  val bool_vec2 : enum
  
  val bool_vec3 : enum
  
  val bool_vec4 : enum
  
  val buffer : enum
  
  val buffer_access_flags : enum
  
  val buffer_binding : enum
  
  val buffer_data_size : enum
  
  val buffer_mapped : enum
  
  val buffer_map_length : enum
  
  val buffer_map_offset : enum
  
  val buffer_map_pointer : enum
  
  val buffer_size : enum
  
  val buffer_update_barrier_bit : enum
  
  val buffer_usage : enum
  
  val buffer_variable : enum
  
  val byte : enum
  
  val ccw : enum
  
  val clamp_to_border : enum
  
  val clamp_to_edge : enum
  
  val color : enum
  
  val colorburn : enum
  
  val colordodge : enum
  
  val color_attachment0 : enum
  
  val color_attachment1 : enum
  
  val color_attachment10 : enum
  
  val color_attachment11 : enum
  
  val color_attachment12 : enum
  
  val color_attachment13 : enum
  
  val color_attachment14 : enum
  
  val color_attachment15 : enum
  
  val color_attachment16 : enum
  
  val color_attachment17 : enum
  
  val color_attachment18 : enum
  
  val color_attachment19 : enum
  
  val color_attachment2 : enum
  
  val color_attachment20 : enum
  
  val color_attachment21 : enum
  
  val color_attachment22 : enum
  
  val color_attachment23 : enum
  
  val color_attachment24 : enum
  
  val color_attachment25 : enum
  
  val color_attachment26 : enum
  
  val color_attachment27 : enum
  
  val color_attachment28 : enum
  
  val color_attachment29 : enum
  
  val color_attachment3 : enum
  
  val color_attachment30 : enum
  
  val color_attachment31 : enum
  
  val color_attachment4 : enum
  
  val color_attachment5 : enum
  
  val color_attachment6 : enum
  
  val color_attachment7 : enum
  
  val color_attachment8 : enum
  
  val color_attachment9 : enum
  
  val color_buffer_bit : enum
  
  val color_clear_value : enum
  
  val color_writemask : enum
  
  val command_barrier_bit : enum
  
  val compare_ref_to_texture : enum
  
  val compile_status : enum
  
  val compressed_r11_eac : enum
  
  val compressed_rg11_eac : enum
  
  val compressed_rgb8_etc2 : enum
  
  val compressed_rgb8_punchthrough_alpha1_etc2 : enum
  
  val compressed_rgba8_etc2_eac : enum
  
  val compressed_rgba_astc_10x10 : enum
  
  val compressed_rgba_astc_10x5 : enum
  
  val compressed_rgba_astc_10x6 : enum
  
  val compressed_rgba_astc_10x8 : enum
  
  val compressed_rgba_astc_12x10 : enum
  
  val compressed_rgba_astc_12x12 : enum
  
  val compressed_rgba_astc_4x4 : enum
  
  val compressed_rgba_astc_5x4 : enum
  
  val compressed_rgba_astc_5x5 : enum
  
  val compressed_rgba_astc_6x5 : enum
  
  val compressed_rgba_astc_6x6 : enum
  
  val compressed_rgba_astc_8x5 : enum
  
  val compressed_rgba_astc_8x6 : enum
  
  val compressed_rgba_astc_8x8 : enum
  
  val compressed_signed_r11_eac : enum
  
  val compressed_signed_rg11_eac : enum
  
  val compressed_srgb8_alpha8_astc_10x10 : enum
  
  val compressed_srgb8_alpha8_astc_10x5 : enum
  
  val compressed_srgb8_alpha8_astc_10x6 : enum
  
  val compressed_srgb8_alpha8_astc_10x8 : enum
  
  val compressed_srgb8_alpha8_astc_12x10 : enum
  
  val compressed_srgb8_alpha8_astc_12x12 : enum
  
  val compressed_srgb8_alpha8_astc_4x4 : enum
  
  val compressed_srgb8_alpha8_astc_5x4 : enum
  
  val compressed_srgb8_alpha8_astc_5x5 : enum
  
  val compressed_srgb8_alpha8_astc_6x5 : enum
  
  val compressed_srgb8_alpha8_astc_6x6 : enum
  
  val compressed_srgb8_alpha8_astc_8x5 : enum
  
  val compressed_srgb8_alpha8_astc_8x6 : enum
  
  val compressed_srgb8_alpha8_astc_8x8 : enum
  
  val compressed_srgb8_alpha8_etc2_eac : enum
  
  val compressed_srgb8_etc2 : enum
  
  val compressed_srgb8_punchthrough_alpha1_etc2 : enum
  
  val compressed_texture_formats : enum
  
  val compute_shader : enum
  
  val compute_shader_bit : enum
  
  val compute_work_group_size : enum
  
  val condition_satisfied : enum
  
  val constant_alpha : enum
  
  val constant_color : enum
  
  val context_flags : enum
  
  val context_flag_debug_bit : enum
  
  val context_flag_robust_access_bit : enum
  
  val context_lost : enum
  
  val copy_read_buffer : enum
  
  val copy_read_buffer_binding : enum
  
  val copy_write_buffer : enum
  
  val copy_write_buffer_binding : enum
  
  val cull_face_enum : enum
  
  val cull_face_mode : enum
  
  val current_program : enum
  
  val current_query : enum
  
  val current_vertex_attrib : enum
  
  val cw : enum
  
  val darken : enum
  
  val debug_callback_function : enum
  
  val debug_callback_user_param : enum
  
  val debug_group_stack_depth : enum
  
  val debug_logged_messages : enum
  
  val debug_next_logged_message_length : enum
  
  val debug_output : enum
  
  val debug_output_synchronous : enum
  
  val debug_severity_high : enum
  
  val debug_severity_low : enum
  
  val debug_severity_medium : enum
  
  val debug_severity_notification : enum
  
  val debug_source_api : enum
  
  val debug_source_application : enum
  
  val debug_source_other : enum
  
  val debug_source_shader_compiler : enum
  
  val debug_source_third_party : enum
  
  val debug_source_window_system : enum
  
  val debug_type_deprecated_behavior : enum
  
  val debug_type_error : enum
  
  val debug_type_marker : enum
  
  val debug_type_other : enum
  
  val debug_type_performance : enum
  
  val debug_type_pop_group : enum
  
  val debug_type_portability : enum
  
  val debug_type_push_group : enum
  
  val debug_type_undefined_behavior : enum
  
  val decr : enum
  
  val decr_wrap : enum
  
  val delete_status : enum
  
  val depth : enum
  
  val depth24_stencil8 : enum
  
  val depth32f_stencil8 : enum
  
  val depth_attachment : enum
  
  val depth_bits : enum
  
  val depth_buffer_bit : enum
  
  val depth_clear_value : enum
  
  val depth_component : enum
  
  val depth_component16 : enum
  
  val depth_component24 : enum
  
  val depth_component32f : enum
  
  val depth_func_enum : enum
  
  val depth_range : enum
  
  val depth_stencil : enum
  
  val depth_stencil_attachment : enum
  
  val depth_stencil_texture_mode : enum
  
  val depth_test : enum
  
  val depth_writemask : enum
  
  val difference : enum
  
  val dispatch_indirect_buffer : enum
  
  val dispatch_indirect_buffer_binding : enum
  
  val dither : enum
  
  val dont_care : enum
  
  val draw_buffer0 : enum
  
  val draw_buffer1 : enum
  
  val draw_buffer10 : enum
  
  val draw_buffer11 : enum
  
  val draw_buffer12 : enum
  
  val draw_buffer13 : enum
  
  val draw_buffer14 : enum
  
  val draw_buffer15 : enum
  
  val draw_buffer2 : enum
  
  val draw_buffer3 : enum
  
  val draw_buffer4 : enum
  
  val draw_buffer5 : enum
  
  val draw_buffer6 : enum
  
  val draw_buffer7 : enum
  
  val draw_buffer8 : enum
  
  val draw_buffer9 : enum
  
  val draw_framebuffer : enum
  
  val draw_framebuffer_binding : enum
  
  val draw_indirect_buffer : enum
  
  val draw_indirect_buffer_binding : enum
  
  val dst_alpha : enum
  
  val dst_color : enum
  
  val dynamic_copy : enum
  
  val dynamic_draw : enum
  
  val dynamic_read : enum
  
  val element_array_barrier_bit : enum
  
  val element_array_buffer : enum
  
  val element_array_buffer_binding : enum
  
  val equal : enum
  
  val exclusion : enum
  
  val extensions : enum
  
  val false_ : enum
  
  val fastest : enum
  
  val first_vertex_convention : enum
  
  val fixed : enum
  
  val float : enum
  
  val float_32_unsigned_int_24_8_rev : enum
  
  val float_mat2 : enum
  
  val float_mat2x3 : enum
  
  val float_mat2x4 : enum
  
  val float_mat3 : enum
  
  val float_mat3x2 : enum
  
  val float_mat3x4 : enum
  
  val float_mat4 : enum
  
  val float_mat4x2 : enum
  
  val float_mat4x3 : enum
  
  val float_vec2 : enum
  
  val float_vec3 : enum
  
  val float_vec4 : enum
  
  val fractional_even : enum
  
  val fractional_odd : enum
  
  val fragment_interpolation_offset_bits : enum
  
  val fragment_shader : enum
  
  val fragment_shader_bit : enum
  
  val fragment_shader_derivative_hint : enum
  
  val framebuffer : enum
  
  val framebuffer_attachment_alpha_size : enum
  
  val framebuffer_attachment_blue_size : enum
  
  val framebuffer_attachment_color_encoding : enum
  
  val framebuffer_attachment_component_type : enum
  
  val framebuffer_attachment_depth_size : enum
  
  val framebuffer_attachment_green_size : enum
  
  val framebuffer_attachment_layered : enum
  
  val framebuffer_attachment_object_name : enum
  
  val framebuffer_attachment_object_type : enum
  
  val framebuffer_attachment_red_size : enum
  
  val framebuffer_attachment_stencil_size : enum
  
  val framebuffer_attachment_texture_cube_map_face : enum
  
  val framebuffer_attachment_texture_layer : enum
  
  val framebuffer_attachment_texture_level : enum
  
  val framebuffer_barrier_bit : enum
  
  val framebuffer_binding : enum
  
  val framebuffer_complete : enum
  
  val framebuffer_default : enum
  
  val framebuffer_default_fixed_sample_locations : enum
  
  val framebuffer_default_height : enum
  
  val framebuffer_default_layers : enum
  
  val framebuffer_default_samples : enum
  
  val framebuffer_default_width : enum
  
  val framebuffer_incomplete_attachment : enum
  
  val framebuffer_incomplete_dimensions : enum
  
  val framebuffer_incomplete_layer_targets : enum
  
  val framebuffer_incomplete_missing_attachment : enum
  
  val framebuffer_incomplete_multisample : enum
  
  val framebuffer_undefined : enum
  
  val framebuffer_unsupported : enum
  
  val front : enum
  
  val front_and_back : enum
  
  val front_face_enum : enum
  
  val func_add : enum
  
  val func_reverse_subtract : enum
  
  val func_subtract : enum
  
  val generate_mipmap_hint : enum
  
  val geometry_input_type : enum
  
  val geometry_output_type : enum
  
  val geometry_shader : enum
  
  val geometry_shader_bit : enum
  
  val geometry_shader_invocations : enum
  
  val geometry_vertices_out : enum
  
  val gequal : enum
  
  val greater : enum
  
  val green : enum
  
  val green_bits : enum
  
  val guilty_context_reset : enum
  
  val half_float : enum
  
  val hardlight : enum
  
  val high_float : enum
  
  val high_int : enum
  
  val hsl_color : enum
  
  val hsl_hue : enum
  
  val hsl_luminosity : enum
  
  val hsl_saturation : enum
  
  val image_2d : enum
  
  val image_2d_array : enum
  
  val image_3d : enum
  
  val image_binding_access : enum
  
  val image_binding_format : enum
  
  val image_binding_layer : enum
  
  val image_binding_layered : enum
  
  val image_binding_level : enum
  
  val image_binding_name : enum
  
  val image_buffer : enum
  
  val image_cube : enum
  
  val image_cube_map_array : enum
  
  val image_format_compatibility_by_class : enum
  
  val image_format_compatibility_by_size : enum
  
  val image_format_compatibility_type : enum
  
  val implementation_color_read_format : enum
  
  val implementation_color_read_type : enum
  
  val incr : enum
  
  val incr_wrap : enum
  
  val info_log_length : enum
  
  val innocent_context_reset : enum
  
  val int : enum
  
  val interleaved_attribs : enum
  
  val int_2_10_10_10_rev : enum
  
  val int_image_2d : enum
  
  val int_image_2d_array : enum
  
  val int_image_3d : enum
  
  val int_image_buffer : enum
  
  val int_image_cube : enum
  
  val int_image_cube_map_array : enum
  
  val int_sampler_2d : enum
  
  val int_sampler_2d_array : enum
  
  val int_sampler_2d_multisample : enum
  
  val int_sampler_2d_multisample_array : enum
  
  val int_sampler_3d : enum
  
  val int_sampler_buffer : enum
  
  val int_sampler_cube : enum
  
  val int_sampler_cube_map_array : enum
  
  val int_vec2 : enum
  
  val int_vec3 : enum
  
  val int_vec4 : enum
  
  val invalid_enum : enum
  
  val invalid_framebuffer_operation : enum
  
  val invalid_index : int32
  
  val invalid_operation : enum
  
  val invalid_value : enum
  
  val invert : enum
  
  val isolines : enum
  
  val is_per_patch : enum
  
  val is_row_major : enum
  
  val keep : enum
  
  val last_vertex_convention : enum
  
  val layer_provoking_vertex : enum
  
  val lequal : enum
  
  val less : enum
  
  val lighten : enum
  
  val linear : enum
  
  val linear_mipmap_linear : enum
  
  val linear_mipmap_nearest : enum
  
  val lines : enum
  
  val lines_adjacency : enum
  
  val line_loop : enum
  
  val line_strip : enum
  
  val line_strip_adjacency : enum
  
  val line_width_enum : enum
  
  val link_status : enum
  
  val location : enum
  
  val lose_context_on_reset : enum
  
  val low_float : enum
  
  val low_int : enum
  
  val luminance : enum
  
  val luminance_alpha : enum
  
  val major_version : enum
  
  val map_flush_explicit_bit : enum
  
  val map_invalidate_buffer_bit : enum
  
  val map_invalidate_range_bit : enum
  
  val map_read_bit : enum
  
  val map_unsynchronized_bit : enum
  
  val map_write_bit : enum
  
  val matrix_stride : enum
  
  val max : enum
  
  val max_3d_texture_size : enum
  
  val max_array_texture_layers : enum
  
  val max_atomic_counter_buffer_bindings : enum
  
  val max_atomic_counter_buffer_size : enum
  
  val max_color_attachments : enum
  
  val max_color_texture_samples : enum
  
  val max_combined_atomic_counters : enum
  
  val max_combined_atomic_counter_buffers : enum
  
  val max_combined_compute_uniform_components : enum
  
  val max_combined_fragment_uniform_components : enum
  
  val max_combined_geometry_uniform_components : enum
  
  val max_combined_image_uniforms : enum
  
  val max_combined_shader_output_resources : enum
  
  val max_combined_shader_storage_blocks : enum
  
  val max_combined_tess_control_uniform_components : enum
  
  val max_combined_tess_evaluation_uniform_components : enum
  
  val max_combined_texture_image_units : enum
  
  val max_combined_uniform_blocks : enum
  
  val max_combined_vertex_uniform_components : enum
  
  val max_compute_atomic_counters : enum
  
  val max_compute_atomic_counter_buffers : enum
  
  val max_compute_image_uniforms : enum
  
  val max_compute_shader_storage_blocks : enum
  
  val max_compute_shared_memory_size : enum
  
  val max_compute_texture_image_units : enum
  
  val max_compute_uniform_blocks : enum
  
  val max_compute_uniform_components : enum
  
  val max_compute_work_group_count : enum
  
  val max_compute_work_group_invocations : enum
  
  val max_compute_work_group_size : enum
  
  val max_cube_map_texture_size : enum
  
  val max_debug_group_stack_depth : enum
  
  val max_debug_logged_messages : enum
  
  val max_debug_message_length : enum
  
  val max_depth_texture_samples : enum
  
  val max_draw_buffers : enum
  
  val max_elements_indices : enum
  
  val max_elements_vertices : enum
  
  val max_element_index : enum
  
  val max_fragment_atomic_counters : enum
  
  val max_fragment_atomic_counter_buffers : enum
  
  val max_fragment_image_uniforms : enum
  
  val max_fragment_input_components : enum
  
  val max_fragment_interpolation_offset : enum
  
  val max_fragment_shader_storage_blocks : enum
  
  val max_fragment_uniform_blocks : enum
  
  val max_fragment_uniform_components : enum
  
  val max_fragment_uniform_vectors : enum
  
  val max_framebuffer_height : enum
  
  val max_framebuffer_layers : enum
  
  val max_framebuffer_samples : enum
  
  val max_framebuffer_width : enum
  
  val max_geometry_atomic_counters : enum
  
  val max_geometry_atomic_counter_buffers : enum
  
  val max_geometry_image_uniforms : enum
  
  val max_geometry_input_components : enum
  
  val max_geometry_output_components : enum
  
  val max_geometry_output_vertices : enum
  
  val max_geometry_shader_invocations : enum
  
  val max_geometry_shader_storage_blocks : enum
  
  val max_geometry_texture_image_units : enum
  
  val max_geometry_total_output_components : enum
  
  val max_geometry_uniform_blocks : enum
  
  val max_geometry_uniform_components : enum
  
  val max_image_units : enum
  
  val max_integer_samples : enum
  
  val max_label_length : enum
  
  val max_name_length : enum
  
  val max_num_active_variables : enum
  
  val max_patch_vertices : enum
  
  val max_program_texel_offset : enum
  
  val max_program_texture_gather_offset : enum
  
  val max_renderbuffer_size : enum
  
  val max_samples : enum
  
  val max_sample_mask_words : enum
  
  val max_server_wait_timeout : enum
  
  val max_shader_storage_block_size : enum
  
  val max_shader_storage_buffer_bindings : enum
  
  val max_tess_control_atomic_counters : enum
  
  val max_tess_control_atomic_counter_buffers : enum
  
  val max_tess_control_image_uniforms : enum
  
  val max_tess_control_input_components : enum
  
  val max_tess_control_output_components : enum
  
  val max_tess_control_shader_storage_blocks : enum
  
  val max_tess_control_texture_image_units : enum
  
  val max_tess_control_total_output_components : enum
  
  val max_tess_control_uniform_blocks : enum
  
  val max_tess_control_uniform_components : enum
  
  val max_tess_evaluation_atomic_counters : enum
  
  val max_tess_evaluation_atomic_counter_buffers : enum
  
  val max_tess_evaluation_image_uniforms : enum
  
  val max_tess_evaluation_input_components : enum
  
  val max_tess_evaluation_output_components : enum
  
  val max_tess_evaluation_shader_storage_blocks : enum
  
  val max_tess_evaluation_texture_image_units : enum
  
  val max_tess_evaluation_uniform_blocks : enum
  
  val max_tess_evaluation_uniform_components : enum
  
  val max_tess_gen_level : enum
  
  val max_tess_patch_components : enum
  
  val max_texture_buffer_size : enum
  
  val max_texture_image_units : enum
  
  val max_texture_lod_bias : enum
  
  val max_texture_size : enum
  
  val max_transform_feedback_interleaved_components : enum
  
  val max_transform_feedback_separate_attribs : enum
  
  val max_transform_feedback_separate_components : enum
  
  val max_uniform_block_size : enum
  
  val max_uniform_buffer_bindings : enum
  
  val max_uniform_locations : enum
  
  val max_varying_components : enum
  
  val max_varying_vectors : enum
  
  val max_vertex_atomic_counters : enum
  
  val max_vertex_atomic_counter_buffers : enum
  
  val max_vertex_attribs : enum
  
  val max_vertex_attrib_bindings : enum
  
  val max_vertex_attrib_relative_offset : enum
  
  val max_vertex_attrib_stride : enum
  
  val max_vertex_image_uniforms : enum
  
  val max_vertex_output_components : enum
  
  val max_vertex_shader_storage_blocks : enum
  
  val max_vertex_texture_image_units : enum
  
  val max_vertex_uniform_blocks : enum
  
  val max_vertex_uniform_components : enum
  
  val max_vertex_uniform_vectors : enum
  
  val max_viewport_dims : enum
  
  val medium_float : enum
  
  val medium_int : enum
  
  val min : enum
  
  val minor_version : enum
  
  val min_fragment_interpolation_offset : enum
  
  val min_program_texel_offset : enum
  
  val min_program_texture_gather_offset : enum
  
  val min_sample_shading_value : enum
  
  val mirrored_repeat : enum
  
  val multiply : enum
  
  val multisample_line_width_granularity : enum
  
  val multisample_line_width_range : enum
  
  val name_length : enum
  
  val nearest : enum
  
  val nearest_mipmap_linear : enum
  
  val nearest_mipmap_nearest : enum
  
  val never : enum
  
  val nicest : enum
  
  val none : enum
  
  val notequal : enum
  
  val no_error : enum
  
  val no_reset_notification : enum
  
  val num_active_variables : enum
  
  val num_compressed_texture_formats : enum
  
  val num_extensions : enum
  
  val num_program_binary_formats : enum
  
  val num_sample_counts : enum
  
  val num_shader_binary_formats : enum
  
  val object_type : enum
  
  val offset : enum
  
  val one : enum
  
  val one_minus_constant_alpha : enum
  
  val one_minus_constant_color : enum
  
  val one_minus_dst_alpha : enum
  
  val one_minus_dst_color : enum
  
  val one_minus_src_alpha : enum
  
  val one_minus_src_color : enum
  
  val out_of_memory : enum
  
  val overlay : enum
  
  val pack_alignment : enum
  
  val pack_row_length : enum
  
  val pack_skip_pixels : enum
  
  val pack_skip_rows : enum
  
  val patches : enum
  
  val patch_vertices : enum
  
  val pixel_buffer_barrier_bit : enum
  
  val pixel_pack_buffer : enum
  
  val pixel_pack_buffer_binding : enum
  
  val pixel_unpack_buffer : enum
  
  val pixel_unpack_buffer_binding : enum
  
  val points : enum
  
  val polygon_offset_factor : enum
  
  val polygon_offset_fill : enum
  
  val polygon_offset_units : enum
  
  val primitives_generated : enum
  
  val primitive_bounding_box_enum : enum
  
  val primitive_restart_fixed_index : enum
  
  val primitive_restart_for_patches_supported : enum
  
  val program : enum
  
  val program_binary_formats : enum
  
  val program_binary_length : enum
  
  val program_binary_retrievable_hint : enum
  
  val program_input : enum
  
  val program_output : enum
  
  val program_pipeline : enum
  
  val program_pipeline_binding : enum
  
  val program_separable : enum
  
  val quads : enum
  
  val query : enum
  
  val query_result : enum
  
  val query_result_available : enum
  
  val r11f_g11f_b10f : enum
  
  val r16f : enum
  
  val r16i : enum
  
  val r16ui : enum
  
  val r32f : enum
  
  val r32i : enum
  
  val r32ui : enum
  
  val r8 : enum
  
  val r8i : enum
  
  val r8ui : enum
  
  val r8_snorm : enum
  
  val rasterizer_discard : enum
  
  val read_buffer_enum : enum
  
  val read_framebuffer : enum
  
  val read_framebuffer_binding : enum
  
  val read_only : enum
  
  val read_write : enum
  
  val red : enum
  
  val red_bits : enum
  
  val red_integer : enum
  
  val referenced_by_compute_shader : enum
  
  val referenced_by_fragment_shader : enum
  
  val referenced_by_geometry_shader : enum
  
  val referenced_by_tess_control_shader : enum
  
  val referenced_by_tess_evaluation_shader : enum
  
  val referenced_by_vertex_shader : enum
  
  val renderbuffer : enum
  
  val renderbuffer_alpha_size : enum
  
  val renderbuffer_binding : enum
  
  val renderbuffer_blue_size : enum
  
  val renderbuffer_depth_size : enum
  
  val renderbuffer_green_size : enum
  
  val renderbuffer_height : enum
  
  val renderbuffer_internal_format : enum
  
  val renderbuffer_red_size : enum
  
  val renderbuffer_samples : enum
  
  val renderbuffer_stencil_size : enum
  
  val renderbuffer_width : enum
  
  val renderer : enum
  
  val repeat : enum
  
  val replace : enum
  
  val reset_notification_strategy : enum
  
  val rg : enum
  
  val rg16f : enum
  
  val rg16i : enum
  
  val rg16ui : enum
  
  val rg32f : enum
  
  val rg32i : enum
  
  val rg32ui : enum
  
  val rg8 : enum
  
  val rg8i : enum
  
  val rg8ui : enum
  
  val rg8_snorm : enum
  
  val rgb : enum
  
  val rgb10_a2 : enum
  
  val rgb10_a2ui : enum
  
  val rgb16f : enum
  
  val rgb16i : enum
  
  val rgb16ui : enum
  
  val rgb32f : enum
  
  val rgb32i : enum
  
  val rgb32ui : enum
  
  val rgb565 : enum
  
  val rgb5_a1 : enum
  
  val rgb8 : enum
  
  val rgb8i : enum
  
  val rgb8ui : enum
  
  val rgb8_snorm : enum
  
  val rgb9_e5 : enum
  
  val rgba : enum
  
  val rgba16f : enum
  
  val rgba16i : enum
  
  val rgba16ui : enum
  
  val rgba32f : enum
  
  val rgba32i : enum
  
  val rgba32ui : enum
  
  val rgba4 : enum
  
  val rgba8 : enum
  
  val rgba8i : enum
  
  val rgba8ui : enum
  
  val rgba8_snorm : enum
  
  val rgba_integer : enum
  
  val rgb_integer : enum
  
  val rg_integer : enum
  
  val sampler : enum
  
  val sampler_2d : enum
  
  val sampler_2d_array : enum
  
  val sampler_2d_array_shadow : enum
  
  val sampler_2d_multisample : enum
  
  val sampler_2d_multisample_array : enum
  
  val sampler_2d_shadow : enum
  
  val sampler_3d : enum
  
  val sampler_binding : enum
  
  val sampler_buffer : enum
  
  val sampler_cube : enum
  
  val sampler_cube_map_array : enum
  
  val sampler_cube_map_array_shadow : enum
  
  val sampler_cube_shadow : enum
  
  val samples : enum
  
  val sample_alpha_to_coverage : enum
  
  val sample_buffers : enum
  
  val sample_coverage_enum : enum
  
  val sample_coverage_invert : enum
  
  val sample_coverage_value : enum
  
  val sample_mask : enum
  
  val sample_mask_value : enum
  
  val sample_position : enum
  
  val sample_shading : enum
  
  val scissor_box : enum
  
  val scissor_test : enum
  
  val screen : enum
  
  val separate_attribs : enum
  
  val shader : enum
  
  val shader_binary_formats : enum
  
  val shader_compiler : enum
  
  val shader_image_access_barrier_bit : enum
  
  val shader_source_length : enum
  
  val shader_storage_barrier_bit : enum
  
  val shader_storage_block : enum
  
  val shader_storage_buffer : enum
  
  val shader_storage_buffer_binding : enum
  
  val shader_storage_buffer_offset_alignment : enum
  
  val shader_storage_buffer_size : enum
  
  val shader_storage_buffer_start : enum
  
  val shader_type : enum
  
  val shading_language_version : enum
  
  val short : enum
  
  val signaled : enum
  
  val signed_normalized : enum
  
  val softlight : enum
  
  val src_alpha : enum
  
  val src_alpha_saturate : enum
  
  val src_color : enum
  
  val srgb : enum
  
  val srgb8 : enum
  
  val srgb8_alpha8 : enum
  
  val stack_overflow : enum
  
  val stack_underflow : enum
  
  val static_copy : enum
  
  val static_draw : enum
  
  val static_read : enum
  
  val stencil : enum
  
  val stencil_attachment : enum
  
  val stencil_back_fail : enum
  
  val stencil_back_func : enum
  
  val stencil_back_pass_depth_fail : enum
  
  val stencil_back_pass_depth_pass : enum
  
  val stencil_back_ref : enum
  
  val stencil_back_value_mask : enum
  
  val stencil_back_writemask : enum
  
  val stencil_bits : enum
  
  val stencil_buffer_bit : enum
  
  val stencil_clear_value : enum
  
  val stencil_fail : enum
  
  val stencil_func_enum : enum
  
  val stencil_index : enum
  
  val stencil_index8 : enum
  
  val stencil_pass_depth_fail : enum
  
  val stencil_pass_depth_pass : enum
  
  val stencil_ref : enum
  
  val stencil_test : enum
  
  val stencil_value_mask : enum
  
  val stencil_writemask : enum
  
  val stream_copy : enum
  
  val stream_draw : enum
  
  val stream_read : enum
  
  val subpixel_bits : enum
  
  val sync_condition : enum
  
  val sync_fence : enum
  
  val sync_flags : enum
  
  val sync_flush_commands_bit : enum
  
  val sync_gpu_commands_complete : enum
  
  val sync_status : enum
  
  val tess_control_output_vertices : enum
  
  val tess_control_shader : enum
  
  val tess_control_shader_bit : enum
  
  val tess_evaluation_shader : enum
  
  val tess_evaluation_shader_bit : enum
  
  val tess_gen_mode : enum
  
  val tess_gen_point_mode : enum
  
  val tess_gen_spacing : enum
  
  val tess_gen_vertex_order : enum
  
  val texture : enum
  
  val texture0 : enum
  
  val texture1 : enum
  
  val texture10 : enum
  
  val texture11 : enum
  
  val texture12 : enum
  
  val texture13 : enum
  
  val texture14 : enum
  
  val texture15 : enum
  
  val texture16 : enum
  
  val texture17 : enum
  
  val texture18 : enum
  
  val texture19 : enum
  
  val texture2 : enum
  
  val texture20 : enum
  
  val texture21 : enum
  
  val texture22 : enum
  
  val texture23 : enum
  
  val texture24 : enum
  
  val texture25 : enum
  
  val texture26 : enum
  
  val texture27 : enum
  
  val texture28 : enum
  
  val texture29 : enum
  
  val texture3 : enum
  
  val texture30 : enum
  
  val texture31 : enum
  
  val texture4 : enum
  
  val texture5 : enum
  
  val texture6 : enum
  
  val texture7 : enum
  
  val texture8 : enum
  
  val texture9 : enum
  
  val texture_2d : enum
  
  val texture_2d_array : enum
  
  val texture_2d_multisample : enum
  
  val texture_2d_multisample_array : enum
  
  val texture_3d : enum
  
  val texture_alpha_size : enum
  
  val texture_alpha_type : enum
  
  val texture_base_level : enum
  
  val texture_binding_2d : enum
  
  val texture_binding_2d_array : enum
  
  val texture_binding_2d_multisample : enum
  
  val texture_binding_2d_multisample_array : enum
  
  val texture_binding_3d : enum
  
  val texture_binding_buffer : enum
  
  val texture_binding_cube_map : enum
  
  val texture_binding_cube_map_array : enum
  
  val texture_blue_size : enum
  
  val texture_blue_type : enum
  
  val texture_border_color : enum
  
  val texture_buffer : enum
  
  val texture_buffer_binding : enum
  
  val texture_buffer_data_store_binding : enum
  
  val texture_buffer_offset : enum
  
  val texture_buffer_offset_alignment : enum
  
  val texture_buffer_size : enum
  
  val texture_compare_func : enum
  
  val texture_compare_mode : enum
  
  val texture_compressed : enum
  
  val texture_cube_map : enum
  
  val texture_cube_map_array : enum
  
  val texture_cube_map_negative_x : enum
  
  val texture_cube_map_negative_y : enum
  
  val texture_cube_map_negative_z : enum
  
  val texture_cube_map_positive_x : enum
  
  val texture_cube_map_positive_y : enum
  
  val texture_cube_map_positive_z : enum
  
  val texture_depth : enum
  
  val texture_depth_size : enum
  
  val texture_depth_type : enum
  
  val texture_fetch_barrier_bit : enum
  
  val texture_fixed_sample_locations : enum
  
  val texture_green_size : enum
  
  val texture_green_type : enum
  
  val texture_height : enum
  
  val texture_immutable_format : enum
  
  val texture_immutable_levels : enum
  
  val texture_internal_format : enum
  
  val texture_mag_filter : enum
  
  val texture_max_level : enum
  
  val texture_max_lod : enum
  
  val texture_min_filter : enum
  
  val texture_min_lod : enum
  
  val texture_red_size : enum
  
  val texture_red_type : enum
  
  val texture_samples : enum
  
  val texture_shared_size : enum
  
  val texture_stencil_size : enum
  
  val texture_swizzle_a : enum
  
  val texture_swizzle_b : enum
  
  val texture_swizzle_g : enum
  
  val texture_swizzle_r : enum
  
  val texture_update_barrier_bit : enum
  
  val texture_width : enum
  
  val texture_wrap_r : enum
  
  val texture_wrap_s : enum
  
  val texture_wrap_t : enum
  
  val timeout_expired : enum
  
  val timeout_ignored : int64
  
  val top_level_array_size : enum
  
  val top_level_array_stride : enum
  
  val transform_feedback : enum
  
  val transform_feedback_active : enum
  
  val transform_feedback_barrier_bit : enum
  
  val transform_feedback_binding : enum
  
  val transform_feedback_buffer : enum
  
  val transform_feedback_buffer_binding : enum
  
  val transform_feedback_buffer_mode : enum
  
  val transform_feedback_buffer_size : enum
  
  val transform_feedback_buffer_start : enum
  
  val transform_feedback_paused : enum
  
  val transform_feedback_primitives_written : enum
  
  val transform_feedback_varying : enum
  
  val transform_feedback_varyings_enum : enum
  
  val transform_feedback_varying_max_length : enum
  
  val triangles : enum
  
  val triangles_adjacency : enum
  
  val triangle_fan : enum
  
  val triangle_strip : enum
  
  val triangle_strip_adjacency : enum
  
  val true_ : enum
  
  val type_ : enum
  
  val undefined_vertex : enum
  
  val uniform : enum
  
  val uniform_array_stride : enum
  
  val uniform_barrier_bit : enum
  
  val uniform_block : enum
  
  val uniform_block_active_uniforms : enum
  
  val uniform_block_active_uniform_indices : enum
  
  val uniform_block_binding_enum : enum
  
  val uniform_block_data_size : enum
  
  val uniform_block_index : enum
  
  val uniform_block_name_length : enum
  
  val uniform_block_referenced_by_fragment_shader : enum
  
  val uniform_block_referenced_by_vertex_shader : enum
  
  val uniform_buffer : enum
  
  val uniform_buffer_binding : enum
  
  val uniform_buffer_offset_alignment : enum
  
  val uniform_buffer_size : enum
  
  val uniform_buffer_start : enum
  
  val uniform_is_row_major : enum
  
  val uniform_matrix_stride : enum
  
  val uniform_name_length : enum
  
  val uniform_offset : enum
  
  val uniform_size : enum
  
  val uniform_type : enum
  
  val unknown_context_reset : enum
  
  val unpack_alignment : enum
  
  val unpack_image_height : enum
  
  val unpack_row_length : enum
  
  val unpack_skip_images : enum
  
  val unpack_skip_pixels : enum
  
  val unpack_skip_rows : enum
  
  val unsignaled : enum
  
  val unsigned_byte : enum
  
  val unsigned_int : enum
  
  val unsigned_int_10f_11f_11f_rev : enum
  
  val unsigned_int_24_8 : enum
  
  val unsigned_int_2_10_10_10_rev : enum
  
  val unsigned_int_5_9_9_9_rev : enum
  
  val unsigned_int_atomic_counter : enum
  
  val unsigned_int_image_2d : enum
  
  val unsigned_int_image_2d_array : enum
  
  val unsigned_int_image_3d : enum
  
  val unsigned_int_image_buffer : enum
  
  val unsigned_int_image_cube : enum
  
  val unsigned_int_image_cube_map_array : enum
  
  val unsigned_int_sampler_2d : enum
  
  val unsigned_int_sampler_2d_array : enum
  
  val unsigned_int_sampler_2d_multisample : enum
  
  val unsigned_int_sampler_2d_multisample_array : enum
  
  val unsigned_int_sampler_3d : enum
  
  val unsigned_int_sampler_buffer : enum
  
  val unsigned_int_sampler_cube : enum
  
  val unsigned_int_sampler_cube_map_array : enum
  
  val unsigned_int_vec2 : enum
  
  val unsigned_int_vec3 : enum
  
  val unsigned_int_vec4 : enum
  
  val unsigned_normalized : enum
  
  val unsigned_short : enum
  
  val unsigned_short_4_4_4_4 : enum
  
  val unsigned_short_5_5_5_1 : enum
  
  val unsigned_short_5_6_5 : enum
  
  val validate_status : enum
  
  val vendor : enum
  
  val version : enum
  
  val vertex_array : enum
  
  val vertex_array_binding : enum
  
  val vertex_attrib_array_barrier_bit : enum
  
  val vertex_attrib_array_buffer_binding : enum
  
  val vertex_attrib_array_divisor : enum
  
  val vertex_attrib_array_enabled : enum
  
  val vertex_attrib_array_integer : enum
  
  val vertex_attrib_array_normalized : enum
  
  val vertex_attrib_array_pointer : enum
  
  val vertex_attrib_array_size : enum
  
  val vertex_attrib_array_stride : enum
  
  val vertex_attrib_array_type : enum
  
  val vertex_attrib_binding_enum : enum
  
  val vertex_attrib_relative_offset : enum
  
  val vertex_binding_buffer : enum
  
  val vertex_binding_divisor_enum : enum
  
  val vertex_binding_offset : enum
  
  val vertex_binding_stride : enum
  
  val vertex_shader : enum
  
  val vertex_shader_bit : enum
  
  val viewport_enum : enum
  
  val wait_failed : enum
  
  val write_only : enum
  
  val zero : enum
  
end

(** {1:conventions Conventions}

    To find the name of an OCaml function corresponding to a C
    function name, map the [gl] prefix to the module name
    {!Tgles3.Gl},
    add an underscore between each minuscule and majuscule and lower
    case the result. For example [glGetError] maps to
    {!Tgles3.Gl.get_error}

    To find the name of an OCaml value corresponding to a C enumerant name,
    map the [GL_] prefix to the module name {!Tgles3.Gl}
    and lower case the rest. For example [GL_COLOR_BUFFER_BIT] maps to
    {!Tgles3.Gl.color_buffer_bit}.

    The following exceptions occur:
    {ul
    {- A few enumerant names do clash with functions name. In that case we
       postfix the enumerant name with [_enum]. For example we have
       {!Tgles3.Gl.viewport} and {!Tgles3.Gl.viewport_enum}.}
    {- If applying the above procedures results in an identifier that
       doesn't start with a letter, prefix the identifier with a ['_'].}
    {- If applying the above procedures results in an identifier that
       is an OCaml keyword, suffix the identifier with a ['_'].}} *)