(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %NAME% %VERSION%
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated with:
   _build/support/apiquery.native -mli -api gl3.3 *)

(** OpenGL 3.x thin bindings.

    [Tgl3] can program core OpenGL 3.2 and 3.3 contexts.
    Consult the {{!conventions}binding conventions}.

    Open the module use it, this defines only the module [Gl]
    in your scope. To use in the toplevel with [findlib],
    just [#require "tgls.tgl3"], it automatically loads the library and
    opens the [Tgl3] module.

    {b References}
    {ul
    {- {{:http://www.opengl.org/registry}OpenGL 3.x}}}

    {e %%VERSION%% — OpenGL 3.x — {{:%%PKG_HOMEPAGE%% }homepage} } *)

(** {1 OpenGL 3.x} *)

(** OpenGL 3.x bindings.
    
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
  type int16 = int
  type sync
  type uint32_bigarray = (int32, Bigarray.int32_elt) bigarray
  type uint64 = int64
  type uint64_bigarray = (int64, Bigarray.int64_elt) bigarray
  type uint8 = int
  type debug_proc = enum -> enum -> int -> enum -> string -> unit
  
  (** {1:funs Functions} *)

  val active_texture : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glActiveTexture.xml}
      [glActiveTexture]} [texture] *)
  
  val attach_shader : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glAttachShader.xml}
      [glAttachShader]} [program shader] *)
  
  val begin_conditional_render : int -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBeginConditionalRender.xml}
      [glBeginConditionalRender]} [id mode] *)
  
  val begin_query : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBeginQuery.xml}
      [glBeginQuery]} [target id] *)
  
  val begin_transform_feedback : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBeginTransformFeedback.xml}
      [glBeginTransformFeedback]} [primitiveMode] *)
  
  val bind_attrib_location : int -> int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindAttribLocation.xml}
      [glBindAttribLocation]} [program index name] *)
  
  val bind_buffer : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindBuffer.xml}
      [glBindBuffer]} [target buffer] *)
  
  val bind_buffer_base : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindBufferBase.xml}
      [glBindBufferBase]} [target index buffer] *)
  
  val bind_buffer_range : enum -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindBufferRange.xml}
      [glBindBufferRange]} [target index buffer offset size] *)
  
  val bind_frag_data_location : int -> int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindFragDataLocation.xml}
      [glBindFragDataLocation]} [program color name] *)
  
  val bind_frag_data_location_indexed : int -> int -> int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindFragDataLocationIndexed.xml}
      [glBindFragDataLocationIndexed]} [program colorNumber index name] *)
  
  val bind_framebuffer : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindFramebuffer.xml}
      [glBindFramebuffer]} [target framebuffer] *)
  
  val bind_renderbuffer : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindRenderbuffer.xml}
      [glBindRenderbuffer]} [target renderbuffer] *)
  
  val bind_sampler : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindSampler.xml}
      [glBindSampler]} [unit sampler] *)
  
  val bind_texture : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindTexture.xml}
      [glBindTexture]} [target texture] *)
  
  val bind_vertex_array : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBindVertexArray.xml}
      [glBindVertexArray]} [array] *)
  
  val blend_color : float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBlendColor.xml}
      [glBlendColor]} [red green blue alpha] *)
  
  val blend_equation : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBlendEquation.xml}
      [glBlendEquation]} [mode] *)
  
  val blend_equation_separate : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBlendEquationSeparate.xml}
      [glBlendEquationSeparate]} [modeRGB modeAlpha] *)
  
  val blend_func : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBlendFunc.xml}
      [glBlendFunc]} [sfactor dfactor] *)
  
  val blend_func_separate : enum -> enum -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBlendFuncSeparate.xml}
      [glBlendFuncSeparate]} [sfactorRGB dfactorRGB sfactorAlpha
        dfactorAlpha] *)
  
  val blit_framebuffer : int -> int -> int -> int -> int -> int -> int ->
    int -> bitfield -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBlitFramebuffer.xml}
      [glBlitFramebuffer]} [srcX0 srcY0 srcX1 srcY1 dstX0 dstY0 dstX1 dstY1
        mask filter] *)
  
  val buffer_data : enum -> int -> ('a, 'b) bigarray option -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBufferData.xml}
      [glBufferData]} [target size data usage] *)
  
  val buffer_sub_data : enum -> int -> int -> ('a, 'b) bigarray option ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBufferSubData.xml}
      [glBufferSubData]} [target offset size data] *)
  
  val check_framebuffer_status : enum -> enum
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCheckFramebufferStatus.xml}
      [glCheckFramebufferStatus]} [target] *)
  
  val clamp_color : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClampColor.xml}
      [glClampColor]} [target clamp] *)
  
  val clear : bitfield -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClear.xml}[glClear]}
        [mask] *)
  
  val clear_bufferfi : enum -> int -> float -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClearBuffer.xml}
      [glClearBufferfi]} [buffer drawbuffer depth stencil] *)
  
  val clear_bufferfv : enum -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClearBuffer.xml}
      [glClearBufferfv]} [buffer drawbuffer value] *)
  
  val clear_bufferiv : enum -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClearBuffer.xml}
      [glClearBufferiv]} [buffer drawbuffer value] *)
  
  val clear_bufferuiv : enum -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClearBuffer.xml}
      [glClearBufferuiv]} [buffer drawbuffer value] *)
  
  val clear_color : float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClearColor.xml}
      [glClearColor]} [red green blue alpha] *)
  
  val clear_depth : float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClearDepth.xml}
      [glClearDepth]} [depth] *)
  
  val clear_stencil : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClearStencil.xml}
      [glClearStencil]} [s] *)
  
  val client_wait_sync : sync -> bitfield -> uint64 -> enum
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glClientWaitSync.xml}
      [glClientWaitSync]} [sync flags timeout] *)
  
  val color_mask : bool -> bool -> bool -> bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glColorMask.xml}
      [glColorMask]} [red green blue alpha] *)
  
  val color_maski : int -> bool -> bool -> bool -> bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glColorMask.xml}
      [glColorMaski]} [index r g b a] *)
  
  val compile_shader : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCompileShader.xml}
      [glCompileShader]} [shader] *)
  
  val compressed_tex_image1d : enum -> int -> enum -> int -> int -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage1D.xml}
      [glCompressedTexImage1D]} [target level internalformat width border
        imageSize data] *)
  
  val compressed_tex_image2d : enum -> int -> enum -> int -> int -> int ->
    int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage2D.xml}
      [glCompressedTexImage2D]} [target level internalformat width height
        border imageSize data] *)
  
  val compressed_tex_image3d : enum -> int -> enum -> int -> int -> int ->
    int -> int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage3D.xml}
      [glCompressedTexImage3D]} [target level internalformat width height
        depth border imageSize data] *)
  
  val compressed_tex_sub_image1d : enum -> int -> int -> int -> enum ->
    int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage1D.xml}
      [glCompressedTexSubImage1D]} [target level xoffset width format
        imageSize data] *)
  
  val compressed_tex_sub_image2d : enum -> int -> int -> int -> int -> int ->
    enum -> int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage2D.xml}
      [glCompressedTexSubImage2D]} [target level xoffset yoffset width height
        format imageSize data] *)
  
  val compressed_tex_sub_image3d : enum -> int -> int -> int -> int -> int ->
    int -> int -> enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage3D.xml}
      [glCompressedTexSubImage3D]} [target level xoffset yoffset zoffset
        width height depth format imageSize data] *)
  
  val copy_buffer_sub_data : enum -> enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCopyBufferSubData.xml}
      [glCopyBufferSubData]} [readTarget writeTarget readOffset writeOffset
        size] *)
  
  val copy_tex_image1d : enum -> int -> enum -> int -> int -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexImage1D.xml}
      [glCopyTexImage1D]} [target level internalformat x y width border] *)
  
  val copy_tex_image2d : enum -> int -> enum -> int -> int -> int -> int ->
    int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexImage2D.xml}
      [glCopyTexImage2D]} [target level internalformat x y width height
        border] *)
  
  val copy_tex_sub_image1d : enum -> int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage1D.xml}
      [glCopyTexSubImage1D]} [target level xoffset x y width] *)
  
  val copy_tex_sub_image2d : enum -> int -> int -> int -> int -> int ->
    int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage2D.xml}
      [glCopyTexSubImage2D]} [target level xoffset yoffset x y width height] *)
  
  val copy_tex_sub_image3d : enum -> int -> int -> int -> int -> int ->
    int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage3D.xml}
      [glCopyTexSubImage3D]} [target level xoffset yoffset zoffset x y width
        height] *)
  
  val create_program : unit -> int
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCreateProgram.xml}
      [glCreateProgram]} [()] *)
  
  val create_shader : enum -> int
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCreateShader.xml}
      [glCreateShader]} [type_] *)
  
  val cull_face : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glCullFace.xml}
      [glCullFace]} [mode] *)
  
  val delete_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteBuffers.xml}
      [glDeleteBuffers]} [n buffers] *)
  
  val delete_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteFramebuffers.xml}
      [glDeleteFramebuffers]} [n framebuffers] *)
  
  val delete_program : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteProgram.xml}
      [glDeleteProgram]} [program] *)
  
  val delete_queries : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteQueries.xml}
      [glDeleteQueries]} [n ids] *)
  
  val delete_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteRenderbuffers.xml}
      [glDeleteRenderbuffers]} [n renderbuffers] *)
  
  val delete_samplers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteSamplers.xml}
      [glDeleteSamplers]} [count samplers] *)
  
  val delete_shader : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteShader.xml}
      [glDeleteShader]} [shader] *)
  
  val delete_sync : sync -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteSync.xml}
      [glDeleteSync]} [sync] *)
  
  val delete_textures : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteTextures.xml}
      [glDeleteTextures]} [n textures] *)
  
  val delete_vertex_arrays : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDeleteVertexArrays.xml}
      [glDeleteVertexArrays]} [n arrays] *)
  
  val depth_func : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDepthFunc.xml}
      [glDepthFunc]} [func] *)
  
  val depth_mask : bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDepthMask.xml}
      [glDepthMask]} [flag] *)
  
  val depth_range : float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDepthRange.xml}
      [glDepthRange]} [near far] *)
  
  val detach_shader : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDetachShader.xml}
      [glDetachShader]} [program shader] *)
  
  val disable : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml}[glDisable]}
        [cap] *)
  
  val disable_vertex_attrib_array : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glEnableVertexAttribArray.xml}
      [glDisableVertexAttribArray]} [index] *)
  
  val disablei : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml}[glDisablei]}
        [target index] *)
  
  val draw_arrays : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawArrays.xml}
      [glDrawArrays]} [mode first count] *)
  
  val draw_arrays_instanced : enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawArraysInstanced.xml}
      [glDrawArraysInstanced]} [mode first count instancecount] *)
  
  val draw_buffer : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawBuffer.xml}
      [glDrawBuffer]} [buf] *)
  
  val draw_buffers : int -> enum_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawBuffers.xml}
      [glDrawBuffers]} [n bufs] *)
  
  val draw_elements : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawElements.xml}
      [glDrawElements]} [mode count type_ indices] *)
  
  val draw_elements_base_vertex : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsBaseVertex.xml}
      [glDrawElementsBaseVertex]} [mode count type_ indices basevertex] *)
  
  val draw_elements_instanced : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsInstanced.xml}
      [glDrawElementsInstanced]} [mode count type_ indices instancecount] *)
  
  val draw_elements_instanced_base_vertex : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsInstancedBaseVertex.xml}
      [glDrawElementsInstancedBaseVertex]} [mode count type_ indices
        instancecount basevertex] *)
  
  val draw_range_elements : enum -> int -> int -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElements.xml}
      [glDrawRangeElements]} [mode start end_ count type_ indices] *)
  
  val draw_range_elements_base_vertex : enum -> int -> int -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElementsBaseVertex.xml}
      [glDrawRangeElementsBaseVertex]} [mode start end_ count type_ indices
        basevertex] *)
  
  val enable : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml}[glEnable]}
        [cap] *)
  
  val enable_vertex_attrib_array : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glEnableVertexAttribArray.xml}
      [glEnableVertexAttribArray]} [index] *)
  
  val enablei : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml}[glEnablei]}
        [target index] *)
  
  val end_conditional_render : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBeginConditionalRender.xml}
      [glEndConditionalRender]} [()] *)
  
  val end_query : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBeginQuery.xml}
      [glEndQuery]} [target] *)
  
  val end_transform_feedback : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glBeginTransformFeedback.xml}
      [glEndTransformFeedback]} [()] *)
  
  val fence_sync : enum -> bitfield -> sync
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFenceSync.xml}
      [glFenceSync]} [condition flags] *)
  
  val finish : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFinish.xml}[glFinish]}
        [()] *)
  
  val flush : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFlush.xml}[glFlush]}
        [()] *)
  
  val flush_mapped_buffer_range : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFlushMappedBufferRange.xml}
      [glFlushMappedBufferRange]} [target offset length] *)
  
  val framebuffer_renderbuffer : enum -> enum -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferRenderbuffer.xml}
      [glFramebufferRenderbuffer]} [target attachment renderbuffertarget
        renderbuffer] *)
  
  val framebuffer_texture : enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml}
      [glFramebufferTexture]} [target attachment texture level] *)
  
  val framebuffer_texture1d : enum -> enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml}
      [glFramebufferTexture1D]} [target attachment textarget texture level] *)
  
  val framebuffer_texture2d : enum -> enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml}
      [glFramebufferTexture2D]} [target attachment textarget texture level] *)
  
  val framebuffer_texture3d : enum -> enum -> enum -> int -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml}
      [glFramebufferTexture3D]} [target attachment textarget texture level
        zoffset] *)
  
  val framebuffer_texture_layer : enum -> enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTextureLayer.xml}
      [glFramebufferTextureLayer]} [target attachment texture level layer] *)
  
  val front_face : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glFrontFace.xml}
      [glFrontFace]} [mode] *)
  
  val gen_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGenBuffers.xml}
      [glGenBuffers]} [n buffers] *)
  
  val gen_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGenFramebuffers.xml}
      [glGenFramebuffers]} [n framebuffers] *)
  
  val gen_queries : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGenQueries.xml}
      [glGenQueries]} [n ids] *)
  
  val gen_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGenRenderbuffers.xml}
      [glGenRenderbuffers]} [n renderbuffers] *)
  
  val gen_samplers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGenSamplers.xml}
      [glGenSamplers]} [count samplers] *)
  
  val gen_textures : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGenTextures.xml}
      [glGenTextures]} [n textures] *)
  
  val gen_vertex_arrays : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGenVertexArrays.xml}
      [glGenVertexArrays]} [n arrays] *)
  
  val generate_mipmap : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGenerateMipmap.xml}
      [glGenerateMipmap]} [target] *)
  
  val get_active_attrib : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveAttrib.xml}
      [glGetActiveAttrib]} [program index bufSize length size type_ name] *)
  
  val get_active_uniform : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniform.xml}
      [glGetActiveUniform]} [program index bufSize length size type_ name] *)
  
  val get_active_uniform_block_name : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformBlockName.xml}
      [glGetActiveUniformBlockName]} [program uniformBlockIndex bufSize
        length uniformBlockName] *)
  
  val get_active_uniform_blockiv : int -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformBlock.xml}
      [glGetActiveUniformBlockiv]} [program uniformBlockIndex pname params] *)
  
  val get_active_uniform_name : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformName.xml}
      [glGetActiveUniformName]} [program uniformIndex bufSize length
        uniformName] *)
  
  val get_active_uniformsiv : int -> int -> uint32_bigarray -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformsiv.xml}
      [glGetActiveUniformsiv]} [program uniformCount uniformIndices pname
        params] *)
  
  val get_attached_shaders : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetAttachedShaders.xml}
      [glGetAttachedShaders]} [program maxCount count shaders] *)
  
  val get_attrib_location : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetAttribLocation.xml}
      [glGetAttribLocation]} [program name] *)
  
  val get_booleani_v : enum -> int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml}
      [glGetBooleani_v]} [target index data] *)
  
  val get_booleanv : enum -> (int, Bigarray.int8_unsigned_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml}[glGetBooleanv]}
        [pname data] *)
  
  val get_buffer_parameteri64v : enum -> enum ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferParameter.xml}
      [glGetBufferParameteri64v]} [target pname params] *)
  
  val get_buffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferParameter.xml}
      [glGetBufferParameteriv]} [target pname params] *)
  
  val get_buffer_pointerv : enum -> enum ->
    (nativeint, Bigarray.nativeint_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferPointerv.xml}
      [glGetBufferPointerv]} [target pname params] *)
  
  val get_buffer_sub_data : enum -> int -> int -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferSubData.xml}
      [glGetBufferSubData]} [target offset size data] *)
  
  val get_compressed_tex_image : enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetCompressedTexImage.xml}
      [glGetCompressedTexImage]} [target level img] *)
  
  val get_doublev : enum -> (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml}[glGetDoublev]}
        [pname data] *)
  
  val get_error : unit -> enum
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetError.xml}
      [glGetError]} [()] *)
  
  val get_floatv : enum -> (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml}[glGetFloatv]}
        [pname data] *)
  
  val get_frag_data_index : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetFragDataIndex.xml}
      [glGetFragDataIndex]} [program name] *)
  
  val get_frag_data_location : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetFragDataLocation.xml}
      [glGetFragDataLocation]} [program name] *)
  
  val get_framebuffer_attachment_parameteriv : enum -> enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetFramebufferAttachmentParameter.xml}
      [glGetFramebufferAttachmentParameteriv]} [target attachment pname
        params] *)
  
  val get_integer64i_v : enum -> int ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml}
      [glGetInteger64i_v]} [target index data] *)
  
  val get_integer64v : enum -> (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml}
      [glGetInteger64v]} [pname data] *)
  
  val get_integeri_v : enum -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml}
      [glGetIntegeri_v]} [target index data] *)
  
  val get_integerv : enum -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml}[glGetIntegerv]}
        [pname data] *)
  
  val get_multisamplefv : enum -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetMultisample.xml}
      [glGetMultisamplefv]} [pname index val_] *)
  
  val get_program_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetProgramInfoLog.xml}
      [glGetProgramInfoLog]} [program bufSize length infoLog] *)
  
  val get_programiv : int -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetProgram.xml}
      [glGetProgramiv]} [program pname params] *)
  
  val get_query_objecti64v : int -> enum ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml}
      [glGetQueryObjecti64v]} [id pname params] *)
  
  val get_query_objectiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml}
      [glGetQueryObjectiv]} [id pname params] *)
  
  val get_query_objectui64v : int -> enum -> uint64_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml}
      [glGetQueryObjectui64v]} [id pname params] *)
  
  val get_query_objectuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml}
      [glGetQueryObjectuiv]} [id pname params] *)
  
  val get_queryiv : enum -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryiv.xml}
      [glGetQueryiv]} [target pname params] *)
  
  val get_renderbuffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetRenderbufferParameter.xml}
      [glGetRenderbufferParameteriv]} [target pname params] *)
  
  val get_sampler_parameter_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml}
      [glGetSamplerParameterIiv]} [sampler pname params] *)
  
  val get_sampler_parameter_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml}
      [glGetSamplerParameterIuiv]} [sampler pname params] *)
  
  val get_sampler_parameterfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml}
      [glGetSamplerParameterfv]} [sampler pname params] *)
  
  val get_sampler_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml}
      [glGetSamplerParameteriv]} [sampler pname params] *)
  
  val get_shader_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetShaderInfoLog.xml}
      [glGetShaderInfoLog]} [shader bufSize length infoLog] *)
  
  val get_shader_source : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetShaderSource.xml}
      [glGetShaderSource]} [shader bufSize length source] *)
  
  val get_shaderiv : int -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetShader.xml}
      [glGetShaderiv]} [shader pname params] *)
  
  val get_string : enum -> string option
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetString.xml}
      [glGetString]} [name] *)
  
  val get_stringi : enum -> int -> string option
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetString.xml}
      [glGetStringi]} [name index] *)
  
  val get_synciv : sync -> enum -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetSync.xml}
      [glGetSynciv]} [sync pname bufSize length values] *)
  
  val get_tex_image : enum -> int -> enum -> enum -> ('a, 'b) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetTexImage.xml}
      [glGetTexImage]} [target level format type_ pixels] *)
  
  val get_tex_level_parameterfv : enum -> int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetTexLevelParameter.xml}
      [glGetTexLevelParameterfv]} [target level pname params] *)
  
  val get_tex_level_parameteriv : enum -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetTexLevelParameter.xml}
      [glGetTexLevelParameteriv]} [target level pname params] *)
  
  val get_tex_parameter_iiv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml}
      [glGetTexParameterIiv]} [target pname params] *)
  
  val get_tex_parameter_iuiv : enum -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml}
      [glGetTexParameterIuiv]} [target pname params] *)
  
  val get_tex_parameterfv : enum -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml}
      [glGetTexParameterfv]} [target pname params] *)
  
  val get_tex_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml}
      [glGetTexParameteriv]} [target pname params] *)
  
  val get_transform_feedback_varying : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetTransformFeedbackVarying.xml}
      [glGetTransformFeedbackVarying]} [program index bufSize length size
        type_ name] *)
  
  val get_uniform_block_index : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformBlockIndex.xml}
      [glGetUniformBlockIndex]} [program uniformBlockName] *)
  
  val get_uniform_indices : int -> string list -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformIndices.xml}
      [glGetUniformIndices]} [program uniformNames uniformIndices] *)
  val get_uniform_location : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformLocation.xml}
      [glGetUniformLocation]} [program name] *)
  
  val get_uniformfv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml}
      [glGetUniformfv]} [program location params] *)
  
  val get_uniformiv : int -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml}
      [glGetUniformiv]} [program location params] *)
  
  val get_uniformuiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml}
      [glGetUniformuiv]} [program location params] *)
  
  val get_vertex_attrib_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml}
      [glGetVertexAttribIiv]} [index pname params] *)
  
  val get_vertex_attrib_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml}
      [glGetVertexAttribIuiv]} [index pname params] *)
  
  val get_vertex_attrib_pointerv : int -> enum ->
    (nativeint, Bigarray.nativeint_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttribPointerv.xml}
      [glGetVertexAttribPointerv]} [index pname pointer] *)
  
  val get_vertex_attribdv : int -> enum ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml}
      [glGetVertexAttribdv]} [index pname params] *)
  
  val get_vertex_attribfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml}
      [glGetVertexAttribfv]} [index pname params] *)
  
  val get_vertex_attribiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml}
      [glGetVertexAttribiv]} [index pname params] *)
  
  val hint : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glHint.xml}[glHint]}
        [target mode] *)
  
  val is_buffer : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsBuffer.xml}
      [glIsBuffer]} [buffer] *)
  
  val is_enabled : enum -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsEnabled.xml}
      [glIsEnabled]} [cap] *)
  
  val is_enabledi : enum -> int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsEnabled.xml}
      [glIsEnabledi]} [target index] *)
  
  val is_framebuffer : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsFramebuffer.xml}
      [glIsFramebuffer]} [framebuffer] *)
  
  val is_program : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsProgram.xml}
      [glIsProgram]} [program] *)
  
  val is_query : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsQuery.xml}[glIsQuery]}
        [id] *)
  
  val is_renderbuffer : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsRenderbuffer.xml}
      [glIsRenderbuffer]} [renderbuffer] *)
  
  val is_sampler : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsSampler.xml}
      [glIsSampler]} [sampler] *)
  
  val is_shader : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsShader.xml}
      [glIsShader]} [shader] *)
  
  val is_sync : sync -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsSync.xml}[glIsSync]}
        [sync] *)
  
  val is_texture : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsTexture.xml}
      [glIsTexture]} [texture] *)
  
  val is_vertex_array : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glIsVertexArray.xml}
      [glIsVertexArray]} [array] *)
  
  val line_width : float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glLineWidth.xml}
      [glLineWidth]} [width] *)
  
  val link_program : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glLinkProgram.xml}
      [glLinkProgram]} [program] *)
  
  val logic_op : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glLogicOp.xml}[glLogicOp]}
        [opcode] *)
  
  val map_buffer : enum -> int -> enum -> ('a, 'b) Bigarray.kind ->
    ('a, 'b) bigarray
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glMapBuffer.xml}
      [glMapBuffer]} [target length access kind]
  
      {b Note.} [length] is the length, in number of bigarray elements, of the
      mapped buffer.
  
      {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
      program termination may happen if you don't respect the access policy. *)
  
  val map_buffer_range : enum -> int -> int -> enum ->
    ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glMapBufferRange.xml}
      [glMapBufferRange]} [target offset length access kind]
  
      {b Note.} [length] is the length in number of bigarray elements of the
      mapped buffer. [offset] is in bytes.
  
      {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
      program termination may happen if you don't respect the access policy. *)
  
  val multi_draw_arrays : enum -> (int32, Bigarray.int32_elt) bigarray ->
    (int32, Bigarray.int32_elt) bigarray -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawArrays.xml}
      [glMultiDrawArrays]} [mode first count drawcount] *)
  
  val multi_draw_elements : enum -> (int32, Bigarray.int32_elt) bigarray ->
    enum -> ('a, 'b) bigarray -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawElements.xml}
      [glMultiDrawElements]} [mode count type_ indices drawcount]
      
      {b Note.} [indices] are byte offsets in the buffer bound on
      {!Gl.element_array_buffer}. Directly specifiying index arrays is
      unsupported. *)
  
  val multi_draw_elements_base_vertex : enum ->
    (int32, Bigarray.int32_elt) bigarray -> enum -> ('a, 'b) bigarray ->
    int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawElementsBaseVertex.xml}
      [glMultiDrawElementsBaseVertex]} [mode count type_ indices drawcount
        basevertex]
      
      {b Note.} [indices] are byte offsets in the buffer bound on
      {!Gl.element_array_buffer}. Directly specifiying index arrays is
      unsupported. *)
  
  val pixel_storef : enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPixelStore.xml}
      [glPixelStoref]} [pname param] *)
  
  val pixel_storei : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPixelStore.xml}
      [glPixelStorei]} [pname param] *)
  
  val point_parameterf : enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPointParameter.xml}
      [glPointParameterf]} [pname param] *)
  
  val point_parameterfv : enum -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPointParameter.xml}
      [glPointParameterfv]} [pname params] *)
  
  val point_parameteri : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPointParameter.xml}
      [glPointParameteri]} [pname param] *)
  
  val point_parameteriv : enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPointParameter.xml}
      [glPointParameteriv]} [pname params] *)
  
  val point_size : float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPointSize.xml}
      [glPointSize]} [size] *)
  
  val polygon_mode : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPolygonMode.xml}
      [glPolygonMode]} [face mode] *)
  
  val polygon_offset : float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPolygonOffset.xml}
      [glPolygonOffset]} [factor units] *)
  
  val primitive_restart_index : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glPrimitiveRestartIndex.xml}
      [glPrimitiveRestartIndex]} [index] *)
  
  val provoking_vertex : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glProvokingVertex.xml}
      [glProvokingVertex]} [mode] *)
  
  val query_counter : int -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glQueryCounter.xml}
      [glQueryCounter]} [id target] *)
  
  val read_buffer : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glReadBuffer.xml}
      [glReadBuffer]} [src] *)
  
  val read_pixels : int -> int -> int -> int -> enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glReadPixels.xml}
      [glReadPixels]} [x y width height format type_ pixels] *)
  
  val renderbuffer_storage : enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glRenderbufferStorage.xml}
      [glRenderbufferStorage]} [target internalformat width height] *)
  
  val renderbuffer_storage_multisample : enum -> int -> enum -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glRenderbufferStorageMultisample.xml}
      [glRenderbufferStorageMultisample]} [target samples internalformat
        width height] *)
  
  val sample_coverage : float -> bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glSampleCoverage.xml}
      [glSampleCoverage]} [value invert] *)
  
  val sample_maski : int -> bitfield -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glSampleMaski.xml}
      [glSampleMaski]} [maskNumber mask] *)
  
  val sampler_parameter_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml}
      [glSamplerParameterIiv]} [sampler pname param] *)
  
  val sampler_parameter_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml}
      [glSamplerParameterIuiv]} [sampler pname param] *)
  
  val sampler_parameterf : int -> enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml}
      [glSamplerParameterf]} [sampler pname param] *)
  
  val sampler_parameterfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml}
      [glSamplerParameterfv]} [sampler pname param] *)
  
  val sampler_parameteri : int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml}
      [glSamplerParameteri]} [sampler pname param] *)
  
  val sampler_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml}
      [glSamplerParameteriv]} [sampler pname param] *)
  
  val scissor : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glScissor.xml}[glScissor]}
        [x y width height] *)
  
  val shader_source : int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glShaderSource.xml}
      [glShaderSource]} [shader source] *)
  
  val stencil_func : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glStencilFunc.xml}
      [glStencilFunc]} [func ref mask] *)
  
  val stencil_func_separate : enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glStencilFuncSeparate.xml}
      [glStencilFuncSeparate]} [face func ref mask] *)
  
  val stencil_mask : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glStencilMask.xml}
      [glStencilMask]} [mask] *)
  
  val stencil_mask_separate : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glStencilMaskSeparate.xml}
      [glStencilMaskSeparate]} [face mask] *)
  
  val stencil_op : enum -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glStencilOp.xml}
      [glStencilOp]} [fail zfail zpass] *)
  
  val stencil_op_separate : enum -> enum -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glStencilOpSeparate.xml}
      [glStencilOpSeparate]} [face sfail dpfail dppass] *)
  
  val tex_buffer : enum -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexBuffer.xml}
      [glTexBuffer]} [target internalformat buffer] *)
  
  val tex_image1d : enum -> int -> int -> int -> int -> enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexImage1D.xml}
      [glTexImage1D]} [target level internalformat width border format type_
        pixels] *)
  
  val tex_image2d : enum -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexImage2D.xml}
      [glTexImage2D]} [target level internalformat width height border format
        type_ pixels] *)
  
  val tex_image2d_multisample : enum -> int -> enum -> int -> int -> bool ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexImage2DMultisample.xml}
      [glTexImage2DMultisample]} [target samples internalformat width height
        fixedsamplelocations] *)
  
  val tex_image3d : enum -> int -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexImage3D.xml}
      [glTexImage3D]} [target level internalformat width height depth border
        format type_ pixels] *)
  
  val tex_image3d_multisample : enum -> int -> enum -> int -> int -> int ->
    bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexImage3DMultisample.xml}
      [glTexImage3DMultisample]} [target samples internalformat width height
        depth fixedsamplelocations] *)
  
  val tex_parameter_iiv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml}
      [glTexParameterIiv]} [target pname params] *)
  
  val tex_parameter_iuiv : enum -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml}
      [glTexParameterIuiv]} [target pname params] *)
  
  val tex_parameterf : enum -> enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml}
      [glTexParameterf]} [target pname param] *)
  
  val tex_parameterfv : enum -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml}
      [glTexParameterfv]} [target pname params] *)
  
  val tex_parameteri : enum -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml}
      [glTexParameteri]} [target pname param] *)
  
  val tex_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml}
      [glTexParameteriv]} [target pname params] *)
  
  val tex_sub_image1d : enum -> int -> int -> int -> enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage1D.xml}
      [glTexSubImage1D]} [target level xoffset width format type_ pixels] *)
  
  val tex_sub_image2d : enum -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage2D.xml}
      [glTexSubImage2D]} [target level xoffset yoffset width height format
        type_ pixels] *)
  
  val tex_sub_image3d : enum -> int -> int -> int -> int -> int -> int ->
    int -> enum -> enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage3D.xml}
      [glTexSubImage3D]} [target level xoffset yoffset zoffset width height
        depth format type_ pixels] *)
  
  val transform_feedback_varyings : int -> string list -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glTransformFeedbackVaryings.xml}
      [glTransformFeedbackVaryings]} [program varyings bufferMode] *)
  val uniform1f : int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform1f]} [location v0] *)
  
  val uniform1fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform1fv]} [location count value] *)
  
  val uniform1i : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform1i]} [location v0] *)
  
  val uniform1iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform1iv]} [location count value] *)
  
  val uniform1ui : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform1ui]} [location v0] *)
  
  val uniform1uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform1uiv]} [location count value] *)
  
  val uniform2f : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform2f]} [location v0 v1] *)
  
  val uniform2fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform2fv]} [location count value] *)
  
  val uniform2i : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform2i]} [location v0 v1] *)
  
  val uniform2iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform2iv]} [location count value] *)
  
  val uniform2ui : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform2ui]} [location v0 v1] *)
  
  val uniform2uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform2uiv]} [location count value] *)
  
  val uniform3f : int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform3f]} [location v0 v1 v2] *)
  
  val uniform3fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform3fv]} [location count value] *)
  
  val uniform3i : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform3i]} [location v0 v1 v2] *)
  
  val uniform3iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform3iv]} [location count value] *)
  
  val uniform3ui : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform3ui]} [location v0 v1 v2] *)
  
  val uniform3uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform3uiv]} [location count value] *)
  
  val uniform4f : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform4f]} [location v0 v1 v2 v3] *)
  
  val uniform4fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform4fv]} [location count value] *)
  
  val uniform4i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform4i]} [location v0 v1 v2 v3] *)
  
  val uniform4iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform4iv]} [location count value] *)
  
  val uniform4ui : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform4ui]} [location v0 v1 v2 v3] *)
  
  val uniform4uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniform4uiv]} [location count value] *)
  
  val uniform_block_binding : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniformBlockBinding.xml}
      [glUniformBlockBinding]} [program uniformBlockIndex
        uniformBlockBinding] *)
  
  val uniform_matrix2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix2fv]} [location count transpose value] *)
  
  val uniform_matrix2x3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix2x3fv]} [location count transpose value] *)
  
  val uniform_matrix2x4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix2x4fv]} [location count transpose value] *)
  
  val uniform_matrix3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix3fv]} [location count transpose value] *)
  
  val uniform_matrix3x2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix3x2fv]} [location count transpose value] *)
  
  val uniform_matrix3x4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix3x4fv]} [location count transpose value] *)
  
  val uniform_matrix4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix4fv]} [location count transpose value] *)
  
  val uniform_matrix4x2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix4x2fv]} [location count transpose value] *)
  
  val uniform_matrix4x3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml}
      [glUniformMatrix4x3fv]} [location count transpose value] *)
  
  val unmap_buffer : enum -> bool
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUnmapBuffer.xml}
      [glUnmapBuffer]} [target] *)
  
  val use_program : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glUseProgram.xml}
      [glUseProgram]} [program] *)
  
  val validate_program : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glValidateProgram.xml}
      [glValidateProgram]} [program] *)
  
  val vertex_attrib1d : int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib1d]} [index x] *)
  
  val vertex_attrib1dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib1dv]} [index v] *)
  
  val vertex_attrib1f : int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib1f]} [index x] *)
  
  val vertex_attrib1fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib1fv]} [index v] *)
  
  val vertex_attrib1s : int -> int16 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib1s]} [index x] *)
  
  val vertex_attrib1sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib1sv]} [index v] *)
  
  val vertex_attrib2d : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib2d]} [index x y] *)
  
  val vertex_attrib2dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib2dv]} [index v] *)
  
  val vertex_attrib2f : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib2f]} [index x y] *)
  
  val vertex_attrib2fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib2fv]} [index v] *)
  
  val vertex_attrib2s : int -> int16 -> int16 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib2s]} [index x y] *)
  
  val vertex_attrib2sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib2sv]} [index v] *)
  
  val vertex_attrib3d : int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib3d]} [index x y z] *)
  
  val vertex_attrib3dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib3dv]} [index v] *)
  
  val vertex_attrib3f : int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib3f]} [index x y z] *)
  
  val vertex_attrib3fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib3fv]} [index v] *)
  
  val vertex_attrib3s : int -> int16 -> int16 -> int16 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib3s]} [index x y z] *)
  
  val vertex_attrib3sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib3sv]} [index v] *)
  
  val vertex_attrib4nbv : int -> (int, Bigarray.int8_signed_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4Nbv]} [index v] *)
  
  val vertex_attrib4niv : int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4Niv]} [index v] *)
  
  val vertex_attrib4nsv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4Nsv]} [index v] *)
  
  val vertex_attrib4nub : int -> uint8 -> uint8 -> uint8 -> uint8 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4Nub]} [index x y z w] *)
  
  val vertex_attrib4nubv : int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4Nubv]} [index v] *)
  
  val vertex_attrib4nuiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4Nuiv]} [index v] *)
  
  val vertex_attrib4nusv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4Nusv]} [index v] *)
  
  val vertex_attrib4bv : int -> (int, Bigarray.int8_signed_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4bv]} [index v] *)
  
  val vertex_attrib4d : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4d]} [index x y z w] *)
  
  val vertex_attrib4dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4dv]} [index v] *)
  
  val vertex_attrib4f : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4f]} [index x y z w] *)
  
  val vertex_attrib4fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4fv]} [index v] *)
  
  val vertex_attrib4iv : int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4iv]} [index v] *)
  
  val vertex_attrib4s : int -> int16 -> int16 -> int16 -> int16 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4s]} [index x y z w] *)
  
  val vertex_attrib4sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4sv]} [index v] *)
  
  val vertex_attrib4ubv : int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4ubv]} [index v] *)
  
  val vertex_attrib4uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4uiv]} [index v] *)
  
  val vertex_attrib4usv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4usv]} [index v] *)
  
  val vertex_attrib_divisor : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribDivisor.xml}
      [glVertexAttribDivisor]} [index divisor] *)
  
  val vertex_attrib_i1i : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI1i]} [index x] *)
  
  val vertex_attrib_i1iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI1iv]} [index v] *)
  
  val vertex_attrib_i1ui : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI1ui]} [index x] *)
  
  val vertex_attrib_i1uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI1uiv]} [index v] *)
  
  val vertex_attrib_i2i : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI2i]} [index x y] *)
  
  val vertex_attrib_i2iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI2iv]} [index v] *)
  
  val vertex_attrib_i2ui : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI2ui]} [index x y] *)
  
  val vertex_attrib_i2uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI2uiv]} [index v] *)
  
  val vertex_attrib_i3i : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI3i]} [index x y z] *)
  
  val vertex_attrib_i3iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI3iv]} [index v] *)
  
  val vertex_attrib_i3ui : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI3ui]} [index x y z] *)
  
  val vertex_attrib_i3uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI3uiv]} [index v] *)
  
  val vertex_attrib_i4bv : int -> (int, Bigarray.int8_signed_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI4bv]} [index v] *)
  
  val vertex_attrib_i4i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI4i]} [index x y z w] *)
  
  val vertex_attrib_i4iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI4iv]} [index v] *)
  
  val vertex_attrib_i4sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI4sv]} [index v] *)
  
  val vertex_attrib_i4ubv : int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI4ubv]} [index v] *)
  
  val vertex_attrib_i4ui : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI4ui]} [index x y z w] *)
  
  val vertex_attrib_i4uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI4uiv]} [index v] *)
  
  val vertex_attrib_i4usv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribI4usv]} [index v] *)
  
  val vertex_attrib_ipointer : int -> int -> enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribPointer.xml}
      [glVertexAttribIPointer]} [index size type_ stride pointer] *)
  
  val vertex_attrib_p1ui : int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribP1ui]} [index type_ normalized value] *)
  
  val vertex_attrib_p1uiv : int -> enum -> bool -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribP1uiv]} [index type_ normalized value] *)
  
  val vertex_attrib_p2ui : int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribP2ui]} [index type_ normalized value] *)
  
  val vertex_attrib_p2uiv : int -> enum -> bool -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribP2uiv]} [index type_ normalized value] *)
  
  val vertex_attrib_p3ui : int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribP3ui]} [index type_ normalized value] *)
  
  val vertex_attrib_p3uiv : int -> enum -> bool -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribP3uiv]} [index type_ normalized value] *)
  
  val vertex_attrib_p4ui : int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribP4ui]} [index type_ normalized value] *)
  
  val vertex_attrib_p4uiv : int -> enum -> bool -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml}
      [glVertexAttribP4uiv]} [index type_ normalized value] *)
  
  val vertex_attrib_pointer : int -> int -> enum -> bool -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribPointer.xml}
      [glVertexAttribPointer]} [index size type_ normalized stride pointer] *)
  
  val viewport : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glViewport.xml}
      [glViewport]} [x y width height] *)
  
  val wait_sync : sync -> bitfield -> uint64 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man3/xhtml/glWaitSync.xml}
      [glWaitSync]} [sync flags timeout] *)
  
  (** {1:enums Enums} *)

  val active_attributes : enum
  
  val active_attribute_max_length : enum
  
  val active_texture_enum : enum
  
  val active_uniforms : enum
  
  val active_uniform_blocks : enum
  
  val active_uniform_block_max_name_length : enum
  
  val active_uniform_max_length : enum
  
  val aliased_line_width_range : enum
  
  val alpha : enum
  
  val already_signaled : enum
  
  val always : enum
  
  val and_ : enum
  
  val and_inverted : enum
  
  val and_reverse : enum
  
  val any_samples_passed : enum
  
  val array_buffer : enum
  
  val array_buffer_binding : enum
  
  val attached_shaders : enum
  
  val back : enum
  
  val back_left : enum
  
  val back_right : enum
  
  val bgr : enum
  
  val bgra : enum
  
  val bgra_integer : enum
  
  val bgr_integer : enum
  
  val blend : enum
  
  val blend_dst : enum
  
  val blend_dst_alpha : enum
  
  val blend_dst_rgb : enum
  
  val blend_equation_alpha : enum
  
  val blend_equation_rgb : enum
  
  val blend_src : enum
  
  val blend_src_alpha : enum
  
  val blend_src_rgb : enum
  
  val blue : enum
  
  val blue_integer : enum
  
  val bool : enum
  
  val bool_vec2 : enum
  
  val bool_vec3 : enum
  
  val bool_vec4 : enum
  
  val buffer_access : enum
  
  val buffer_access_flags : enum
  
  val buffer_mapped : enum
  
  val buffer_map_length : enum
  
  val buffer_map_offset : enum
  
  val buffer_map_pointer : enum
  
  val buffer_size : enum
  
  val buffer_usage : enum
  
  val byte : enum
  
  val ccw : enum
  
  val clamp_read_color : enum
  
  val clamp_to_border : enum
  
  val clamp_to_edge : enum
  
  val clear_enum : enum
  
  val clip_distance0 : enum
  
  val clip_distance1 : enum
  
  val clip_distance2 : enum
  
  val clip_distance3 : enum
  
  val clip_distance4 : enum
  
  val clip_distance5 : enum
  
  val clip_distance6 : enum
  
  val clip_distance7 : enum
  
  val color : enum
  
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
  
  val color_logic_op : enum
  
  val color_writemask : enum
  
  val compare_ref_to_texture : enum
  
  val compile_status : enum
  
  val compressed_red : enum
  
  val compressed_red_rgtc1 : enum
  
  val compressed_rg : enum
  
  val compressed_rgb : enum
  
  val compressed_rgba : enum
  
  val compressed_rg_rgtc2 : enum
  
  val compressed_signed_red_rgtc1 : enum
  
  val compressed_signed_rg_rgtc2 : enum
  
  val compressed_srgb : enum
  
  val compressed_srgb_alpha : enum
  
  val compressed_texture_formats : enum
  
  val condition_satisfied : enum
  
  val constant_alpha : enum
  
  val constant_color : enum
  
  val context_compatibility_profile_bit : enum
  
  val context_core_profile_bit : enum
  
  val context_flags : enum
  
  val context_flag_forward_compatible_bit : enum
  
  val context_profile_mask : enum
  
  val copy : enum
  
  val copy_inverted : enum
  
  val copy_read_buffer : enum
  
  val copy_write_buffer : enum
  
  val cull_face_enum : enum
  
  val cull_face_mode : enum
  
  val current_program : enum
  
  val current_query : enum
  
  val current_vertex_attrib : enum
  
  val cw : enum
  
  val decr : enum
  
  val decr_wrap : enum
  
  val delete_status : enum
  
  val depth : enum
  
  val depth24_stencil8 : enum
  
  val depth32f_stencil8 : enum
  
  val depth_attachment : enum
  
  val depth_buffer_bit : enum
  
  val depth_clamp : enum
  
  val depth_clear_value : enum
  
  val depth_component : enum
  
  val depth_component16 : enum
  
  val depth_component24 : enum
  
  val depth_component32 : enum
  
  val depth_component32f : enum
  
  val depth_func_enum : enum
  
  val depth_range_enum : enum
  
  val depth_stencil : enum
  
  val depth_stencil_attachment : enum
  
  val depth_test : enum
  
  val depth_writemask : enum
  
  val dither : enum
  
  val dont_care : enum
  
  val double : enum
  
  val doublebuffer : enum
  
  val draw_buffer_enum : enum
  
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
  
  val dst_alpha : enum
  
  val dst_color : enum
  
  val dynamic_copy : enum
  
  val dynamic_draw : enum
  
  val dynamic_read : enum
  
  val element_array_buffer : enum
  
  val element_array_buffer_binding : enum
  
  val equal : enum
  
  val equiv : enum
  
  val extensions : enum
  
  val false_ : enum
  
  val fastest : enum
  
  val fill : enum
  
  val first_vertex_convention : enum
  
  val fixed_only : enum
  
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
  
  val fragment_shader : enum
  
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
  
  val framebuffer_binding : enum
  
  val framebuffer_complete : enum
  
  val framebuffer_default : enum
  
  val framebuffer_incomplete_attachment : enum
  
  val framebuffer_incomplete_draw_buffer : enum
  
  val framebuffer_incomplete_layer_targets : enum
  
  val framebuffer_incomplete_missing_attachment : enum
  
  val framebuffer_incomplete_multisample : enum
  
  val framebuffer_incomplete_read_buffer : enum
  
  val framebuffer_srgb : enum
  
  val framebuffer_undefined : enum
  
  val framebuffer_unsupported : enum
  
  val front : enum
  
  val front_and_back : enum
  
  val front_face_enum : enum
  
  val front_left : enum
  
  val front_right : enum
  
  val func_add : enum
  
  val func_reverse_subtract : enum
  
  val func_subtract : enum
  
  val geometry_input_type : enum
  
  val geometry_output_type : enum
  
  val geometry_shader : enum
  
  val geometry_vertices_out : enum
  
  val gequal : enum
  
  val greater : enum
  
  val green : enum
  
  val green_integer : enum
  
  val half_float : enum
  
  val incr : enum
  
  val incr_wrap : enum
  
  val info_log_length : enum
  
  val int : enum
  
  val interleaved_attribs : enum
  
  val int_2_10_10_10_rev : enum
  
  val int_sampler_1d : enum
  
  val int_sampler_1d_array : enum
  
  val int_sampler_2d : enum
  
  val int_sampler_2d_array : enum
  
  val int_sampler_2d_multisample : enum
  
  val int_sampler_2d_multisample_array : enum
  
  val int_sampler_2d_rect : enum
  
  val int_sampler_3d : enum
  
  val int_sampler_buffer : enum
  
  val int_sampler_cube : enum
  
  val int_vec2 : enum
  
  val int_vec3 : enum
  
  val int_vec4 : enum
  
  val invalid_enum : enum
  
  val invalid_framebuffer_operation : enum
  
  val invalid_index : int32
  
  val invalid_operation : enum
  
  val invalid_value : enum
  
  val invert : enum
  
  val keep : enum
  
  val last_vertex_convention : enum
  
  val left : enum
  
  val lequal : enum
  
  val less : enum
  
  val line : enum
  
  val linear : enum
  
  val linear_mipmap_linear : enum
  
  val linear_mipmap_nearest : enum
  
  val lines : enum
  
  val lines_adjacency : enum
  
  val line_loop : enum
  
  val line_smooth : enum
  
  val line_smooth_hint : enum
  
  val line_strip : enum
  
  val line_strip_adjacency : enum
  
  val line_width_enum : enum
  
  val line_width_granularity : enum
  
  val line_width_range : enum
  
  val link_status : enum
  
  val logic_op_mode : enum
  
  val lower_left : enum
  
  val major_version : enum
  
  val map_flush_explicit_bit : enum
  
  val map_invalidate_buffer_bit : enum
  
  val map_invalidate_range_bit : enum
  
  val map_read_bit : enum
  
  val map_unsynchronized_bit : enum
  
  val map_write_bit : enum
  
  val max : enum
  
  val max_3d_texture_size : enum
  
  val max_array_texture_layers : enum
  
  val max_clip_distances : enum
  
  val max_color_attachments : enum
  
  val max_color_texture_samples : enum
  
  val max_combined_fragment_uniform_components : enum
  
  val max_combined_geometry_uniform_components : enum
  
  val max_combined_texture_image_units : enum
  
  val max_combined_uniform_blocks : enum
  
  val max_combined_vertex_uniform_components : enum
  
  val max_cube_map_texture_size : enum
  
  val max_depth_texture_samples : enum
  
  val max_draw_buffers : enum
  
  val max_dual_source_draw_buffers : enum
  
  val max_elements_indices : enum
  
  val max_elements_vertices : enum
  
  val max_fragment_input_components : enum
  
  val max_fragment_uniform_blocks : enum
  
  val max_fragment_uniform_components : enum
  
  val max_geometry_input_components : enum
  
  val max_geometry_output_components : enum
  
  val max_geometry_output_vertices : enum
  
  val max_geometry_texture_image_units : enum
  
  val max_geometry_total_output_components : enum
  
  val max_geometry_uniform_blocks : enum
  
  val max_geometry_uniform_components : enum
  
  val max_integer_samples : enum
  
  val max_program_texel_offset : enum
  
  val max_rectangle_texture_size : enum
  
  val max_renderbuffer_size : enum
  
  val max_samples : enum
  
  val max_sample_mask_words : enum
  
  val max_server_wait_timeout : enum
  
  val max_texture_buffer_size : enum
  
  val max_texture_image_units : enum
  
  val max_texture_lod_bias : enum
  
  val max_texture_size : enum
  
  val max_transform_feedback_interleaved_components : enum
  
  val max_transform_feedback_separate_attribs : enum
  
  val max_transform_feedback_separate_components : enum
  
  val max_uniform_block_size : enum
  
  val max_uniform_buffer_bindings : enum
  
  val max_varying_components : enum
  
  val max_varying_floats : enum
  
  val max_vertex_attribs : enum
  
  val max_vertex_output_components : enum
  
  val max_vertex_texture_image_units : enum
  
  val max_vertex_uniform_blocks : enum
  
  val max_vertex_uniform_components : enum
  
  val max_viewport_dims : enum
  
  val min : enum
  
  val minor_version : enum
  
  val min_program_texel_offset : enum
  
  val mirrored_repeat : enum
  
  val multisample : enum
  
  val nand : enum
  
  val nearest : enum
  
  val nearest_mipmap_linear : enum
  
  val nearest_mipmap_nearest : enum
  
  val never : enum
  
  val nicest : enum
  
  val none : enum
  
  val noop : enum
  
  val nor : enum
  
  val notequal : enum
  
  val no_error : enum
  
  val num_compressed_texture_formats : enum
  
  val num_extensions : enum
  
  val object_type : enum
  
  val one : enum
  
  val one_minus_constant_alpha : enum
  
  val one_minus_constant_color : enum
  
  val one_minus_dst_alpha : enum
  
  val one_minus_dst_color : enum
  
  val one_minus_src1_alpha : enum
  
  val one_minus_src1_color : enum
  
  val one_minus_src_alpha : enum
  
  val one_minus_src_color : enum
  
  val or_ : enum
  
  val or_inverted : enum
  
  val or_reverse : enum
  
  val out_of_memory : enum
  
  val pack_alignment : enum
  
  val pack_image_height : enum
  
  val pack_lsb_first : enum
  
  val pack_row_length : enum
  
  val pack_skip_images : enum
  
  val pack_skip_pixels : enum
  
  val pack_skip_rows : enum
  
  val pack_swap_bytes : enum
  
  val pixel_pack_buffer : enum
  
  val pixel_pack_buffer_binding : enum
  
  val pixel_unpack_buffer : enum
  
  val pixel_unpack_buffer_binding : enum
  
  val point : enum
  
  val points : enum
  
  val point_fade_threshold_size : enum
  
  val point_size_enum : enum
  
  val point_size_granularity : enum
  
  val point_size_range : enum
  
  val point_sprite_coord_origin : enum
  
  val polygon_mode_enum : enum
  
  val polygon_offset_factor : enum
  
  val polygon_offset_fill : enum
  
  val polygon_offset_line : enum
  
  val polygon_offset_point : enum
  
  val polygon_offset_units : enum
  
  val polygon_smooth : enum
  
  val polygon_smooth_hint : enum
  
  val primitives_generated : enum
  
  val primitive_restart : enum
  
  val primitive_restart_index_enum : enum
  
  val program_point_size : enum
  
  val provoking_vertex_enum : enum
  
  val proxy_texture_1d : enum
  
  val proxy_texture_1d_array : enum
  
  val proxy_texture_2d : enum
  
  val proxy_texture_2d_array : enum
  
  val proxy_texture_2d_multisample : enum
  
  val proxy_texture_2d_multisample_array : enum
  
  val proxy_texture_3d : enum
  
  val proxy_texture_cube_map : enum
  
  val proxy_texture_rectangle : enum
  
  val quads_follow_provoking_vertex_convention : enum
  
  val query_by_region_no_wait : enum
  
  val query_by_region_wait : enum
  
  val query_counter_bits : enum
  
  val query_no_wait : enum
  
  val query_result : enum
  
  val query_result_available : enum
  
  val query_wait : enum
  
  val r11f_g11f_b10f : enum
  
  val r16 : enum
  
  val r16f : enum
  
  val r16i : enum
  
  val r16ui : enum
  
  val r16_snorm : enum
  
  val r32f : enum
  
  val r32i : enum
  
  val r32ui : enum
  
  val r3_g3_b2 : enum
  
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
  
  val red_integer : enum
  
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
  
  val rg : enum
  
  val rg16 : enum
  
  val rg16f : enum
  
  val rg16i : enum
  
  val rg16ui : enum
  
  val rg16_snorm : enum
  
  val rg32f : enum
  
  val rg32i : enum
  
  val rg32ui : enum
  
  val rg8 : enum
  
  val rg8i : enum
  
  val rg8ui : enum
  
  val rg8_snorm : enum
  
  val rgb : enum
  
  val rgb10 : enum
  
  val rgb10_a2 : enum
  
  val rgb10_a2ui : enum
  
  val rgb12 : enum
  
  val rgb16 : enum
  
  val rgb16f : enum
  
  val rgb16i : enum
  
  val rgb16ui : enum
  
  val rgb16_snorm : enum
  
  val rgb32f : enum
  
  val rgb32i : enum
  
  val rgb32ui : enum
  
  val rgb4 : enum
  
  val rgb5 : enum
  
  val rgb5_a1 : enum
  
  val rgb8 : enum
  
  val rgb8i : enum
  
  val rgb8ui : enum
  
  val rgb8_snorm : enum
  
  val rgb9_e5 : enum
  
  val rgba : enum
  
  val rgba12 : enum
  
  val rgba16 : enum
  
  val rgba16f : enum
  
  val rgba16i : enum
  
  val rgba16ui : enum
  
  val rgba16_snorm : enum
  
  val rgba2 : enum
  
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
  
  val right : enum
  
  val sampler_1d : enum
  
  val sampler_1d_array : enum
  
  val sampler_1d_array_shadow : enum
  
  val sampler_1d_shadow : enum
  
  val sampler_2d : enum
  
  val sampler_2d_array : enum
  
  val sampler_2d_array_shadow : enum
  
  val sampler_2d_multisample : enum
  
  val sampler_2d_multisample_array : enum
  
  val sampler_2d_rect : enum
  
  val sampler_2d_rect_shadow : enum
  
  val sampler_2d_shadow : enum
  
  val sampler_3d : enum
  
  val sampler_binding : enum
  
  val sampler_buffer : enum
  
  val sampler_cube : enum
  
  val sampler_cube_shadow : enum
  
  val samples : enum
  
  val samples_passed : enum
  
  val sample_alpha_to_coverage : enum
  
  val sample_alpha_to_one : enum
  
  val sample_buffers : enum
  
  val sample_coverage_enum : enum
  
  val sample_coverage_invert : enum
  
  val sample_coverage_value : enum
  
  val sample_mask : enum
  
  val sample_mask_value : enum
  
  val sample_position : enum
  
  val scissor_box : enum
  
  val scissor_test : enum
  
  val separate_attribs : enum
  
  val set : enum
  
  val shader_source_length : enum
  
  val shader_type : enum
  
  val shading_language_version : enum
  
  val short : enum
  
  val signaled : enum
  
  val signed_normalized : enum
  
  val smooth_line_width_granularity : enum
  
  val smooth_line_width_range : enum
  
  val smooth_point_size_granularity : enum
  
  val smooth_point_size_range : enum
  
  val src1_alpha : enum
  
  val src1_color : enum
  
  val src_alpha : enum
  
  val src_alpha_saturate : enum
  
  val src_color : enum
  
  val srgb : enum
  
  val srgb8 : enum
  
  val srgb8_alpha8 : enum
  
  val srgb_alpha : enum
  
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
  
  val stencil_buffer_bit : enum
  
  val stencil_clear_value : enum
  
  val stencil_fail : enum
  
  val stencil_func_enum : enum
  
  val stencil_index : enum
  
  val stencil_index1 : enum
  
  val stencil_index16 : enum
  
  val stencil_index4 : enum
  
  val stencil_index8 : enum
  
  val stencil_pass_depth_fail : enum
  
  val stencil_pass_depth_pass : enum
  
  val stencil_ref : enum
  
  val stencil_test : enum
  
  val stencil_value_mask : enum
  
  val stencil_writemask : enum
  
  val stereo : enum
  
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
  
  val texture_1d : enum
  
  val texture_1d_array : enum
  
  val texture_2d : enum
  
  val texture_2d_array : enum
  
  val texture_2d_multisample : enum
  
  val texture_2d_multisample_array : enum
  
  val texture_3d : enum
  
  val texture_alpha_size : enum
  
  val texture_alpha_type : enum
  
  val texture_base_level : enum
  
  val texture_binding_1d : enum
  
  val texture_binding_1d_array : enum
  
  val texture_binding_2d : enum
  
  val texture_binding_2d_array : enum
  
  val texture_binding_2d_multisample : enum
  
  val texture_binding_2d_multisample_array : enum
  
  val texture_binding_3d : enum
  
  val texture_binding_buffer : enum
  
  val texture_binding_cube_map : enum
  
  val texture_binding_rectangle : enum
  
  val texture_blue_size : enum
  
  val texture_blue_type : enum
  
  val texture_border_color : enum
  
  val texture_buffer : enum
  
  val texture_buffer_data_store_binding : enum
  
  val texture_compare_func : enum
  
  val texture_compare_mode : enum
  
  val texture_compressed : enum
  
  val texture_compressed_image_size : enum
  
  val texture_compression_hint : enum
  
  val texture_cube_map : enum
  
  val texture_cube_map_negative_x : enum
  
  val texture_cube_map_negative_y : enum
  
  val texture_cube_map_negative_z : enum
  
  val texture_cube_map_positive_x : enum
  
  val texture_cube_map_positive_y : enum
  
  val texture_cube_map_positive_z : enum
  
  val texture_cube_map_seamless : enum
  
  val texture_depth : enum
  
  val texture_depth_size : enum
  
  val texture_depth_type : enum
  
  val texture_fixed_sample_locations : enum
  
  val texture_green_size : enum
  
  val texture_green_type : enum
  
  val texture_height : enum
  
  val texture_internal_format : enum
  
  val texture_lod_bias : enum
  
  val texture_mag_filter : enum
  
  val texture_max_level : enum
  
  val texture_max_lod : enum
  
  val texture_min_filter : enum
  
  val texture_min_lod : enum
  
  val texture_rectangle : enum
  
  val texture_red_size : enum
  
  val texture_red_type : enum
  
  val texture_samples : enum
  
  val texture_shared_size : enum
  
  val texture_stencil_size : enum
  
  val texture_swizzle_a : enum
  
  val texture_swizzle_b : enum
  
  val texture_swizzle_g : enum
  
  val texture_swizzle_r : enum
  
  val texture_swizzle_rgba : enum
  
  val texture_width : enum
  
  val texture_wrap_r : enum
  
  val texture_wrap_s : enum
  
  val texture_wrap_t : enum
  
  val timeout_expired : enum
  
  val timeout_ignored : int64
  
  val timestamp : enum
  
  val time_elapsed : enum
  
  val transform_feedback_buffer : enum
  
  val transform_feedback_buffer_binding : enum
  
  val transform_feedback_buffer_mode : enum
  
  val transform_feedback_buffer_size : enum
  
  val transform_feedback_buffer_start : enum
  
  val transform_feedback_primitives_written : enum
  
  val transform_feedback_varyings_enum : enum
  
  val transform_feedback_varying_max_length : enum
  
  val triangles : enum
  
  val triangles_adjacency : enum
  
  val triangle_fan : enum
  
  val triangle_strip : enum
  
  val triangle_strip_adjacency : enum
  
  val true_ : enum
  
  val uniform_array_stride : enum
  
  val uniform_block_active_uniforms : enum
  
  val uniform_block_active_uniform_indices : enum
  
  val uniform_block_binding_enum : enum
  
  val uniform_block_data_size : enum
  
  val uniform_block_index : enum
  
  val uniform_block_name_length : enum
  
  val uniform_block_referenced_by_fragment_shader : enum
  
  val uniform_block_referenced_by_geometry_shader : enum
  
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
  
  val unpack_alignment : enum
  
  val unpack_image_height : enum
  
  val unpack_lsb_first : enum
  
  val unpack_row_length : enum
  
  val unpack_skip_images : enum
  
  val unpack_skip_pixels : enum
  
  val unpack_skip_rows : enum
  
  val unpack_swap_bytes : enum
  
  val unsignaled : enum
  
  val unsigned_byte : enum
  
  val unsigned_byte_2_3_3_rev : enum
  
  val unsigned_byte_3_3_2 : enum
  
  val unsigned_int : enum
  
  val unsigned_int_10f_11f_11f_rev : enum
  
  val unsigned_int_10_10_10_2 : enum
  
  val unsigned_int_24_8 : enum
  
  val unsigned_int_2_10_10_10_rev : enum
  
  val unsigned_int_5_9_9_9_rev : enum
  
  val unsigned_int_8_8_8_8 : enum
  
  val unsigned_int_8_8_8_8_rev : enum
  
  val unsigned_int_sampler_1d : enum
  
  val unsigned_int_sampler_1d_array : enum
  
  val unsigned_int_sampler_2d : enum
  
  val unsigned_int_sampler_2d_array : enum
  
  val unsigned_int_sampler_2d_multisample : enum
  
  val unsigned_int_sampler_2d_multisample_array : enum
  
  val unsigned_int_sampler_2d_rect : enum
  
  val unsigned_int_sampler_3d : enum
  
  val unsigned_int_sampler_buffer : enum
  
  val unsigned_int_sampler_cube : enum
  
  val unsigned_int_vec2 : enum
  
  val unsigned_int_vec3 : enum
  
  val unsigned_int_vec4 : enum
  
  val unsigned_normalized : enum
  
  val unsigned_short : enum
  
  val unsigned_short_1_5_5_5_rev : enum
  
  val unsigned_short_4_4_4_4 : enum
  
  val unsigned_short_4_4_4_4_rev : enum
  
  val unsigned_short_5_5_5_1 : enum
  
  val unsigned_short_5_6_5 : enum
  
  val unsigned_short_5_6_5_rev : enum
  
  val upper_left : enum
  
  val validate_status : enum
  
  val vendor : enum
  
  val version : enum
  
  val vertex_array_binding : enum
  
  val vertex_attrib_array_buffer_binding : enum
  
  val vertex_attrib_array_divisor : enum
  
  val vertex_attrib_array_enabled : enum
  
  val vertex_attrib_array_integer : enum
  
  val vertex_attrib_array_normalized : enum
  
  val vertex_attrib_array_pointer : enum
  
  val vertex_attrib_array_size : enum
  
  val vertex_attrib_array_stride : enum
  
  val vertex_attrib_array_type : enum
  
  val vertex_program_point_size : enum
  
  val vertex_shader : enum
  
  val viewport_enum : enum
  
  val wait_failed : enum
  
  val write_only : enum
  
  val xor : enum
  
  val zero : enum
  
end

(** {1:conventions Conventions}

    To find the name of an OCaml function corresponding to a C
    function name, map the [gl] prefix to the module name
    {!Tgl3.Gl},
    add an underscore between each minuscule and majuscule and lower
    case the result. For example [glGetError] maps to
    {!Tgl3.Gl.get_error}

    To find the name of an OCaml value corresponding to a C enumerant name,
    map the [GL_] prefix to the module name {!Tgl3.Gl}
    and lower case the rest. For example [GL_COLOR_BUFFER_BIT] maps to
    {!Tgl3.Gl.color_buffer_bit}.

    The following exceptions occur:
    {ul
    {- A few enumerant names do clash with functions name. In that case we
       postfix the enumerant name with [_enum]. For example we have
       {!Tgl3.Gl.viewport} and {!Tgl3.Gl.viewport_enum}.}
    {- If applying the above procedures results in an identifier that
       doesn't start with a letter, prefix the identifier with a ['_'].}
    {- If applying the above procedures results in an identifier that
       is an OCaml keyword, suffix the identifier with a ['_'].}} *)

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
