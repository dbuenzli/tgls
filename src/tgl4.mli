(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %NAME% %VERSION%
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated with:
   _build/support/apiquery.native -mli -api gl4.5 *)

(** OpenGL 4.x thin bindings.

    [Tgl4] can program core OpenGL 4.0 to 4.5 contexts.
    Consult the {{!conventions}binding conventions}.

    Open the module use it, this defines only the module [Gl]
    in your scope. To use in the toplevel with [findlib],
    just [#require "tgls.tgl4"], it automatically loads the library and
    opens the [Tgl4] module.

    {b References}
    {ul
    {- {{:http://www.opengl.org/registry}OpenGL 4.x}}}

    {e %%VERSION%% — OpenGL 4.x — {{:%%PKG_HOMEPAGE%% }homepage} } *)

(** {1 OpenGL 4.x} *)

(** OpenGL 4.x bindings.
    
    {{!types}Types}, {{!funs}functions} and {{!enums}enumerants}. *)
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

  val active_shader_program : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glActiveShaderProgram.xhtml}
      [glActiveShaderProgram]} [pipeline program] *)
  
  val active_texture : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glActiveTexture.xhtml}
      [glActiveTexture]} [texture] *)
  
  val attach_shader : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glAttachShader.xhtml}
      [glAttachShader]} [program shader] *)
  
  val begin_conditional_render : int -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBeginConditionalRender.xhtml}
      [glBeginConditionalRender]} [id mode] *)
  
  val begin_query : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBeginQuery.xhtml}
      [glBeginQuery]} [target id] *)
  
  val begin_query_indexed : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBeginQueryIndexed.xhtml}
      [glBeginQueryIndexed]} [target index id] *)
  
  val begin_transform_feedback : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBeginTransformFeedback.xhtml}
      [glBeginTransformFeedback]} [primitiveMode] *)
  
  val bind_attrib_location : int -> int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindAttribLocation.xhtml}
      [glBindAttribLocation]} [program index name] *)
  
  val bind_buffer : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindBuffer.xhtml}
      [glBindBuffer]} [target buffer] *)
  
  val bind_buffer_base : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindBufferBase.xhtml}
      [glBindBufferBase]} [target index buffer] *)
  
  val bind_buffer_range : enum -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindBufferRange.xhtml}
      [glBindBufferRange]} [target index buffer offset size] *)
  
  val bind_buffers_base : enum -> int -> int -> uint32_bigarray option ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindBuffersBase.xhtml}
      [glBindBuffersBase]} [target first count buffers] *)
  
  val bind_buffers_range : enum -> int -> int -> uint32_bigarray option ->
    (nativeint, Bigarray.nativeint_elt) bigarray option ->
    (nativeint, Bigarray.nativeint_elt) bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindBuffersRange.xhtml}
      [glBindBuffersRange]} [target first count buffers offsets sizes] *)
  
  val bind_frag_data_location : int -> int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindFragDataLocation.xhtml}
      [glBindFragDataLocation]} [program color name] *)
  
  val bind_frag_data_location_indexed : int -> int -> int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindFragDataLocationIndexed.xhtml}
      [glBindFragDataLocationIndexed]} [program colorNumber index name] *)
  
  val bind_framebuffer : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindFramebuffer.xhtml}
      [glBindFramebuffer]} [target framebuffer] *)
  
  val bind_image_texture : int -> int -> int -> bool -> int -> enum ->
    enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindImageTexture.xhtml}
      [glBindImageTexture]} [unit texture level layered layer access format] *)
  
  val bind_image_textures : int -> int -> uint32_bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindImageTextures.xhtml}
      [glBindImageTextures]} [first count textures] *)
  
  val bind_program_pipeline : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindProgramPipeline.xhtml}
      [glBindProgramPipeline]} [pipeline] *)
  
  val bind_renderbuffer : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindRenderbuffer.xhtml}
      [glBindRenderbuffer]} [target renderbuffer] *)
  
  val bind_sampler : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindSampler.xhtml}
      [glBindSampler]} [unit sampler] *)
  
  val bind_samplers : int -> int -> uint32_bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindSamplers.xhtml}
      [glBindSamplers]} [first count samplers] *)
  
  val bind_texture : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindTexture.xhtml}
      [glBindTexture]} [target texture] *)
  
  val bind_texture_unit : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindTextureUnit.xhtml}
      [glBindTextureUnit]} [unit texture] *)
  
  val bind_textures : int -> int -> uint32_bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindTextures.xhtml}
      [glBindTextures]} [first count textures] *)
  
  val bind_transform_feedback : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindTransformFeedback.xhtml}
      [glBindTransformFeedback]} [target id] *)
  
  val bind_vertex_array : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindVertexArray.xhtml}
      [glBindVertexArray]} [array] *)
  
  val bind_vertex_buffer : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffer.xhtml}
      [glBindVertexBuffer]} [bindingindex buffer offset stride] *)
  
  val bind_vertex_buffers : int -> int -> uint32_bigarray option ->
    (nativeint, Bigarray.nativeint_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffers.xhtml}
      [glBindVertexBuffers]} [first count buffers offsets strides] *)
  
  val blend_color : float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendColor.xhtml}
      [glBlendColor]} [red green blue alpha] *)
  
  val blend_equation : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendEquation.xhtml}
      [glBlendEquation]} [mode] *)
  
  val blend_equation_separate : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendEquationSeparate.xhtml}
      [glBlendEquationSeparate]} [modeRGB modeAlpha] *)
  
  val blend_equation_separatei : int -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendEquationSeparate.xhtml}
      [glBlendEquationSeparatei]} [buf modeRGB modeAlpha] *)
  
  val blend_equationi : int -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendEquation.xhtml}
      [glBlendEquationi]} [buf mode] *)
  
  val blend_func : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendFunc.xhtml}
      [glBlendFunc]} [sfactor dfactor] *)
  
  val blend_func_separate : enum -> enum -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendFuncSeparate.xhtml}
      [glBlendFuncSeparate]} [sfactorRGB dfactorRGB sfactorAlpha
        dfactorAlpha] *)
  
  val blend_func_separatei : int -> enum -> enum -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendFuncSeparate.xhtml}
      [glBlendFuncSeparatei]} [buf srcRGB dstRGB srcAlpha dstAlpha] *)
  
  val blend_funci : int -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlendFunc.xhtml}
      [glBlendFunci]} [buf src dst] *)
  
  val blit_framebuffer : int -> int -> int -> int -> int -> int -> int ->
    int -> bitfield -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlitFramebuffer.xhtml}
      [glBlitFramebuffer]} [srcX0 srcY0 srcX1 srcY1 dstX0 dstY0 dstX1 dstY1
        mask filter] *)
  
  val blit_named_framebuffer : int -> int -> int -> int -> int -> int ->
    int -> int -> int -> int -> bitfield -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBlitFramebuffer.xhtml}
      [glBlitNamedFramebuffer]} [readFramebuffer drawFramebuffer srcX0 srcY0
        srcX1 srcY1 dstX0 dstY0 dstX1 dstY1 mask filter] *)
  
  val buffer_data : enum -> int -> ('a, 'b) bigarray option -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBufferData.xhtml}
      [glBufferData]} [target size data usage] *)
  
  val buffer_storage : enum -> int -> ('a, 'b) bigarray option -> bitfield ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBufferStorage.xhtml}
      [glBufferStorage]} [target size data flags] *)
  
  val buffer_sub_data : enum -> int -> int -> ('a, 'b) bigarray option ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBufferSubData.xhtml}
      [glBufferSubData]} [target offset size data] *)
  
  val check_framebuffer_status : enum -> enum
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCheckFramebufferStatus.xhtml}
      [glCheckFramebufferStatus]} [target] *)
  
  val check_named_framebuffer_status : int -> enum -> enum
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCheckFramebufferStatus.xhtml}
      [glCheckNamedFramebufferStatus]} [framebuffer target] *)
  
  val clamp_color : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClampColor.xhtml}
      [glClampColor]} [target clamp] *)
  
  val clear : bitfield -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClear.xhtml}[glClear]}
        [mask] *)
  
  val clear_buffer_data : enum -> enum -> enum -> enum ->
    ('a, 'b) bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBufferData.xhtml}
      [glClearBufferData]} [target internalformat format type_ data] *)
  
  val clear_buffer_sub_data : enum -> enum -> int -> int -> enum -> enum ->
    ('a, 'b) bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBufferSubData.xhtml}
      [glClearBufferSubData]} [target internalformat offset size format type_
        data] *)
  
  val clear_bufferfi : enum -> int -> float -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml}
      [glClearBufferfi]} [buffer drawbuffer depth stencil] *)
  
  val clear_bufferfv : enum -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml}
      [glClearBufferfv]} [buffer drawbuffer value] *)
  
  val clear_bufferiv : enum -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml}
      [glClearBufferiv]} [buffer drawbuffer value] *)
  
  val clear_bufferuiv : enum -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml}
      [glClearBufferuiv]} [buffer drawbuffer value] *)
  
  val clear_color : float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearColor.xhtml}
      [glClearColor]} [red green blue alpha] *)
  
  val clear_depth : float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearDepth.xhtml}
      [glClearDepth]} [depth] *)
  
  val clear_depthf : float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearDepth.xhtml}
      [glClearDepthf]} [d] *)
  
  val clear_named_buffer_data : int -> enum -> enum -> enum ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBufferData.xhtml}
      [glClearNamedBufferData]} [buffer internalformat format type_ data] *)
  
  val clear_named_buffer_sub_data : int -> enum -> int -> int -> enum ->
    enum -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBufferSubData.xhtml}
      [glClearNamedBufferSubData]} [buffer internalformat offset size format
        type_ data] *)
  
  val clear_named_framebufferfi : int -> enum -> int -> float -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml}
      [glClearNamedFramebufferfi]} [framebuffer buffer drawbuffer depth
        stencil] *)
  
  val clear_named_framebufferfv : int -> enum -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml}
      [glClearNamedFramebufferfv]} [framebuffer buffer drawbuffer value] *)
  
  val clear_named_framebufferiv : int -> enum -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml}
      [glClearNamedFramebufferiv]} [framebuffer buffer drawbuffer value] *)
  
  val clear_named_framebufferuiv : int -> enum -> int -> uint32_bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml}
      [glClearNamedFramebufferuiv]} [framebuffer buffer drawbuffer value] *)
  
  val clear_stencil : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearStencil.xhtml}
      [glClearStencil]} [s] *)
  
  val clear_tex_image : int -> int -> enum -> enum ->
    ('a, 'b) bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearTexImage.xhtml}
      [glClearTexImage]} [texture level format type_ data] *)
  
  val clear_tex_sub_image : int -> int -> int -> int -> int -> int -> int ->
    int -> enum -> enum -> ('a, 'b) bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClearTexSubImage.xhtml}
      [glClearTexSubImage]} [texture level xoffset yoffset zoffset width
        height depth format type_ data] *)
  
  val client_wait_sync : sync -> bitfield -> uint64 -> enum
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClientWaitSync.xhtml}
      [glClientWaitSync]} [sync flags timeout] *)
  
  val clip_control : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glClipControl.xhtml}
      [glClipControl]} [origin depth] *)
  
  val color_mask : bool -> bool -> bool -> bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glColorMask.xhtml}
      [glColorMask]} [red green blue alpha] *)
  
  val color_maski : int -> bool -> bool -> bool -> bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glColorMask.xhtml}
      [glColorMaski]} [index r g b a] *)
  
  val compile_shader : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompileShader.xhtml}
      [glCompileShader]} [shader] *)
  
  val compressed_tex_image1d : enum -> int -> enum -> int -> int -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage1D.xhtml}
      [glCompressedTexImage1D]} [target level internalformat width border
        imageSize data] *)
  
  val compressed_tex_image2d : enum -> int -> enum -> int -> int -> int ->
    int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage2D.xhtml}
      [glCompressedTexImage2D]} [target level internalformat width height
        border imageSize data] *)
  
  val compressed_tex_image3d : enum -> int -> enum -> int -> int -> int ->
    int -> int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage3D.xhtml}
      [glCompressedTexImage3D]} [target level internalformat width height
        depth border imageSize data] *)
  
  val compressed_tex_sub_image1d : enum -> int -> int -> int -> enum ->
    int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage1D.xhtml}
      [glCompressedTexSubImage1D]} [target level xoffset width format
        imageSize data] *)
  
  val compressed_tex_sub_image2d : enum -> int -> int -> int -> int -> int ->
    enum -> int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage2D.xhtml}
      [glCompressedTexSubImage2D]} [target level xoffset yoffset width height
        format imageSize data] *)
  
  val compressed_tex_sub_image3d : enum -> int -> int -> int -> int -> int ->
    int -> int -> enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage3D.xhtml}
      [glCompressedTexSubImage3D]} [target level xoffset yoffset zoffset
        width height depth format imageSize data] *)
  
  val compressed_texture_sub_image1d : int -> int -> int -> int -> enum ->
    int -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage1D.xhtml}
      [glCompressedTextureSubImage1D]} [texture level xoffset width format
        imageSize data] *)
  
  val compressed_texture_sub_image2d : int -> int -> int -> int -> int ->
    int -> enum -> int -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage2D.xhtml}
      [glCompressedTextureSubImage2D]} [texture level xoffset yoffset width
        height format imageSize data] *)
  
  val compressed_texture_sub_image3d : int -> int -> int -> int -> int ->
    int -> int -> int -> enum -> int -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage3D.xhtml}
      [glCompressedTextureSubImage3D]} [texture level xoffset yoffset zoffset
        width height depth format imageSize data] *)
  
  val copy_buffer_sub_data : enum -> enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyBufferSubData.xhtml}
      [glCopyBufferSubData]} [readTarget writeTarget readOffset writeOffset
        size] *)
  
  val copy_image_sub_data : int -> enum -> int -> int -> int -> int -> int ->
    enum -> int -> int -> int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyImageSubData.xhtml}
      [glCopyImageSubData]} [srcName srcTarget srcLevel srcX srcY srcZ
        dstName dstTarget dstLevel dstX dstY dstZ srcWidth srcHeight
        srcDepth] *)
  
  val copy_named_buffer_sub_data : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyBufferSubData.xhtml}
      [glCopyNamedBufferSubData]} [readBuffer writeBuffer readOffset
        writeOffset size] *)
  
  val copy_tex_image1d : enum -> int -> enum -> int -> int -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyTexImage1D.xhtml}
      [glCopyTexImage1D]} [target level internalformat x y width border] *)
  
  val copy_tex_image2d : enum -> int -> enum -> int -> int -> int -> int ->
    int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyTexImage2D.xhtml}
      [glCopyTexImage2D]} [target level internalformat x y width height
        border] *)
  
  val copy_tex_sub_image1d : enum -> int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage1D.xhtml}
      [glCopyTexSubImage1D]} [target level xoffset x y width] *)
  
  val copy_tex_sub_image2d : enum -> int -> int -> int -> int -> int ->
    int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage2D.xhtml}
      [glCopyTexSubImage2D]} [target level xoffset yoffset x y width height] *)
  
  val copy_tex_sub_image3d : enum -> int -> int -> int -> int -> int ->
    int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage3D.xhtml}
      [glCopyTexSubImage3D]} [target level xoffset yoffset zoffset x y width
        height] *)
  
  val copy_texture_sub_image1d : int -> int -> int -> int -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage1D.xhtml}
      [glCopyTextureSubImage1D]} [texture level xoffset x y width] *)
  
  val copy_texture_sub_image2d : int -> int -> int -> int -> int -> int ->
    int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage2D.xhtml}
      [glCopyTextureSubImage2D]} [texture level xoffset yoffset x y width
        height] *)
  
  val copy_texture_sub_image3d : int -> int -> int -> int -> int -> int ->
    int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage3D.xhtml}
      [glCopyTextureSubImage3D]} [texture level xoffset yoffset zoffset x y
        width height] *)
  
  val create_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateBuffers.xhtml}
      [glCreateBuffers]} [n buffers] *)
  
  val create_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateFramebuffers.xhtml}
      [glCreateFramebuffers]} [n framebuffers] *)
  
  val create_program : unit -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateProgram.xhtml}
      [glCreateProgram]} [()] *)
  
  val create_program_pipelines : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateProgramPipelines.xhtml}
      [glCreateProgramPipelines]} [n pipelines] *)
  
  val create_queries : enum -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateQueries.xhtml}
      [glCreateQueries]} [target n ids] *)
  
  val create_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateRenderbuffers.xhtml}
      [glCreateRenderbuffers]} [n renderbuffers] *)
  
  val create_samplers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateSamplers.xhtml}
      [glCreateSamplers]} [n samplers] *)
  
  val create_shader : enum -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateShader.xhtml}
      [glCreateShader]} [type_] *)
  
  val create_shader_programv : enum -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateShaderProgram.xhtml}
      [glCreateShaderProgramv]} [type_ source] *)
  
  val create_textures : enum -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateTextures.xhtml}
      [glCreateTextures]} [target n textures] *)
  
  val create_transform_feedbacks : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateTransformFeedbacks.xhtml}
      [glCreateTransformFeedbacks]} [n ids] *)
  
  val create_vertex_arrays : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCreateVertexArrays.xhtml}
      [glCreateVertexArrays]} [n arrays] *)
  
  val cull_face : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glCullFace.xhtml}
      [glCullFace]} [mode] *)
  
  val debug_message_callback : debug_proc -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDebugMessageCallback.xhtml}
      [glDebugMessageCallback]} [f] *)
  
  val debug_message_control : enum -> enum -> enum -> int ->
    uint32_bigarray option -> bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDebugMessageControl.xhtml}
      [glDebugMessageControl]} [source type_ severity count ids enabled] *)
  
  val debug_message_insert : enum -> enum -> int -> enum -> int -> string ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDebugMessageInsert.xhtml}
      [glDebugMessageInsert]} [source type_ id severity length buf] *)
  
  val delete_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteBuffers.xhtml}
      [glDeleteBuffers]} [n buffers] *)
  
  val delete_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteFramebuffers.xhtml}
      [glDeleteFramebuffers]} [n framebuffers] *)
  
  val delete_program : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteProgram.xhtml}
      [glDeleteProgram]} [program] *)
  
  val delete_program_pipelines : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteProgramPipelines.xhtml}
      [glDeleteProgramPipelines]} [n pipelines] *)
  
  val delete_queries : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteQueries.xhtml}
      [glDeleteQueries]} [n ids] *)
  
  val delete_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteRenderbuffers.xhtml}
      [glDeleteRenderbuffers]} [n renderbuffers] *)
  
  val delete_samplers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteSamplers.xhtml}
      [glDeleteSamplers]} [count samplers] *)
  
  val delete_shader : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteShader.xhtml}
      [glDeleteShader]} [shader] *)
  
  val delete_sync : sync -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteSync.xhtml}
      [glDeleteSync]} [sync] *)
  
  val delete_textures : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteTextures.xhtml}
      [glDeleteTextures]} [n textures] *)
  
  val delete_transform_feedbacks : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteTransformFeedbacks.xhtml}
      [glDeleteTransformFeedbacks]} [n ids] *)
  
  val delete_vertex_arrays : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDeleteVertexArrays.xhtml}
      [glDeleteVertexArrays]} [n arrays] *)
  
  val depth_func : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDepthFunc.xhtml}
      [glDepthFunc]} [func] *)
  
  val depth_mask : bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDepthMask.xhtml}
      [glDepthMask]} [flag] *)
  
  val depth_range : float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDepthRange.xhtml}
      [glDepthRange]} [near far] *)
  
  val depth_range_arrayv : int -> int ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDepthRangeArray.xhtml}
      [glDepthRangeArrayv]} [first count v] *)
  
  val depth_range_indexed : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDepthRangeIndexed.xhtml}
      [glDepthRangeIndexed]} [index n f] *)
  
  val depth_rangef : float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDepthRange.xhtml}
      [glDepthRangef]} [n f] *)
  
  val detach_shader : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDetachShader.xhtml}
      [glDetachShader]} [program shader] *)
  
  val disable : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml}[glDisable]}
        [cap] *)
  
  val disable_vertex_array_attrib : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml}
      [glDisableVertexArrayAttrib]} [vaobj index] *)
  
  val disable_vertex_attrib_array : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml}
      [glDisableVertexAttribArray]} [index] *)
  
  val disablei : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml}
      [glDisablei]} [target index] *)
  
  val dispatch_compute : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDispatchCompute.xhtml}
      [glDispatchCompute]} [num_groups_x num_groups_y num_groups_z] *)
  
  val dispatch_compute_indirect : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDispatchComputeIndirect.xhtml}
      [glDispatchComputeIndirect]} [indirect] *)
  
  val draw_arrays : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawArrays.xhtml}
      [glDrawArrays]} [mode first count] *)
  
  val draw_arrays_indirect : enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawArraysIndirect.xhtml}
      [glDrawArraysIndirect]} [mode indirect] *)
  
  val draw_arrays_instanced : enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawArraysInstanced.xhtml}
      [glDrawArraysInstanced]} [mode first count instancecount] *)
  
  val draw_arrays_instanced_base_instance : enum -> int -> int -> int ->
    int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawArraysInstancedBaseInstance.xhtml}
      [glDrawArraysInstancedBaseInstance]} [mode first count instancecount
        baseinstance] *)
  
  val draw_buffer : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawBuffer.xhtml}
      [glDrawBuffer]} [buf] *)
  
  val draw_buffers : int -> enum_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawBuffers.xhtml}
      [glDrawBuffers]} [n bufs] *)
  
  val draw_elements : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawElements.xhtml}
      [glDrawElements]} [mode count type_ indices] *)
  
  val draw_elements_base_vertex : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawElementsBaseVertex.xhtml}
      [glDrawElementsBaseVertex]} [mode count type_ indices basevertex] *)
  
  val draw_elements_indirect : enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawElementsIndirect.xhtml}
      [glDrawElementsIndirect]} [mode type_ indirect] *)
  
  val draw_elements_instanced : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstanced.xhtml}
      [glDrawElementsInstanced]} [mode count type_ indices instancecount] *)
  
  val draw_elements_instanced_base_instance : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseInstance.xhtml}
      [glDrawElementsInstancedBaseInstance]} [mode count type_ indices
        instancecount baseinstance] *)
  
  val draw_elements_instanced_base_vertex : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseVertex.xhtml}
      [glDrawElementsInstancedBaseVertex]} [mode count type_ indices
        instancecount basevertex] *)
  
  val draw_elements_instanced_base_vertex_base_instance : enum -> int ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> int ->
    int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseVertexBaseInstance.xhtml}
      [glDrawElementsInstancedBaseVertexBaseInstance]} [mode count type_
        indices instancecount basevertex baseinstance] *)
  
  val draw_range_elements : enum -> int -> int -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawRangeElements.xhtml}
      [glDrawRangeElements]} [mode start end_ count type_ indices] *)
  
  val draw_range_elements_base_vertex : enum -> int -> int -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawRangeElementsBaseVertex.xhtml}
      [glDrawRangeElementsBaseVertex]} [mode start end_ count type_ indices
        basevertex] *)
  
  val draw_transform_feedback : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedback.xhtml}
      [glDrawTransformFeedback]} [mode id] *)
  
  val draw_transform_feedback_instanced : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackInstanced.xhtml}
      [glDrawTransformFeedbackInstanced]} [mode id instancecount] *)
  
  val draw_transform_feedback_stream : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackStream.xhtml}
      [glDrawTransformFeedbackStream]} [mode id stream] *)
  
  val draw_transform_feedback_stream_instanced : enum -> int -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackStreamInstanced.xhtml}
      [glDrawTransformFeedbackStreamInstanced]} [mode id stream
        instancecount] *)
  
  val enable : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml}[glEnable]}
        [cap] *)
  
  val enable_vertex_array_attrib : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml}
      [glEnableVertexArrayAttrib]} [vaobj index] *)
  
  val enable_vertex_attrib_array : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml}
      [glEnableVertexAttribArray]} [index] *)
  
  val enablei : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml}[glEnablei]}
        [target index] *)
  
  val end_conditional_render : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBeginConditionalRender.xhtml}
      [glEndConditionalRender]} [()] *)
  
  val end_query : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBeginQuery.xhtml}
      [glEndQuery]} [target] *)
  
  val end_query_indexed : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBeginQueryIndexed.xhtml}
      [glEndQueryIndexed]} [target index] *)
  
  val end_transform_feedback : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBeginTransformFeedback.xhtml}
      [glEndTransformFeedback]} [()] *)
  
  val fence_sync : enum -> bitfield -> sync
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFenceSync.xhtml}
      [glFenceSync]} [condition flags] *)
  
  val finish : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFinish.xhtml}[glFinish]}
        [()] *)
  
  val flush : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFlush.xhtml}[glFlush]}
        [()] *)
  
  val flush_mapped_buffer_range : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFlushMappedBufferRange.xhtml}
      [glFlushMappedBufferRange]} [target offset length] *)
  
  val flush_mapped_named_buffer_range : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFlushMappedBufferRange.xhtml}
      [glFlushMappedNamedBufferRange]} [buffer offset length] *)
  
  val framebuffer_parameteri : enum -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferParameteri.xhtml}
      [glFramebufferParameteri]} [target pname param] *)
  
  val framebuffer_renderbuffer : enum -> enum -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferRenderbuffer.xhtml}
      [glFramebufferRenderbuffer]} [target attachment renderbuffertarget
        renderbuffer] *)
  
  val framebuffer_texture : enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml}
      [glFramebufferTexture]} [target attachment texture level] *)
  
  val framebuffer_texture1d : enum -> enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml}
      [glFramebufferTexture1D]} [target attachment textarget texture level] *)
  
  val framebuffer_texture2d : enum -> enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml}
      [glFramebufferTexture2D]} [target attachment textarget texture level] *)
  
  val framebuffer_texture3d : enum -> enum -> enum -> int -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml}
      [glFramebufferTexture3D]} [target attachment textarget texture level
        zoffset] *)
  
  val framebuffer_texture_layer : enum -> enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferTextureLayer.xhtml}
      [glFramebufferTextureLayer]} [target attachment texture level layer] *)
  
  val front_face : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFrontFace.xhtml}
      [glFrontFace]} [mode] *)
  
  val gen_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenBuffers.xhtml}
      [glGenBuffers]} [n buffers] *)
  
  val gen_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenFramebuffers.xhtml}
      [glGenFramebuffers]} [n framebuffers] *)
  
  val gen_program_pipelines : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenProgramPipelines.xhtml}
      [glGenProgramPipelines]} [n pipelines] *)
  
  val gen_queries : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenQueries.xhtml}
      [glGenQueries]} [n ids] *)
  
  val gen_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenRenderbuffers.xhtml}
      [glGenRenderbuffers]} [n renderbuffers] *)
  
  val gen_samplers : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenSamplers.xhtml}
      [glGenSamplers]} [count samplers] *)
  
  val gen_textures : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenTextures.xhtml}
      [glGenTextures]} [n textures] *)
  
  val gen_transform_feedbacks : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenTransformFeedbacks.xhtml}
      [glGenTransformFeedbacks]} [n ids] *)
  
  val gen_vertex_arrays : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenVertexArrays.xhtml}
      [glGenVertexArrays]} [n arrays] *)
  
  val generate_mipmap : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenerateMipmap.xhtml}
      [glGenerateMipmap]} [target] *)
  
  val generate_texture_mipmap : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGenerateMipmap.xhtml}
      [glGenerateTextureMipmap]} [texture] *)
  
  val get_active_atomic_counter_bufferiv : int -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveAtomicCounterBufferiv.xhtml}
      [glGetActiveAtomicCounterBufferiv]} [program bufferIndex pname params] *)
  
  val get_active_attrib : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveAttrib.xhtml}
      [glGetActiveAttrib]} [program index bufSize length size type_ name] *)
  
  val get_active_subroutine_name : int -> enum -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineName.xhtml}
      [glGetActiveSubroutineName]} [program shadertype index bufsize length
        name] *)
  
  val get_active_subroutine_uniform_name : int -> enum -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineUniformName.xhtml}
      [glGetActiveSubroutineUniformName]} [program shadertype index bufsize
        length name] *)
  
  val get_active_subroutine_uniformiv : int -> enum -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineUniform.xhtml}
      [glGetActiveSubroutineUniformiv]} [program shadertype index pname
        values] *)
  
  val get_active_uniform : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveUniform.xhtml}
      [glGetActiveUniform]} [program index bufSize length size type_ name] *)
  
  val get_active_uniform_block_name : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformBlockName.xhtml}
      [glGetActiveUniformBlockName]} [program uniformBlockIndex bufSize
        length uniformBlockName] *)
  
  val get_active_uniform_blockiv : int -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformBlock.xhtml}
      [glGetActiveUniformBlockiv]} [program uniformBlockIndex pname params] *)
  
  val get_active_uniform_name : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformName.xhtml}
      [glGetActiveUniformName]} [program uniformIndex bufSize length
        uniformName] *)
  
  val get_active_uniformsiv : int -> int -> uint32_bigarray -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformsiv.xhtml}
      [glGetActiveUniformsiv]} [program uniformCount uniformIndices pname
        params] *)
  
  val get_attached_shaders : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetAttachedShaders.xhtml}
      [glGetAttachedShaders]} [program maxCount count shaders] *)
  
  val get_attrib_location : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetAttribLocation.xhtml}
      [glGetAttribLocation]} [program name] *)
  
  val get_booleani_v : enum -> int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}
      [glGetBooleani_v]} [target index data] *)
  
  val get_booleanv : enum -> (int, Bigarray.int8_unsigned_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}
      [glGetBooleanv]} [pname data] *)
  
  val get_buffer_parameteri64v : enum -> enum ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml}
      [glGetBufferParameteri64v]} [target pname params] *)
  
  val get_buffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml}
      [glGetBufferParameteriv]} [target pname params] *)
  
  val get_buffer_pointerv : enum -> enum ->
    (nativeint, Bigarray.nativeint_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetBufferPointerv.xhtml}
      [glGetBufferPointerv]} [target pname params] *)
  
  val get_buffer_sub_data : enum -> int -> int -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetBufferSubData.xhtml}
      [glGetBufferSubData]} [target offset size data] *)
  
  val get_compressed_tex_image : enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetCompressedTexImage.xhtml}
      [glGetCompressedTexImage]} [target level img] *)
  
  val get_compressed_texture_image : int -> int -> int ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetCompressedTexImage.xhtml}
      [glGetCompressedTextureImage]} [texture level bufSize pixels] *)
  
  val get_compressed_texture_sub_image : int -> int -> int -> int -> int ->
    int -> int -> int -> int -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetCompressedTextureSubImage.xhtml}
      [glGetCompressedTextureSubImage]} [texture level xoffset yoffset
        zoffset width height depth bufSize pixels] *)
  
  val get_debug_message_log : int -> int -> enum_bigarray -> enum_bigarray ->
    uint32_bigarray option -> enum_bigarray ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray option -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetDebugMessageLog.xhtml}
      [glGetDebugMessageLog]} [count bufSize sources types ids severities
        lengths messageLog] *)
  
  val get_doublei_v : enum -> int ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}
      [glGetDoublei_v]} [target index data] *)
  
  val get_doublev : enum -> (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}[glGetDoublev]}
        [pname data] *)
  
  val get_error : unit -> enum
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetError.xhtml}
      [glGetError]} [()] *)
  
  val get_floati_v : enum -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}
      [glGetFloati_v]} [target index data] *)
  
  val get_floatv : enum -> (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}[glGetFloatv]}
        [pname data] *)
  
  val get_frag_data_index : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetFragDataIndex.xhtml}
      [glGetFragDataIndex]} [program name] *)
  
  val get_frag_data_location : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetFragDataLocation.xhtml}
      [glGetFragDataLocation]} [program name] *)
  
  val get_framebuffer_attachment_parameteriv : enum -> enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetFramebufferAttachmentParameter.xhtml}
      [glGetFramebufferAttachmentParameteriv]} [target attachment pname
        params] *)
  
  val get_framebuffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetFramebufferParameter.xhtml}
      [glGetFramebufferParameteriv]} [target pname params] *)
  
  val get_graphics_reset_status : unit -> enum
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetGraphicsResetStatus.xhtml}
      [glGetGraphicsResetStatus]} [()] *)
  
  val get_integer64i_v : enum -> int ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}
      [glGetInteger64i_v]} [target index data] *)
  
  val get_integer64v : enum -> (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}
      [glGetInteger64v]} [pname data] *)
  
  val get_integeri_v : enum -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}
      [glGetIntegeri_v]} [target index data] *)
  
  val get_integerv : enum -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGet.xhtml}
      [glGetIntegerv]} [pname data] *)
  
  val get_internalformati64v : enum -> enum -> enum -> int ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetInternalformat.xhtml}
      [glGetInternalformati64v]} [target internalformat pname bufSize params] *)
  
  val get_internalformativ : enum -> enum -> enum -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetInternalformat.xhtml}
      [glGetInternalformativ]} [target internalformat pname bufSize params] *)
  
  val get_multisamplefv : enum -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetMultisample.xhtml}
      [glGetMultisamplefv]} [pname index val_] *)
  
  val get_named_buffer_parameteri64v : int -> enum ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml}
      [glGetNamedBufferParameteri64v]} [buffer pname params] *)
  
  val get_named_buffer_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml}
      [glGetNamedBufferParameteriv]} [buffer pname params] *)
  
  val get_named_buffer_pointerv : int -> enum ->
    (nativeint, Bigarray.nativeint_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetBufferPointerv.xhtml}
      [glGetNamedBufferPointerv]} [buffer pname params] *)
  
  val get_named_buffer_sub_data : int -> int -> int -> ('a, 'b) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetBufferSubData.xhtml}
      [glGetNamedBufferSubData]} [buffer offset size data] *)
  
  val get_named_framebuffer_attachment_parameteriv : int -> enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetFramebufferAttachmentParameter.xhtml}
      [glGetNamedFramebufferAttachmentParameteriv]} [framebuffer attachment
        pname params] *)
  
  val get_named_framebuffer_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetFramebufferParameter.xhtml}
      [glGetNamedFramebufferParameteriv]} [framebuffer pname param] *)
  
  val get_named_renderbuffer_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetRenderbufferParameter.xhtml}
      [glGetNamedRenderbufferParameteriv]} [renderbuffer pname params] *)
  
  val get_object_label : enum -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetObjectLabel.xhtml}
      [glGetObjectLabel]} [identifier name bufSize length label] *)
  
  val get_object_ptr_label : ('a, 'b) bigarray -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetObjectPtrLabel.xhtml}
      [glGetObjectPtrLabel]} [ptr bufSize length label] *)
  
  val get_pointerv : enum -> (nativeint, Bigarray.nativeint_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetPointerv.xhtml}
      [glGetPointerv]} [pname params] *)
  
  val get_program_binary : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option -> enum_bigarray ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramBinary.xhtml}
      [glGetProgramBinary]} [program bufSize length binaryFormat binary] *)
  
  val get_program_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramInfoLog.xhtml}
      [glGetProgramInfoLog]} [program bufSize length infoLog] *)
  
  val get_program_interfaceiv : int -> enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramInterface.xhtml}
      [glGetProgramInterfaceiv]} [program programInterface pname params] *)
  
  val get_program_pipeline_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramPipelineInfoLog.xhtml}
      [glGetProgramPipelineInfoLog]} [pipeline bufSize length infoLog] *)
  
  val get_program_pipelineiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramPipeline.xhtml}
      [glGetProgramPipelineiv]} [pipeline pname params] *)
  
  val get_program_resource_index : int -> enum -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceIndex.xhtml}
      [glGetProgramResourceIndex]} [program programInterface name] *)
  
  val get_program_resource_location : int -> enum -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceLocation.xhtml}
      [glGetProgramResourceLocation]} [program programInterface name] *)
  
  val get_program_resource_location_index : int -> enum -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceLocationIndex.xhtml}
      [glGetProgramResourceLocationIndex]} [program programInterface name] *)
  
  val get_program_resource_name : int -> enum -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceName.xhtml}
      [glGetProgramResourceName]} [program programInterface index bufSize
        length name] *)
  
  val get_program_resourceiv : int -> enum -> int -> int -> enum_bigarray ->
    int -> (int32, Bigarray.int32_elt) bigarray ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramResource.xhtml}
      [glGetProgramResourceiv]} [program programInterface index propCount
        props bufSize length params] *)
  
  val get_program_stageiv : int -> enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgramStage.xhtml}
      [glGetProgramStageiv]} [program shadertype pname values] *)
  
  val get_programiv : int -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetProgram.xhtml}
      [glGetProgramiv]} [program pname params] *)
  
  val get_query_buffer_objecti64v : int -> int -> enum -> int -> unit
  (** [glGetQueryBufferObjecti64v] [id buffer pname offset] *)
  
  val get_query_buffer_objectiv : int -> int -> enum -> int -> unit
  (** [glGetQueryBufferObjectiv] [id buffer pname offset] *)
  
  val get_query_buffer_objectui64v : int -> int -> enum -> int -> unit
  (** [glGetQueryBufferObjectui64v] [id buffer pname offset] *)
  
  val get_query_buffer_objectuiv : int -> int -> enum -> int -> unit
  (** [glGetQueryBufferObjectuiv] [id buffer pname offset] *)
  
  val get_query_indexediv : enum -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetQueryIndexed.xhtml}
      [glGetQueryIndexediv]} [target index pname params] *)
  
  val get_query_objecti64v : int -> enum ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml}
      [glGetQueryObjecti64v]} [id pname params] *)
  
  val get_query_objectiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml}
      [glGetQueryObjectiv]} [id pname params] *)
  
  val get_query_objectui64v : int -> enum -> uint64_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml}
      [glGetQueryObjectui64v]} [id pname params] *)
  
  val get_query_objectuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml}
      [glGetQueryObjectuiv]} [id pname params] *)
  
  val get_queryiv : enum -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetQueryiv.xhtml}
      [glGetQueryiv]} [target pname params] *)
  
  val get_renderbuffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetRenderbufferParameter.xhtml}
      [glGetRenderbufferParameteriv]} [target pname params] *)
  
  val get_sampler_parameter_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml}
      [glGetSamplerParameterIiv]} [sampler pname params] *)
  
  val get_sampler_parameter_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml}
      [glGetSamplerParameterIuiv]} [sampler pname params] *)
  
  val get_sampler_parameterfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml}
      [glGetSamplerParameterfv]} [sampler pname params] *)
  
  val get_sampler_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml}
      [glGetSamplerParameteriv]} [sampler pname params] *)
  
  val get_shader_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetShaderInfoLog.xhtml}
      [glGetShaderInfoLog]} [shader bufSize length infoLog] *)
  
  val get_shader_precision_format : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetShaderPrecisionFormat.xhtml}
      [glGetShaderPrecisionFormat]} [shadertype precisiontype range
        precision] *)
  
  val get_shader_source : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetShaderSource.xhtml}
      [glGetShaderSource]} [shader bufSize length source] *)
  
  val get_shaderiv : int -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetShader.xhtml}
      [glGetShaderiv]} [shader pname params] *)
  
  val get_string : enum -> string option
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetString.xhtml}
      [glGetString]} [name] *)
  
  val get_stringi : enum -> int -> string option
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetString.xhtml}
      [glGetStringi]} [name index] *)
  
  val get_subroutine_index : int -> enum -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetSubroutineIndex.xhtml}
      [glGetSubroutineIndex]} [program shadertype name] *)
  
  val get_subroutine_uniform_location : int -> enum -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetSubroutineUniformLocation.xhtml}
      [glGetSubroutineUniformLocation]} [program shadertype name] *)
  
  val get_synciv : sync -> enum -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetSync.xhtml}
      [glGetSynciv]} [sync pname bufSize length values] *)
  
  val get_tex_image : enum -> int -> enum -> enum -> ('a, 'b) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexImage.xhtml}
      [glGetTexImage]} [target level format type_ pixels] *)
  
  val get_tex_level_parameterfv : enum -> int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml}
      [glGetTexLevelParameterfv]} [target level pname params] *)
  
  val get_tex_level_parameteriv : enum -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml}
      [glGetTexLevelParameteriv]} [target level pname params] *)
  
  val get_tex_parameter_iiv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml}
      [glGetTexParameterIiv]} [target pname params] *)
  
  val get_tex_parameter_iuiv : enum -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml}
      [glGetTexParameterIuiv]} [target pname params] *)
  
  val get_tex_parameterfv : enum -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml}
      [glGetTexParameterfv]} [target pname params] *)
  
  val get_tex_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml}
      [glGetTexParameteriv]} [target pname params] *)
  
  val get_texture_image : int -> int -> enum -> enum -> int ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexImage.xhtml}
      [glGetTextureImage]} [texture level format type_ bufSize pixels] *)
  
  val get_texture_level_parameterfv : int -> int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml}
      [glGetTextureLevelParameterfv]} [texture level pname params] *)
  
  val get_texture_level_parameteriv : int -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml}
      [glGetTextureLevelParameteriv]} [texture level pname params] *)
  
  val get_texture_parameter_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml}
      [glGetTextureParameterIiv]} [texture pname params] *)
  
  val get_texture_parameter_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml}
      [glGetTextureParameterIuiv]} [texture pname params] *)
  
  val get_texture_parameterfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml}
      [glGetTextureParameterfv]} [texture pname params] *)
  
  val get_texture_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml}
      [glGetTextureParameteriv]} [texture pname params] *)
  
  val get_texture_sub_image : int -> int -> int -> int -> int -> int ->
    int -> int -> enum -> enum -> int -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTextureSubImage.xhtml}
      [glGetTextureSubImage]} [texture level xoffset yoffset zoffset width
        height depth format type_ bufSize pixels] *)
  
  val get_transform_feedback_varying : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedbackVarying.xhtml}
      [glGetTransformFeedbackVarying]} [program index bufSize length size
        type_ name] *)
  
  val get_transform_feedbacki64_v : int -> enum -> int ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml}
      [glGetTransformFeedbacki64_v]} [xfb pname index param] *)
  
  val get_transform_feedbacki_v : int -> enum -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml}
      [glGetTransformFeedbacki_v]} [xfb pname index param] *)
  
  val get_transform_feedbackiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml}
      [glGetTransformFeedbackiv]} [xfb pname param] *)
  
  val get_uniform_block_index : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniformBlockIndex.xhtml}
      [glGetUniformBlockIndex]} [program uniformBlockName] *)
  
  val get_uniform_indices : int -> string list -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniformIndices.xhtml}
      [glGetUniformIndices]} [program uniformNames uniformIndices] *)
  val get_uniform_location : int -> string -> int
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniformLocation.xhtml}
      [glGetUniformLocation]} [program name] *)
  
  val get_uniform_subroutineuiv : enum -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniformSubroutine.xhtml}
      [glGetUniformSubroutineuiv]} [shadertype location params] *)
  
  val get_uniformdv : int -> int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml}
      [glGetUniformdv]} [program location params] *)
  
  val get_uniformfv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml}
      [glGetUniformfv]} [program location params] *)
  
  val get_uniformiv : int -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml}
      [glGetUniformiv]} [program location params] *)
  
  val get_uniformuiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml}
      [glGetUniformuiv]} [program location params] *)
  
  val get_vertex_array_indexed64iv : int -> int -> enum ->
    (int64, Bigarray.int64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayIndexed.xhtml}
      [glGetVertexArrayIndexed64iv]} [vaobj index pname param] *)
  
  val get_vertex_array_indexediv : int -> int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayIndexed.xhtml}
      [glGetVertexArrayIndexediv]} [vaobj index pname param] *)
  
  val get_vertex_arrayiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayiv.xhtml}
      [glGetVertexArrayiv]} [vaobj pname param] *)
  
  val get_vertex_attrib_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribIiv]} [index pname params] *)
  
  val get_vertex_attrib_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribIuiv]} [index pname params] *)
  
  val get_vertex_attrib_ldv : int -> enum ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribLdv]} [index pname params] *)
  
  val get_vertex_attrib_pointerv : int -> enum ->
    (nativeint, Bigarray.nativeint_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexAttribPointerv.xhtml}
      [glGetVertexAttribPointerv]} [index pname pointer] *)
  
  val get_vertex_attribdv : int -> enum ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribdv]} [index pname params] *)
  
  val get_vertex_attribfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribfv]} [index pname params] *)
  
  val get_vertex_attribiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml}
      [glGetVertexAttribiv]} [index pname params] *)
  
  val getn_compressed_tex_image : enum -> int -> int -> ('a, 'b) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetCompressedTexImage.xhtml}
      [glGetnCompressedTexImage]} [target lod bufSize pixels] *)
  
  val getn_tex_image : enum -> int -> enum -> enum -> int ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetTexImage.xhtml}
      [glGetnTexImage]} [target level format type_ bufSize pixels] *)
  
  val getn_uniformdv : int -> int -> int ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml}
      [glGetnUniformdv]} [program location bufSize params] *)
  
  val getn_uniformfv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml}
      [glGetnUniformfv]} [program location bufSize params] *)
  
  val getn_uniformiv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml}
      [glGetnUniformiv]} [program location bufSize params] *)
  
  val getn_uniformuiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml}
      [glGetnUniformuiv]} [program location bufSize params] *)
  
  val hint : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glHint.xhtml}[glHint]}
        [target mode] *)
  
  val invalidate_buffer_data : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glInvalidateBufferData.xhtml}
      [glInvalidateBufferData]} [buffer] *)
  
  val invalidate_buffer_sub_data : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glInvalidateBufferSubData.xhtml}
      [glInvalidateBufferSubData]} [buffer offset length] *)
  
  val invalidate_framebuffer : enum -> int -> enum_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glInvalidateFramebuffer.xhtml}
      [glInvalidateFramebuffer]} [target numAttachments attachments] *)
  
  val invalidate_named_framebuffer_data : int -> int -> enum_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glInvalidateBufferData.xhtml}
      [glInvalidateNamedFramebufferData]} [framebuffer numAttachments
        attachments] *)
  
  val invalidate_named_framebuffer_sub_data : int -> int -> enum_bigarray ->
    int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glInvalidateBufferSubData.xhtml}
      [glInvalidateNamedFramebufferSubData]} [framebuffer numAttachments
        attachments x y width height] *)
  
  val invalidate_sub_framebuffer : enum -> int -> enum_bigarray -> int ->
    int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glInvalidateSubFramebuffer.xhtml}
      [glInvalidateSubFramebuffer]} [target numAttachments attachments x y
        width height] *)
  
  val invalidate_tex_image : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glInvalidateTexImage.xhtml}
      [glInvalidateTexImage]} [texture level] *)
  
  val invalidate_tex_sub_image : int -> int -> int -> int -> int -> int ->
    int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glInvalidateTexSubImage.xhtml}
      [glInvalidateTexSubImage]} [texture level xoffset yoffset zoffset width
        height depth] *)
  
  val is_buffer : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsBuffer.xhtml}
      [glIsBuffer]} [buffer] *)
  
  val is_enabled : enum -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsEnabled.xhtml}
      [glIsEnabled]} [cap] *)
  
  val is_enabledi : enum -> int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsEnabled.xhtml}
      [glIsEnabledi]} [target index] *)
  
  val is_framebuffer : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsFramebuffer.xhtml}
      [glIsFramebuffer]} [framebuffer] *)
  
  val is_program : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsProgram.xhtml}
      [glIsProgram]} [program] *)
  
  val is_program_pipeline : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsProgramPipeline.xhtml}
      [glIsProgramPipeline]} [pipeline] *)
  
  val is_query : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsQuery.xhtml}
      [glIsQuery]} [id] *)
  
  val is_renderbuffer : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsRenderbuffer.xhtml}
      [glIsRenderbuffer]} [renderbuffer] *)
  
  val is_sampler : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsSampler.xhtml}
      [glIsSampler]} [sampler] *)
  
  val is_shader : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsShader.xhtml}
      [glIsShader]} [shader] *)
  
  val is_sync : sync -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsSync.xhtml}[glIsSync]}
        [sync] *)
  
  val is_texture : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsTexture.xhtml}
      [glIsTexture]} [texture] *)
  
  val is_transform_feedback : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsTransformFeedback.xhtml}
      [glIsTransformFeedback]} [id] *)
  
  val is_vertex_array : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glIsVertexArray.xhtml}
      [glIsVertexArray]} [array] *)
  
  val line_width : float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glLineWidth.xhtml}
      [glLineWidth]} [width] *)
  
  val link_program : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glLinkProgram.xhtml}
      [glLinkProgram]} [program] *)
  
  val logic_op : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glLogicOp.xhtml}
      [glLogicOp]} [opcode] *)
  
  val map_buffer : enum -> int -> enum -> ('a, 'b) Bigarray.kind ->
    ('a, 'b) bigarray
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMapBuffer.xhtml}
      [glMapBuffer]} [target length access kind]
  
      {b Note.} [length] is the length, in number of bigarray elements, of the
      mapped buffer.
  
      {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
      program termination may happen if you don't respect the access policy. *)
  
  val map_buffer_range : enum -> int -> int -> enum ->
    ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMapBufferRange.xhtml}
      [glMapBufferRange]} [target offset length access kind]
  
      {b Note.} [length] is the length in number of bigarray elements of the
      mapped buffer. [offset] is in bytes.
  
      {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
      program termination may happen if you don't respect the access policy. *)
  
  val map_named_buffer : enum -> int -> enum -> ('a, 'b) Bigarray.kind ->
    ('a, 'b) bigarray
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMapBuffer.xhtml}
      [glMapNamedBuffer]} [buffer length access kind]
  
      {b Note.} [length] is the length, in number of bigarray elements, of the
      mapped buffer.
  
      {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
      program termination may happen if you don't respect the access policy. *)
  
  val map_named_buffer_range : enum -> int -> int -> enum ->
    ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMapBufferRange.xhtml}
      [glMapNamedBufferRange]} [buffer offset length access kind]
  
      {b Note.} [length] is the length in number of bigarray elements of the
      mapped buffer. [offset] is in bytes.
  
      {b Warning.} The bigarray becomes invalid once the buffer is unmapped and
      program termination may happen if you don't respect the access policy. *)
  
  val memory_barrier : bitfield -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMemoryBarrier.xhtml}
      [glMemoryBarrier]} [barriers]
      
      {b Warning.} On 32 bits platforms the constant
                {!all_barrier_bits} is represented by 0x7FFFFFFF
                instead of 0xFFFFFFFF, this may result in an OpenGL
                error (or not). *)
  
  val memory_barrier_by_region : bitfield -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMemoryBarrier.xhtml}
      [glMemoryBarrierByRegion]} [barriers]
      
      {b Warning.} On 32 bits platforms the constant
                {!all_barrier_bits} is represented by 0x7FFFFFFF
                instead of 0xFFFFFFFF, this may result in an OpenGL
                error (or not). *)
  
  val min_sample_shading : float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMinSampleShading.xhtml}
      [glMinSampleShading]} [value] *)
  
  val multi_draw_arrays : enum -> (int32, Bigarray.int32_elt) bigarray ->
    (int32, Bigarray.int32_elt) bigarray -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMultiDrawArrays.xhtml}
      [glMultiDrawArrays]} [mode first count drawcount] *)
  
  val multi_draw_arrays_indirect : enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMultiDrawArraysIndirect.xhtml}
      [glMultiDrawArraysIndirect]} [mode indirect drawcount stride] *)
  
  val multi_draw_elements : enum -> (int32, Bigarray.int32_elt) bigarray ->
    enum -> ('a, 'b) bigarray -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMultiDrawElements.xhtml}
      [glMultiDrawElements]} [mode count type_ indices drawcount]
      
      {b Note.} [indices] are byte offsets in the buffer bound on
      {!Gl.element_array_buffer}. Directly specifiying index arrays is
      unsupported. *)
  
  val multi_draw_elements_base_vertex : enum ->
    (int32, Bigarray.int32_elt) bigarray -> enum -> ('a, 'b) bigarray ->
    int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMultiDrawElementsBaseVertex.xhtml}
      [glMultiDrawElementsBaseVertex]} [mode count type_ indices drawcount
        basevertex]
      
      {b Note.} [indices] are byte offsets in the buffer bound on
      {!Gl.element_array_buffer}. Directly specifiying index arrays is
      unsupported. *)
  
  val multi_draw_elements_indirect : enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glMultiDrawElementsIndirect.xhtml}
      [glMultiDrawElementsIndirect]} [mode type_ indirect drawcount stride] *)
  
  val named_buffer_data : int -> int -> ('a, 'b) bigarray -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBufferData.xhtml}
      [glNamedBufferData]} [buffer size data usage] *)
  
  val named_buffer_storage : int -> int -> ('a, 'b) bigarray -> bitfield ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBufferStorage.xhtml}
      [glNamedBufferStorage]} [buffer size data flags] *)
  
  val named_buffer_sub_data : int -> int -> int -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBufferSubData.xhtml}
      [glNamedBufferSubData]} [buffer offset size data] *)
  
  val named_framebuffer_draw_buffer : int -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawBuffer.xhtml}
      [glNamedFramebufferDrawBuffer]} [framebuffer buf] *)
  
  val named_framebuffer_draw_buffers : int -> int -> enum_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glDrawBuffers.xhtml}
      [glNamedFramebufferDrawBuffers]} [framebuffer n bufs] *)
  
  val named_framebuffer_parameteri : int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferParameteri.xhtml}
      [glNamedFramebufferParameteri]} [framebuffer pname param] *)
  
  val named_framebuffer_read_buffer : int -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glReadBuffer.xhtml}
      [glNamedFramebufferReadBuffer]} [framebuffer src] *)
  
  val named_framebuffer_renderbuffer : int -> enum -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferRenderbuffer.xhtml}
      [glNamedFramebufferRenderbuffer]} [framebuffer attachment
        renderbuffertarget renderbuffer] *)
  
  val named_framebuffer_texture : int -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml}
      [glNamedFramebufferTexture]} [framebuffer attachment texture level] *)
  
  val named_framebuffer_texture_layer : int -> enum -> int -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glFramebufferTextureLayer.xhtml}
      [glNamedFramebufferTextureLayer]} [framebuffer attachment texture level
        layer] *)
  
  val named_renderbuffer_storage : int -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorage.xhtml}
      [glNamedRenderbufferStorage]} [renderbuffer internalformat width
        height] *)
  
  val named_renderbuffer_storage_multisample : int -> int -> enum -> int ->
    int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorageMultisample.xhtml}
      [glNamedRenderbufferStorageMultisample]} [renderbuffer samples
        internalformat width height] *)
  
  val object_label : enum -> int -> int -> string option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glObjectLabel.xhtml}
      [glObjectLabel]} [identifier name length label] *)
  
  val object_ptr_label : ('a, 'b) bigarray -> int -> string option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glObjectPtrLabel.xhtml}
      [glObjectPtrLabel]} [ptr length label] *)
  
  val patch_parameterfv : enum -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPatchParameter.xhtml}
      [glPatchParameterfv]} [pname values] *)
  
  val patch_parameteri : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPatchParameter.xhtml}
      [glPatchParameteri]} [pname value] *)
  
  val pause_transform_feedback : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPauseTransformFeedback.xhtml}
      [glPauseTransformFeedback]} [()] *)
  
  val pixel_storef : enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPixelStore.xhtml}
      [glPixelStoref]} [pname param] *)
  
  val pixel_storei : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPixelStore.xhtml}
      [glPixelStorei]} [pname param] *)
  
  val point_parameterf : enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPointParameter.xhtml}
      [glPointParameterf]} [pname param] *)
  
  val point_parameterfv : enum -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPointParameter.xhtml}
      [glPointParameterfv]} [pname params] *)
  
  val point_parameteri : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPointParameter.xhtml}
      [glPointParameteri]} [pname param] *)
  
  val point_parameteriv : enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPointParameter.xhtml}
      [glPointParameteriv]} [pname params] *)
  
  val point_size : float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPointSize.xhtml}
      [glPointSize]} [size] *)
  
  val polygon_mode : enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPolygonMode.xhtml}
      [glPolygonMode]} [face mode] *)
  
  val polygon_offset : float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPolygonOffset.xhtml}
      [glPolygonOffset]} [factor units] *)
  
  val pop_debug_group : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPopDebugGroup.xhtml}
      [glPopDebugGroup]} [()] *)
  
  val primitive_restart_index : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPrimitiveRestartIndex.xhtml}
      [glPrimitiveRestartIndex]} [index] *)
  
  val program_binary : int -> enum -> ('a, 'b) bigarray -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramBinary.xhtml}
      [glProgramBinary]} [program binaryFormat binary length] *)
  
  val program_parameteri : int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramParameter.xhtml}
      [glProgramParameteri]} [program pname value] *)
  
  val program_uniform1d : int -> int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform1d]} [program location v0] *)
  
  val program_uniform1dv : int -> int -> int ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform1dv]} [program location count value] *)
  
  val program_uniform1f : int -> int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform1f]} [program location v0] *)
  
  val program_uniform1fv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform1fv]} [program location count value] *)
  
  val program_uniform1i : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform1i]} [program location v0] *)
  
  val program_uniform1iv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform1iv]} [program location count value] *)
  
  val program_uniform1ui : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform1ui]} [program location v0] *)
  
  val program_uniform1uiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform1uiv]} [program location count value] *)
  
  val program_uniform2d : int -> int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform2d]} [program location v0 v1] *)
  
  val program_uniform2dv : int -> int -> int ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform2dv]} [program location count value] *)
  
  val program_uniform2f : int -> int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform2f]} [program location v0 v1] *)
  
  val program_uniform2fv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform2fv]} [program location count value] *)
  
  val program_uniform2i : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform2i]} [program location v0 v1] *)
  
  val program_uniform2iv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform2iv]} [program location count value] *)
  
  val program_uniform2ui : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform2ui]} [program location v0 v1] *)
  
  val program_uniform2uiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform2uiv]} [program location count value] *)
  
  val program_uniform3d : int -> int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform3d]} [program location v0 v1 v2] *)
  
  val program_uniform3dv : int -> int -> int ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform3dv]} [program location count value] *)
  
  val program_uniform3f : int -> int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform3f]} [program location v0 v1 v2] *)
  
  val program_uniform3fv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform3fv]} [program location count value] *)
  
  val program_uniform3i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform3i]} [program location v0 v1 v2] *)
  
  val program_uniform3iv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform3iv]} [program location count value] *)
  
  val program_uniform3ui : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform3ui]} [program location v0 v1 v2] *)
  
  val program_uniform3uiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform3uiv]} [program location count value] *)
  
  val program_uniform4d : int -> int -> float -> float -> float -> float ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform4d]} [program location v0 v1 v2 v3] *)
  
  val program_uniform4dv : int -> int -> int ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform4dv]} [program location count value] *)
  
  val program_uniform4f : int -> int -> float -> float -> float -> float ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform4f]} [program location v0 v1 v2 v3] *)
  
  val program_uniform4fv : int -> int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform4fv]} [program location count value] *)
  
  val program_uniform4i : int -> int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform4i]} [program location v0 v1 v2 v3] *)
  
  val program_uniform4iv : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform4iv]} [program location count value] *)
  
  val program_uniform4ui : int -> int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform4ui]} [program location v0 v1 v2 v3] *)
  
  val program_uniform4uiv : int -> int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniform4uiv]} [program location count value] *)
  
  val program_uniform_matrix2dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2dv]} [program location count transpose value] *)
  
  val program_uniform_matrix2fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2fv]} [program location count transpose value] *)
  
  val program_uniform_matrix2x3dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2x3dv]} [program location count transpose value] *)
  
  val program_uniform_matrix2x3fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2x3fv]} [program location count transpose value] *)
  
  val program_uniform_matrix2x4dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2x4dv]} [program location count transpose value] *)
  
  val program_uniform_matrix2x4fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix2x4fv]} [program location count transpose value] *)
  
  val program_uniform_matrix3dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3dv]} [program location count transpose value] *)
  
  val program_uniform_matrix3fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3fv]} [program location count transpose value] *)
  
  val program_uniform_matrix3x2dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3x2dv]} [program location count transpose value] *)
  
  val program_uniform_matrix3x2fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3x2fv]} [program location count transpose value] *)
  
  val program_uniform_matrix3x4dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3x4dv]} [program location count transpose value] *)
  
  val program_uniform_matrix3x4fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix3x4fv]} [program location count transpose value] *)
  
  val program_uniform_matrix4dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4dv]} [program location count transpose value] *)
  
  val program_uniform_matrix4fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4fv]} [program location count transpose value] *)
  
  val program_uniform_matrix4x2dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4x2dv]} [program location count transpose value] *)
  
  val program_uniform_matrix4x2fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4x2fv]} [program location count transpose value] *)
  
  val program_uniform_matrix4x3dv : int -> int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4x3dv]} [program location count transpose value] *)
  
  val program_uniform_matrix4x3fv : int -> int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml}
      [glProgramUniformMatrix4x3fv]} [program location count transpose value] *)
  
  val provoking_vertex : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glProvokingVertex.xhtml}
      [glProvokingVertex]} [mode] *)
  
  val push_debug_group : enum -> int -> int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glPushDebugGroup.xhtml}
      [glPushDebugGroup]} [source id length message] *)
  
  val query_counter : int -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glQueryCounter.xhtml}
      [glQueryCounter]} [id target] *)
  
  val read_buffer : enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glReadBuffer.xhtml}
      [glReadBuffer]} [src] *)
  
  val read_pixels : int -> int -> int -> int -> enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glReadPixels.xhtml}
      [glReadPixels]} [x y width height format type_ pixels] *)
  
  val readn_pixels : int -> int -> int -> int -> enum -> enum -> int ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glReadPixels.xhtml}
      [glReadnPixels]} [x y width height format type_ bufSize data] *)
  
  val release_shader_compiler : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glReleaseShaderCompiler.xhtml}
      [glReleaseShaderCompiler]} [()] *)
  
  val renderbuffer_storage : enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorage.xhtml}
      [glRenderbufferStorage]} [target internalformat width height] *)
  
  val renderbuffer_storage_multisample : enum -> int -> enum -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorageMultisample.xhtml}
      [glRenderbufferStorageMultisample]} [target samples internalformat
        width height] *)
  
  val resume_transform_feedback : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glResumeTransformFeedback.xhtml}
      [glResumeTransformFeedback]} [()] *)
  
  val sample_coverage : float -> bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glSampleCoverage.xhtml}
      [glSampleCoverage]} [value invert] *)
  
  val sample_maski : int -> bitfield -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glSampleMaski.xhtml}
      [glSampleMaski]} [maskNumber mask] *)
  
  val sampler_parameter_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml}
      [glSamplerParameterIiv]} [sampler pname param] *)
  
  val sampler_parameter_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml}
      [glSamplerParameterIuiv]} [sampler pname param] *)
  
  val sampler_parameterf : int -> enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml}
      [glSamplerParameterf]} [sampler pname param] *)
  
  val sampler_parameterfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml}
      [glSamplerParameterfv]} [sampler pname param] *)
  
  val sampler_parameteri : int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml}
      [glSamplerParameteri]} [sampler pname param] *)
  
  val sampler_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml}
      [glSamplerParameteriv]} [sampler pname param] *)
  
  val scissor : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glScissor.xhtml}
      [glScissor]} [x y width height] *)
  
  val scissor_arrayv : int -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glScissorArray.xhtml}
      [glScissorArrayv]} [first count v] *)
  
  val scissor_indexed : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glScissorIndexed.xhtml}
      [glScissorIndexed]} [index left bottom width height] *)
  
  val scissor_indexedv : int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glScissorIndexed.xhtml}
      [glScissorIndexedv]} [index v] *)
  
  val shader_binary : int -> uint32_bigarray -> enum -> ('a, 'b) bigarray ->
    int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glShaderBinary.xhtml}
      [glShaderBinary]} [count shaders binaryformat binary length] *)
  
  val shader_source : int -> string -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glShaderSource.xhtml}
      [glShaderSource]} [shader source] *)
  
  val shader_storage_block_binding : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glShaderStorageBlockBinding.xhtml}
      [glShaderStorageBlockBinding]} [program storageBlockIndex
        storageBlockBinding] *)
  
  val stencil_func : enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glStencilFunc.xhtml}
      [glStencilFunc]} [func ref mask] *)
  
  val stencil_func_separate : enum -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glStencilFuncSeparate.xhtml}
      [glStencilFuncSeparate]} [face func ref mask] *)
  
  val stencil_mask : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glStencilMask.xhtml}
      [glStencilMask]} [mask] *)
  
  val stencil_mask_separate : enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glStencilMaskSeparate.xhtml}
      [glStencilMaskSeparate]} [face mask] *)
  
  val stencil_op : enum -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glStencilOp.xhtml}
      [glStencilOp]} [fail zfail zpass] *)
  
  val stencil_op_separate : enum -> enum -> enum -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glStencilOpSeparate.xhtml}
      [glStencilOpSeparate]} [face sfail dpfail dppass] *)
  
  val tex_buffer : enum -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexBuffer.xhtml}
      [glTexBuffer]} [target internalformat buffer] *)
  
  val tex_buffer_range : enum -> enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexBufferRange.xhtml}
      [glTexBufferRange]} [target internalformat buffer offset size] *)
  
  val tex_image1d : enum -> int -> int -> int -> int -> enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexImage1D.xhtml}
      [glTexImage1D]} [target level internalformat width border format type_
        pixels] *)
  
  val tex_image2d : enum -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexImage2D.xhtml}
      [glTexImage2D]} [target level internalformat width height border format
        type_ pixels] *)
  
  val tex_image2d_multisample : enum -> int -> enum -> int -> int -> bool ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexImage2DMultisample.xhtml}
      [glTexImage2DMultisample]} [target samples internalformat width height
        fixedsamplelocations] *)
  
  val tex_image3d : enum -> int -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexImage3D.xhtml}
      [glTexImage3D]} [target level internalformat width height depth border
        format type_ pixels] *)
  
  val tex_image3d_multisample : enum -> int -> enum -> int -> int -> int ->
    bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexImage3DMultisample.xhtml}
      [glTexImage3DMultisample]} [target samples internalformat width height
        depth fixedsamplelocations] *)
  
  val tex_parameter_iiv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTexParameterIiv]} [target pname params] *)
  
  val tex_parameter_iuiv : enum -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTexParameterIuiv]} [target pname params] *)
  
  val tex_parameterf : enum -> enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTexParameterf]} [target pname param] *)
  
  val tex_parameterfv : enum -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTexParameterfv]} [target pname params] *)
  
  val tex_parameteri : enum -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTexParameteri]} [target pname param] *)
  
  val tex_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTexParameteriv]} [target pname params] *)
  
  val tex_storage1d : enum -> int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage1D.xhtml}
      [glTexStorage1D]} [target levels internalformat width] *)
  
  val tex_storage2d : enum -> int -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage2D.xhtml}
      [glTexStorage2D]} [target levels internalformat width height] *)
  
  val tex_storage2d_multisample : enum -> int -> enum -> int -> int ->
    bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage2DMultisample.xhtml}
      [glTexStorage2DMultisample]} [target samples internalformat width
        height fixedsamplelocations] *)
  
  val tex_storage3d : enum -> int -> enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage3D.xhtml}
      [glTexStorage3D]} [target levels internalformat width height depth] *)
  
  val tex_storage3d_multisample : enum -> int -> enum -> int -> int -> int ->
    bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage3DMultisample.xhtml}
      [glTexStorage3DMultisample]} [target samples internalformat width
        height depth fixedsamplelocations] *)
  
  val tex_sub_image1d : enum -> int -> int -> int -> enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexSubImage1D.xhtml}
      [glTexSubImage1D]} [target level xoffset width format type_ pixels] *)
  
  val tex_sub_image2d : enum -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexSubImage2D.xhtml}
      [glTexSubImage2D]} [target level xoffset yoffset width height format
        type_ pixels] *)
  
  val tex_sub_image3d : enum -> int -> int -> int -> int -> int -> int ->
    int -> enum -> enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexSubImage3D.xhtml}
      [glTexSubImage3D]} [target level xoffset yoffset zoffset width height
        depth format type_ pixels] *)
  
  val texture_barrier : unit -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTextureBarrier.xhtml}
      [glTextureBarrier]} [()] *)
  
  val texture_buffer : int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexBuffer.xhtml}
      [glTextureBuffer]} [texture internalformat buffer] *)
  
  val texture_buffer_range : int -> enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexBufferRange.xhtml}
      [glTextureBufferRange]} [texture internalformat buffer offset size] *)
  
  val texture_parameter_iiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTextureParameterIiv]} [texture pname params] *)
  
  val texture_parameter_iuiv : int -> enum -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTextureParameterIuiv]} [texture pname params] *)
  
  val texture_parameterf : int -> enum -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTextureParameterf]} [texture pname param] *)
  
  val texture_parameterfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTextureParameterfv]} [texture pname param] *)
  
  val texture_parameteri : int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTextureParameteri]} [texture pname param] *)
  
  val texture_parameteriv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml}
      [glTextureParameteriv]} [texture pname param] *)
  
  val texture_storage1d : int -> int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage1D.xhtml}
      [glTextureStorage1D]} [texture levels internalformat width] *)
  
  val texture_storage2d : int -> int -> enum -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage2D.xhtml}
      [glTextureStorage2D]} [texture levels internalformat width height] *)
  
  val texture_storage2d_multisample : int -> int -> enum -> int -> int ->
    bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage2DMultisample.xhtml}
      [glTextureStorage2DMultisample]} [texture samples internalformat width
        height fixedsamplelocations] *)
  
  val texture_storage3d : int -> int -> enum -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage3D.xhtml}
      [glTextureStorage3D]} [texture levels internalformat width height
        depth] *)
  
  val texture_storage3d_multisample : int -> int -> enum -> int -> int ->
    int -> bool -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexStorage3DMultisample.xhtml}
      [glTextureStorage3DMultisample]} [texture samples internalformat width
        height depth fixedsamplelocations] *)
  
  val texture_sub_image1d : int -> int -> int -> int -> enum -> enum ->
    ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexSubImage1D.xhtml}
      [glTextureSubImage1D]} [texture level xoffset width format type_
        pixels] *)
  
  val texture_sub_image2d : int -> int -> int -> int -> int -> int -> enum ->
    enum -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexSubImage2D.xhtml}
      [glTextureSubImage2D]} [texture level xoffset yoffset width height
        format type_ pixels] *)
  
  val texture_sub_image3d : int -> int -> int -> int -> int -> int -> int ->
    int -> enum -> enum -> ('a, 'b) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTexSubImage3D.xhtml}
      [glTextureSubImage3D]} [texture level xoffset yoffset zoffset width
        height depth format type_ pixels] *)
  
  val texture_view : int -> enum -> int -> enum -> int -> int -> int ->
    int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTextureView.xhtml}
      [glTextureView]} [texture target origtexture internalformat minlevel
        numlevels minlayer numlayers] *)
  
  val transform_feedback_buffer_base : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTransformFeedbackBufferBase.xhtml}
      [glTransformFeedbackBufferBase]} [xfb index buffer] *)
  
  val transform_feedback_buffer_range : int -> int -> int -> int -> int ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTransformFeedbackBufferRange.xhtml}
      [glTransformFeedbackBufferRange]} [xfb index buffer offset size] *)
  
  val transform_feedback_varyings : int -> string list -> enum -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glTransformFeedbackVaryings.xhtml}
      [glTransformFeedbackVaryings]} [program varyings bufferMode] *)
  val uniform1d : int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform1d]} [location x] *)
  
  val uniform1dv : int -> int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform1dv]} [location count value] *)
  
  val uniform1f : int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform1f]} [location v0] *)
  
  val uniform1fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform1fv]} [location count value] *)
  
  val uniform1i : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform1i]} [location v0] *)
  
  val uniform1iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform1iv]} [location count value] *)
  
  val uniform1ui : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform1ui]} [location v0] *)
  
  val uniform1uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform1uiv]} [location count value] *)
  
  val uniform2d : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform2d]} [location x y] *)
  
  val uniform2dv : int -> int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform2dv]} [location count value] *)
  
  val uniform2f : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform2f]} [location v0 v1] *)
  
  val uniform2fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform2fv]} [location count value] *)
  
  val uniform2i : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform2i]} [location v0 v1] *)
  
  val uniform2iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform2iv]} [location count value] *)
  
  val uniform2ui : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform2ui]} [location v0 v1] *)
  
  val uniform2uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform2uiv]} [location count value] *)
  
  val uniform3d : int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform3d]} [location x y z] *)
  
  val uniform3dv : int -> int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform3dv]} [location count value] *)
  
  val uniform3f : int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform3f]} [location v0 v1 v2] *)
  
  val uniform3fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform3fv]} [location count value] *)
  
  val uniform3i : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform3i]} [location v0 v1 v2] *)
  
  val uniform3iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform3iv]} [location count value] *)
  
  val uniform3ui : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform3ui]} [location v0 v1 v2] *)
  
  val uniform3uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform3uiv]} [location count value] *)
  
  val uniform4d : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform4d]} [location x y z w] *)
  
  val uniform4dv : int -> int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform4dv]} [location count value] *)
  
  val uniform4f : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform4f]} [location v0 v1 v2 v3] *)
  
  val uniform4fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform4fv]} [location count value] *)
  
  val uniform4i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform4i]} [location v0 v1 v2 v3] *)
  
  val uniform4iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform4iv]} [location count value] *)
  
  val uniform4ui : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform4ui]} [location v0 v1 v2 v3] *)
  
  val uniform4uiv : int -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniform4uiv]} [location count value] *)
  
  val uniform_block_binding : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniformBlockBinding.xhtml}
      [glUniformBlockBinding]} [program uniformBlockIndex
        uniformBlockBinding] *)
  
  val uniform_matrix2dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix2dv]} [location count transpose value] *)
  
  val uniform_matrix2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix2fv]} [location count transpose value] *)
  
  val uniform_matrix2x3dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix2x3dv]} [location count transpose value] *)
  
  val uniform_matrix2x3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix2x3fv]} [location count transpose value] *)
  
  val uniform_matrix2x4dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix2x4dv]} [location count transpose value] *)
  
  val uniform_matrix2x4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix2x4fv]} [location count transpose value] *)
  
  val uniform_matrix3dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix3dv]} [location count transpose value] *)
  
  val uniform_matrix3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix3fv]} [location count transpose value] *)
  
  val uniform_matrix3x2dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix3x2dv]} [location count transpose value] *)
  
  val uniform_matrix3x2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix3x2fv]} [location count transpose value] *)
  
  val uniform_matrix3x4dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix3x4dv]} [location count transpose value] *)
  
  val uniform_matrix3x4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix3x4fv]} [location count transpose value] *)
  
  val uniform_matrix4dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix4dv]} [location count transpose value] *)
  
  val uniform_matrix4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix4fv]} [location count transpose value] *)
  
  val uniform_matrix4x2dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix4x2dv]} [location count transpose value] *)
  
  val uniform_matrix4x2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix4x2fv]} [location count transpose value] *)
  
  val uniform_matrix4x3dv : int -> int -> bool ->
    (float, Bigarray.float64_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix4x3dv]} [location count transpose value] *)
  
  val uniform_matrix4x3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml}
      [glUniformMatrix4x3fv]} [location count transpose value] *)
  
  val uniform_subroutinesuiv : enum -> int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUniformSubroutines.xhtml}
      [glUniformSubroutinesuiv]} [shadertype count indices] *)
  
  val unmap_buffer : enum -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUnmapBuffer.xhtml}
      [glUnmapBuffer]} [target] *)
  
  val unmap_named_buffer : int -> bool
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUnmapBuffer.xhtml}
      [glUnmapNamedBuffer]} [buffer] *)
  
  val use_program : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUseProgram.xhtml}
      [glUseProgram]} [program] *)
  
  val use_program_stages : int -> bitfield -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glUseProgramStages.xhtml}
      [glUseProgramStages]} [pipeline stages program]
      
      {b Warning.} On 32 bits platforms the constant
                {!all_shader_bits} is represented by 0x7FFFFFFF
                instead of 0xFFFFFFFF, this may result in an OpenGL
                error (or not). *)
  
  val validate_program : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glValidateProgram.xhtml}
      [glValidateProgram]} [program] *)
  
  val validate_program_pipeline : int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glValidateProgramPipeline.xhtml}
      [glValidateProgramPipeline]} [pipeline] *)
  
  val vertex_array_attrib_binding : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribBinding.xhtml}
      [glVertexArrayAttribBinding]} [vaobj attribindex bindingindex] *)
  
  val vertex_array_attrib_format : int -> int -> int -> enum -> bool ->
    int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml}
      [glVertexArrayAttribFormat]} [vaobj attribindex size type_ normalized
        relativeoffset] *)
  
  val vertex_array_attrib_iformat : int -> int -> int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml}
      [glVertexArrayAttribIFormat]} [vaobj attribindex size type_
        relativeoffset] *)
  
  val vertex_array_attrib_lformat : int -> int -> int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml}
      [glVertexArrayAttribLFormat]} [vaobj attribindex size type_
        relativeoffset] *)
  
  val vertex_array_binding_divisor : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexBindingDivisor.xhtml}
      [glVertexArrayBindingDivisor]} [vaobj bindingindex divisor] *)
  
  val vertex_array_element_buffer : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexArrayElementBuffer.xhtml}
      [glVertexArrayElementBuffer]} [vaobj buffer] *)
  
  val vertex_array_vertex_buffer : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffer.xhtml}
      [glVertexArrayVertexBuffer]} [vaobj bindingindex buffer offset stride] *)
  
  val vertex_array_vertex_buffers : int -> int -> int ->
    uint32_bigarray option ->
    (nativeint, Bigarray.nativeint_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray option -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffers.xhtml}
      [glVertexArrayVertexBuffers]} [vaobj first count buffers offsets
        strides] *)
  
  val vertex_attrib1d : int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib1d]} [index x] *)
  
  val vertex_attrib1dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib1dv]} [index v] *)
  
  val vertex_attrib1f : int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib1f]} [index x] *)
  
  val vertex_attrib1fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib1fv]} [index v] *)
  
  val vertex_attrib1s : int -> int16 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib1s]} [index x] *)
  
  val vertex_attrib1sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib1sv]} [index v] *)
  
  val vertex_attrib2d : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib2d]} [index x y] *)
  
  val vertex_attrib2dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib2dv]} [index v] *)
  
  val vertex_attrib2f : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib2f]} [index x y] *)
  
  val vertex_attrib2fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib2fv]} [index v] *)
  
  val vertex_attrib2s : int -> int16 -> int16 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib2s]} [index x y] *)
  
  val vertex_attrib2sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib2sv]} [index v] *)
  
  val vertex_attrib3d : int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib3d]} [index x y z] *)
  
  val vertex_attrib3dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib3dv]} [index v] *)
  
  val vertex_attrib3f : int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib3f]} [index x y z] *)
  
  val vertex_attrib3fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib3fv]} [index v] *)
  
  val vertex_attrib3s : int -> int16 -> int16 -> int16 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib3s]} [index x y z] *)
  
  val vertex_attrib3sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib3sv]} [index v] *)
  
  val vertex_attrib4nbv : int -> (int, Bigarray.int8_signed_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4Nbv]} [index v] *)
  
  val vertex_attrib4niv : int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4Niv]} [index v] *)
  
  val vertex_attrib4nsv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4Nsv]} [index v] *)
  
  val vertex_attrib4nub : int -> uint8 -> uint8 -> uint8 -> uint8 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4Nub]} [index x y z w] *)
  
  val vertex_attrib4nubv : int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4Nubv]} [index v] *)
  
  val vertex_attrib4nuiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4Nuiv]} [index v] *)
  
  val vertex_attrib4nusv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4Nusv]} [index v] *)
  
  val vertex_attrib4bv : int -> (int, Bigarray.int8_signed_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4bv]} [index v] *)
  
  val vertex_attrib4d : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4d]} [index x y z w] *)
  
  val vertex_attrib4dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4dv]} [index v] *)
  
  val vertex_attrib4f : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4f]} [index x y z w] *)
  
  val vertex_attrib4fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4fv]} [index v] *)
  
  val vertex_attrib4iv : int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4iv]} [index v] *)
  
  val vertex_attrib4s : int -> int16 -> int16 -> int16 -> int16 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4s]} [index x y z w] *)
  
  val vertex_attrib4sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4sv]} [index v] *)
  
  val vertex_attrib4ubv : int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4ubv]} [index v] *)
  
  val vertex_attrib4uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4uiv]} [index v] *)
  
  val vertex_attrib4usv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttrib4usv]} [index v] *)
  
  val vertex_attrib_binding : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribBinding.xhtml}
      [glVertexAttribBinding]} [attribindex bindingindex] *)
  
  val vertex_attrib_divisor : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribDivisor.xhtml}
      [glVertexAttribDivisor]} [index divisor] *)
  
  val vertex_attrib_format : int -> int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml}
      [glVertexAttribFormat]} [attribindex size type_ normalized
        relativeoffset] *)
  
  val vertex_attrib_i1i : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI1i]} [index x] *)
  
  val vertex_attrib_i1iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI1iv]} [index v] *)
  
  val vertex_attrib_i1ui : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI1ui]} [index x] *)
  
  val vertex_attrib_i1uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI1uiv]} [index v] *)
  
  val vertex_attrib_i2i : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI2i]} [index x y] *)
  
  val vertex_attrib_i2iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI2iv]} [index v] *)
  
  val vertex_attrib_i2ui : int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI2ui]} [index x y] *)
  
  val vertex_attrib_i2uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI2uiv]} [index v] *)
  
  val vertex_attrib_i3i : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI3i]} [index x y z] *)
  
  val vertex_attrib_i3iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI3iv]} [index v] *)
  
  val vertex_attrib_i3ui : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI3ui]} [index x y z] *)
  
  val vertex_attrib_i3uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI3uiv]} [index v] *)
  
  val vertex_attrib_i4bv : int -> (int, Bigarray.int8_signed_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI4bv]} [index v] *)
  
  val vertex_attrib_i4i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI4i]} [index x y z w] *)
  
  val vertex_attrib_i4iv : int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI4iv]} [index v] *)
  
  val vertex_attrib_i4sv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI4sv]} [index v] *)
  
  val vertex_attrib_i4ubv : int ->
    (int, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI4ubv]} [index v] *)
  
  val vertex_attrib_i4ui : int -> int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI4ui]} [index x y z w] *)
  
  val vertex_attrib_i4uiv : int -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI4uiv]} [index v] *)
  
  val vertex_attrib_i4usv : int ->
    (int, Bigarray.int16_unsigned_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribI4usv]} [index v] *)
  
  val vertex_attrib_iformat : int -> int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml}
      [glVertexAttribIFormat]} [attribindex size type_ relativeoffset] *)
  
  val vertex_attrib_ipointer : int -> int -> enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml}
      [glVertexAttribIPointer]} [index size type_ stride pointer] *)
  
  val vertex_attrib_l1d : int -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribL1d]} [index x] *)
  
  val vertex_attrib_l1dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribL1dv]} [index v] *)
  
  val vertex_attrib_l2d : int -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribL2d]} [index x y] *)
  
  val vertex_attrib_l2dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribL2dv]} [index v] *)
  
  val vertex_attrib_l3d : int -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribL3d]} [index x y z] *)
  
  val vertex_attrib_l3dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribL3dv]} [index v] *)
  
  val vertex_attrib_l4d : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribL4d]} [index x y z w] *)
  
  val vertex_attrib_l4dv : int -> (float, Bigarray.float64_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribL4dv]} [index v] *)
  
  val vertex_attrib_lformat : int -> int -> enum -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml}
      [glVertexAttribLFormat]} [attribindex size type_ relativeoffset] *)
  
  val vertex_attrib_lpointer : int -> int -> enum -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml}
      [glVertexAttribLPointer]} [index size type_ stride pointer] *)
  
  val vertex_attrib_p1ui : int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribP1ui]} [index type_ normalized value] *)
  
  val vertex_attrib_p1uiv : int -> enum -> bool -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribP1uiv]} [index type_ normalized value] *)
  
  val vertex_attrib_p2ui : int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribP2ui]} [index type_ normalized value] *)
  
  val vertex_attrib_p2uiv : int -> enum -> bool -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribP2uiv]} [index type_ normalized value] *)
  
  val vertex_attrib_p3ui : int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribP3ui]} [index type_ normalized value] *)
  
  val vertex_attrib_p3uiv : int -> enum -> bool -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribP3uiv]} [index type_ normalized value] *)
  
  val vertex_attrib_p4ui : int -> enum -> bool -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribP4ui]} [index type_ normalized value] *)
  
  val vertex_attrib_p4uiv : int -> enum -> bool -> uint32_bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml}
      [glVertexAttribP4uiv]} [index type_ normalized value] *)
  
  val vertex_attrib_pointer : int -> int -> enum -> bool -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml}
      [glVertexAttribPointer]} [index size type_ normalized stride pointer] *)
  
  val vertex_binding_divisor : int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glVertexBindingDivisor.xhtml}
      [glVertexBindingDivisor]} [bindingindex divisor] *)
  
  val viewport : int -> int -> int -> int -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glViewport.xhtml}
      [glViewport]} [x y width height] *)
  
  val viewport_arrayv : int -> int ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glViewportArray.xhtml}
      [glViewportArrayv]} [first count v] *)
  
  val viewport_indexedf : int -> float -> float -> float -> float -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glViewportIndexed.xhtml}
      [glViewportIndexedf]} [index x y w h] *)
  
  val viewport_indexedfv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glViewportIndexed.xhtml}
      [glViewportIndexedfv]} [index v] *)
  
  val wait_sync : sync -> bitfield -> uint64 -> unit
  (** {{:http://www.opengl.org/sdk/docs/man4/html/glWaitSync.xhtml}
      [glWaitSync]} [sync flags timeout] *)
  
  (** {1:enums Enums} *)

  val active_atomic_counter_buffers : enum
  
  val active_attributes : enum
  
  val active_attribute_max_length : enum
  
  val active_program : enum
  
  val active_resources : enum
  
  val active_subroutines : enum
  
  val active_subroutine_max_length : enum
  
  val active_subroutine_uniforms : enum
  
  val active_subroutine_uniform_locations : enum
  
  val active_subroutine_uniform_max_length : enum
  
  val active_texture_enum : enum
  
  val active_uniforms : enum
  
  val active_uniform_blocks : enum
  
  val active_uniform_block_max_name_length : enum
  
  val active_uniform_max_length : enum
  
  val active_variables : enum
  
  val aliased_line_width_range : enum
  
  val all_barrier_bits : enum
  
  val all_shader_bits : enum
  
  val alpha : enum
  
  val already_signaled : enum
  
  val always : enum
  
  val and_ : enum
  
  val and_inverted : enum
  
  val and_reverse : enum
  
  val any_samples_passed : enum
  
  val any_samples_passed_conservative : enum
  
  val array_buffer : enum
  
  val array_buffer_binding : enum
  
  val array_size : enum
  
  val array_stride : enum
  
  val atomic_counter_barrier_bit : enum
  
  val atomic_counter_buffer : enum
  
  val atomic_counter_buffer_active_atomic_counters : enum
  
  val atomic_counter_buffer_active_atomic_counter_indices : enum
  
  val atomic_counter_buffer_binding : enum
  
  val atomic_counter_buffer_data_size : enum
  
  val atomic_counter_buffer_index : enum
  
  val atomic_counter_buffer_referenced_by_compute_shader : enum
  
  val atomic_counter_buffer_referenced_by_fragment_shader : enum
  
  val atomic_counter_buffer_referenced_by_geometry_shader : enum
  
  val atomic_counter_buffer_referenced_by_tess_control_shader : enum
  
  val atomic_counter_buffer_referenced_by_tess_evaluation_shader : enum
  
  val atomic_counter_buffer_referenced_by_vertex_shader : enum
  
  val atomic_counter_buffer_size : enum
  
  val atomic_counter_buffer_start : enum
  
  val attached_shaders : enum
  
  val auto_generate_mipmap : enum
  
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
  
  val block_index : enum
  
  val blue : enum
  
  val blue_integer : enum
  
  val bool : enum
  
  val bool_vec2 : enum
  
  val bool_vec3 : enum
  
  val bool_vec4 : enum
  
  val buffer : enum
  
  val buffer_access : enum
  
  val buffer_access_flags : enum
  
  val buffer_binding : enum
  
  val buffer_data_size : enum
  
  val buffer_immutable_storage : enum
  
  val buffer_mapped : enum
  
  val buffer_map_length : enum
  
  val buffer_map_offset : enum
  
  val buffer_map_pointer : enum
  
  val buffer_size : enum
  
  val buffer_storage_flags : enum
  
  val buffer_update_barrier_bit : enum
  
  val buffer_usage : enum
  
  val buffer_variable : enum
  
  val byte : enum
  
  val caveat_support : enum
  
  val ccw : enum
  
  val clamp_read_color : enum
  
  val clamp_to_border : enum
  
  val clamp_to_edge : enum
  
  val clear_enum : enum
  
  val clear_buffer : enum
  
  val clear_texture : enum
  
  val client_mapped_buffer_barrier_bit : enum
  
  val client_storage_bit : enum
  
  val clip_depth_mode : enum
  
  val clip_distance0 : enum
  
  val clip_distance1 : enum
  
  val clip_distance2 : enum
  
  val clip_distance3 : enum
  
  val clip_distance4 : enum
  
  val clip_distance5 : enum
  
  val clip_distance6 : enum
  
  val clip_distance7 : enum
  
  val clip_origin : enum
  
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
  
  val color_components : enum
  
  val color_encoding : enum
  
  val color_logic_op : enum
  
  val color_renderable : enum
  
  val color_writemask : enum
  
  val command_barrier_bit : enum
  
  val compare_ref_to_texture : enum
  
  val compatible_subroutines : enum
  
  val compile_status : enum
  
  val compressed_r11_eac : enum
  
  val compressed_red : enum
  
  val compressed_red_rgtc1 : enum
  
  val compressed_rg : enum
  
  val compressed_rg11_eac : enum
  
  val compressed_rgb : enum
  
  val compressed_rgb8_etc2 : enum
  
  val compressed_rgb8_punchthrough_alpha1_etc2 : enum
  
  val compressed_rgba : enum
  
  val compressed_rgba8_etc2_eac : enum
  
  val compressed_rgba_bptc_unorm : enum
  
  val compressed_rgb_bptc_signed_float : enum
  
  val compressed_rgb_bptc_unsigned_float : enum
  
  val compressed_rg_rgtc2 : enum
  
  val compressed_signed_r11_eac : enum
  
  val compressed_signed_red_rgtc1 : enum
  
  val compressed_signed_rg11_eac : enum
  
  val compressed_signed_rg_rgtc2 : enum
  
  val compressed_srgb : enum
  
  val compressed_srgb8_alpha8_etc2_eac : enum
  
  val compressed_srgb8_etc2 : enum
  
  val compressed_srgb8_punchthrough_alpha1_etc2 : enum
  
  val compressed_srgb_alpha : enum
  
  val compressed_srgb_alpha_bptc_unorm : enum
  
  val compressed_texture_formats : enum
  
  val compute_shader : enum
  
  val compute_shader_bit : enum
  
  val compute_subroutine : enum
  
  val compute_subroutine_uniform : enum
  
  val compute_texture : enum
  
  val compute_work_group_size : enum
  
  val condition_satisfied : enum
  
  val constant_alpha : enum
  
  val constant_color : enum
  
  val context_compatibility_profile_bit : enum
  
  val context_core_profile_bit : enum
  
  val context_flags : enum
  
  val context_flag_debug_bit : enum
  
  val context_flag_forward_compatible_bit : enum
  
  val context_flag_robust_access_bit : enum
  
  val context_lost : enum
  
  val context_profile_mask : enum
  
  val context_release_behavior : enum
  
  val context_release_behavior_flush : enum
  
  val copy : enum
  
  val copy_inverted : enum
  
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
  
  val depth_buffer_bit : enum
  
  val depth_clamp : enum
  
  val depth_clear_value : enum
  
  val depth_component : enum
  
  val depth_component16 : enum
  
  val depth_component24 : enum
  
  val depth_component32 : enum
  
  val depth_component32f : enum
  
  val depth_components : enum
  
  val depth_func_enum : enum
  
  val depth_range_enum : enum
  
  val depth_renderable : enum
  
  val depth_stencil : enum
  
  val depth_stencil_attachment : enum
  
  val depth_stencil_texture_mode : enum
  
  val depth_test : enum
  
  val depth_writemask : enum
  
  val dispatch_indirect_buffer : enum
  
  val dispatch_indirect_buffer_binding : enum
  
  val dither : enum
  
  val dont_care : enum
  
  val double : enum
  
  val doublebuffer : enum
  
  val double_mat2 : enum
  
  val double_mat2x3 : enum
  
  val double_mat2x4 : enum
  
  val double_mat3 : enum
  
  val double_mat3x2 : enum
  
  val double_mat3x4 : enum
  
  val double_mat4 : enum
  
  val double_mat4x2 : enum
  
  val double_mat4x3 : enum
  
  val double_vec2 : enum
  
  val double_vec3 : enum
  
  val double_vec4 : enum
  
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
  
  val draw_indirect_buffer : enum
  
  val draw_indirect_buffer_binding : enum
  
  val dst_alpha : enum
  
  val dst_color : enum
  
  val dynamic_copy : enum
  
  val dynamic_draw : enum
  
  val dynamic_read : enum
  
  val dynamic_storage_bit : enum
  
  val element_array_barrier_bit : enum
  
  val element_array_buffer : enum
  
  val element_array_buffer_binding : enum
  
  val equal : enum
  
  val equiv : enum
  
  val extensions : enum
  
  val false_ : enum
  
  val fastest : enum
  
  val fill : enum
  
  val filter : enum
  
  val first_vertex_convention : enum
  
  val fixed : enum
  
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
  
  val fractional_even : enum
  
  val fractional_odd : enum
  
  val fragment_interpolation_offset_bits : enum
  
  val fragment_shader : enum
  
  val fragment_shader_bit : enum
  
  val fragment_shader_derivative_hint : enum
  
  val fragment_subroutine : enum
  
  val fragment_subroutine_uniform : enum
  
  val fragment_texture : enum
  
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
  
  val framebuffer_blend : enum
  
  val framebuffer_complete : enum
  
  val framebuffer_default : enum
  
  val framebuffer_default_fixed_sample_locations : enum
  
  val framebuffer_default_height : enum
  
  val framebuffer_default_layers : enum
  
  val framebuffer_default_samples : enum
  
  val framebuffer_default_width : enum
  
  val framebuffer_incomplete_attachment : enum
  
  val framebuffer_incomplete_draw_buffer : enum
  
  val framebuffer_incomplete_layer_targets : enum
  
  val framebuffer_incomplete_missing_attachment : enum
  
  val framebuffer_incomplete_multisample : enum
  
  val framebuffer_incomplete_read_buffer : enum
  
  val framebuffer_renderable : enum
  
  val framebuffer_renderable_layered : enum
  
  val framebuffer_srgb : enum
  
  val framebuffer_undefined : enum
  
  val framebuffer_unsupported : enum
  
  val front : enum
  
  val front_and_back : enum
  
  val front_face_enum : enum
  
  val front_left : enum
  
  val front_right : enum
  
  val full_support : enum
  
  val func_add : enum
  
  val func_reverse_subtract : enum
  
  val func_subtract : enum
  
  val geometry_input_type : enum
  
  val geometry_output_type : enum
  
  val geometry_shader : enum
  
  val geometry_shader_bit : enum
  
  val geometry_shader_invocations : enum
  
  val geometry_subroutine : enum
  
  val geometry_subroutine_uniform : enum
  
  val geometry_texture : enum
  
  val geometry_vertices_out : enum
  
  val gequal : enum
  
  val get_texture_image_format : enum
  
  val get_texture_image_type : enum
  
  val greater : enum
  
  val green : enum
  
  val green_integer : enum
  
  val guilty_context_reset : enum
  
  val half_float : enum
  
  val high_float : enum
  
  val high_int : enum
  
  val image_1d : enum
  
  val image_1d_array : enum
  
  val image_2d : enum
  
  val image_2d_array : enum
  
  val image_2d_multisample : enum
  
  val image_2d_multisample_array : enum
  
  val image_2d_rect : enum
  
  val image_3d : enum
  
  val image_binding_access : enum
  
  val image_binding_format : enum
  
  val image_binding_layer : enum
  
  val image_binding_layered : enum
  
  val image_binding_level : enum
  
  val image_binding_name : enum
  
  val image_buffer : enum
  
  val image_class_10_10_10_2 : enum
  
  val image_class_11_11_10 : enum
  
  val image_class_1_x_16 : enum
  
  val image_class_1_x_32 : enum
  
  val image_class_1_x_8 : enum
  
  val image_class_2_x_16 : enum
  
  val image_class_2_x_32 : enum
  
  val image_class_2_x_8 : enum
  
  val image_class_4_x_16 : enum
  
  val image_class_4_x_32 : enum
  
  val image_class_4_x_8 : enum
  
  val image_compatibility_class : enum
  
  val image_cube : enum
  
  val image_cube_map_array : enum
  
  val image_format_compatibility_by_class : enum
  
  val image_format_compatibility_by_size : enum
  
  val image_format_compatibility_type : enum
  
  val image_pixel_format : enum
  
  val image_pixel_type : enum
  
  val image_texel_size : enum
  
  val implementation_color_read_format : enum
  
  val implementation_color_read_type : enum
  
  val incr : enum
  
  val incr_wrap : enum
  
  val info_log_length : enum
  
  val innocent_context_reset : enum
  
  val int : enum
  
  val interleaved_attribs : enum
  
  val internalformat_alpha_size : enum
  
  val internalformat_alpha_type : enum
  
  val internalformat_blue_size : enum
  
  val internalformat_blue_type : enum
  
  val internalformat_depth_size : enum
  
  val internalformat_depth_type : enum
  
  val internalformat_green_size : enum
  
  val internalformat_green_type : enum
  
  val internalformat_preferred : enum
  
  val internalformat_red_size : enum
  
  val internalformat_red_type : enum
  
  val internalformat_shared_size : enum
  
  val internalformat_stencil_size : enum
  
  val internalformat_stencil_type : enum
  
  val internalformat_supported : enum
  
  val int_2_10_10_10_rev : enum
  
  val int_image_1d : enum
  
  val int_image_1d_array : enum
  
  val int_image_2d : enum
  
  val int_image_2d_array : enum
  
  val int_image_2d_multisample : enum
  
  val int_image_2d_multisample_array : enum
  
  val int_image_2d_rect : enum
  
  val int_image_3d : enum
  
  val int_image_buffer : enum
  
  val int_image_cube : enum
  
  val int_image_cube_map_array : enum
  
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
  
  val location : enum
  
  val location_component : enum
  
  val location_index : enum
  
  val logic_op_mode : enum
  
  val lose_context_on_reset : enum
  
  val lower_left : enum
  
  val low_float : enum
  
  val low_int : enum
  
  val major_version : enum
  
  val manual_generate_mipmap : enum
  
  val map_coherent_bit : enum
  
  val map_flush_explicit_bit : enum
  
  val map_invalidate_buffer_bit : enum
  
  val map_invalidate_range_bit : enum
  
  val map_persistent_bit : enum
  
  val map_read_bit : enum
  
  val map_unsynchronized_bit : enum
  
  val map_write_bit : enum
  
  val matrix_stride : enum
  
  val max : enum
  
  val max_3d_texture_size : enum
  
  val max_array_texture_layers : enum
  
  val max_atomic_counter_buffer_bindings : enum
  
  val max_atomic_counter_buffer_size : enum
  
  val max_clip_distances : enum
  
  val max_color_attachments : enum
  
  val max_color_texture_samples : enum
  
  val max_combined_atomic_counters : enum
  
  val max_combined_atomic_counter_buffers : enum
  
  val max_combined_clip_and_cull_distances : enum
  
  val max_combined_compute_uniform_components : enum
  
  val max_combined_dimensions : enum
  
  val max_combined_fragment_uniform_components : enum
  
  val max_combined_geometry_uniform_components : enum
  
  val max_combined_image_uniforms : enum
  
  val max_combined_image_units_and_fragment_outputs : enum
  
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
  
  val max_cull_distances : enum
  
  val max_debug_group_stack_depth : enum
  
  val max_debug_logged_messages : enum
  
  val max_debug_message_length : enum
  
  val max_depth : enum
  
  val max_depth_texture_samples : enum
  
  val max_draw_buffers : enum
  
  val max_dual_source_draw_buffers : enum
  
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
  
  val max_height : enum
  
  val max_image_samples : enum
  
  val max_image_units : enum
  
  val max_integer_samples : enum
  
  val max_label_length : enum
  
  val max_layers : enum
  
  val max_name_length : enum
  
  val max_num_active_variables : enum
  
  val max_num_compatible_subroutines : enum
  
  val max_patch_vertices : enum
  
  val max_program_texel_offset : enum
  
  val max_program_texture_gather_offset : enum
  
  val max_rectangle_texture_size : enum
  
  val max_renderbuffer_size : enum
  
  val max_samples : enum
  
  val max_sample_mask_words : enum
  
  val max_server_wait_timeout : enum
  
  val max_shader_storage_block_size : enum
  
  val max_shader_storage_buffer_bindings : enum
  
  val max_subroutines : enum
  
  val max_subroutine_uniform_locations : enum
  
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
  
  val max_transform_feedback_buffers : enum
  
  val max_transform_feedback_interleaved_components : enum
  
  val max_transform_feedback_separate_attribs : enum
  
  val max_transform_feedback_separate_components : enum
  
  val max_uniform_block_size : enum
  
  val max_uniform_buffer_bindings : enum
  
  val max_uniform_locations : enum
  
  val max_varying_components : enum
  
  val max_varying_floats : enum
  
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
  
  val max_vertex_streams : enum
  
  val max_vertex_texture_image_units : enum
  
  val max_vertex_uniform_blocks : enum
  
  val max_vertex_uniform_components : enum
  
  val max_vertex_uniform_vectors : enum
  
  val max_viewports : enum
  
  val max_viewport_dims : enum
  
  val max_width : enum
  
  val medium_float : enum
  
  val medium_int : enum
  
  val min : enum
  
  val minor_version : enum
  
  val min_fragment_interpolation_offset : enum
  
  val min_map_buffer_alignment : enum
  
  val min_program_texel_offset : enum
  
  val min_program_texture_gather_offset : enum
  
  val min_sample_shading_value : enum
  
  val mipmap : enum
  
  val mirrored_repeat : enum
  
  val mirror_clamp_to_edge : enum
  
  val multisample : enum
  
  val name_length : enum
  
  val nand : enum
  
  val nearest : enum
  
  val nearest_mipmap_linear : enum
  
  val nearest_mipmap_nearest : enum
  
  val negative_one_to_one : enum
  
  val never : enum
  
  val nicest : enum
  
  val none : enum
  
  val noop : enum
  
  val nor : enum
  
  val notequal : enum
  
  val no_error : enum
  
  val no_reset_notification : enum
  
  val num_active_variables : enum
  
  val num_compatible_subroutines : enum
  
  val num_compressed_texture_formats : enum
  
  val num_extensions : enum
  
  val num_program_binary_formats : enum
  
  val num_sample_counts : enum
  
  val num_shader_binary_formats : enum
  
  val num_shading_language_versions : enum
  
  val object_type : enum
  
  val offset : enum
  
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
  
  val pack_compressed_block_depth : enum
  
  val pack_compressed_block_height : enum
  
  val pack_compressed_block_size : enum
  
  val pack_compressed_block_width : enum
  
  val pack_image_height : enum
  
  val pack_lsb_first : enum
  
  val pack_row_length : enum
  
  val pack_skip_images : enum
  
  val pack_skip_pixels : enum
  
  val pack_skip_rows : enum
  
  val pack_swap_bytes : enum
  
  val patches : enum
  
  val patch_default_inner_level : enum
  
  val patch_default_outer_level : enum
  
  val patch_vertices : enum
  
  val pixel_buffer_barrier_bit : enum
  
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
  
  val primitive_restart_fixed_index : enum
  
  val primitive_restart_for_patches_supported : enum
  
  val primitive_restart_index_enum : enum
  
  val program : enum
  
  val program_binary_formats : enum
  
  val program_binary_length : enum
  
  val program_binary_retrievable_hint : enum
  
  val program_input : enum
  
  val program_output : enum
  
  val program_pipeline : enum
  
  val program_pipeline_binding : enum
  
  val program_point_size : enum
  
  val program_separable : enum
  
  val provoking_vertex_enum : enum
  
  val proxy_texture_1d : enum
  
  val proxy_texture_1d_array : enum
  
  val proxy_texture_2d : enum
  
  val proxy_texture_2d_array : enum
  
  val proxy_texture_2d_multisample : enum
  
  val proxy_texture_2d_multisample_array : enum
  
  val proxy_texture_3d : enum
  
  val proxy_texture_cube_map : enum
  
  val proxy_texture_cube_map_array : enum
  
  val proxy_texture_rectangle : enum
  
  val quads : enum
  
  val quads_follow_provoking_vertex_convention : enum
  
  val query : enum
  
  val query_buffer : enum
  
  val query_buffer_barrier_bit : enum
  
  val query_buffer_binding : enum
  
  val query_by_region_no_wait : enum
  
  val query_by_region_no_wait_inverted : enum
  
  val query_by_region_wait : enum
  
  val query_by_region_wait_inverted : enum
  
  val query_counter_bits : enum
  
  val query_no_wait : enum
  
  val query_no_wait_inverted : enum
  
  val query_result : enum
  
  val query_result_available : enum
  
  val query_result_no_wait : enum
  
  val query_target : enum
  
  val query_wait : enum
  
  val query_wait_inverted : enum
  
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
  
  val read_pixels_enum : enum
  
  val read_pixels_format : enum
  
  val read_pixels_type : enum
  
  val read_write : enum
  
  val red : enum
  
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
  
  val rgb565 : enum
  
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
  
  val sampler : enum
  
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
  
  val sampler_cube_map_array : enum
  
  val sampler_cube_map_array_shadow : enum
  
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
  
  val sample_shading : enum
  
  val scissor_box : enum
  
  val scissor_test : enum
  
  val separate_attribs : enum
  
  val set : enum
  
  val shader : enum
  
  val shader_binary_formats : enum
  
  val shader_compiler : enum
  
  val shader_image_access_barrier_bit : enum
  
  val shader_image_atomic : enum
  
  val shader_image_load : enum
  
  val shader_image_store : enum
  
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
  
  val simultaneous_texture_and_depth_test : enum
  
  val simultaneous_texture_and_depth_write : enum
  
  val simultaneous_texture_and_stencil_test : enum
  
  val simultaneous_texture_and_stencil_write : enum
  
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
  
  val srgb_read : enum
  
  val srgb_write : enum
  
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
  
  val stencil_buffer_bit : enum
  
  val stencil_clear_value : enum
  
  val stencil_components : enum
  
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
  
  val stencil_renderable : enum
  
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
  
  val tess_control_output_vertices : enum
  
  val tess_control_shader : enum
  
  val tess_control_shader_bit : enum
  
  val tess_control_subroutine : enum
  
  val tess_control_subroutine_uniform : enum
  
  val tess_control_texture : enum
  
  val tess_evaluation_shader : enum
  
  val tess_evaluation_shader_bit : enum
  
  val tess_evaluation_subroutine : enum
  
  val tess_evaluation_subroutine_uniform : enum
  
  val tess_evaluation_texture : enum
  
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
  
  val texture_binding_cube_map_array : enum
  
  val texture_binding_rectangle : enum
  
  val texture_blue_size : enum
  
  val texture_blue_type : enum
  
  val texture_border_color : enum
  
  val texture_buffer_enum : enum
  
  val texture_buffer_binding : enum
  
  val texture_buffer_data_store_binding : enum
  
  val texture_buffer_offset : enum
  
  val texture_buffer_offset_alignment : enum
  
  val texture_buffer_size : enum
  
  val texture_compare_func : enum
  
  val texture_compare_mode : enum
  
  val texture_compressed : enum
  
  val texture_compressed_block_height : enum
  
  val texture_compressed_block_size : enum
  
  val texture_compressed_block_width : enum
  
  val texture_compressed_image_size : enum
  
  val texture_compression_hint : enum
  
  val texture_cube_map : enum
  
  val texture_cube_map_array : enum
  
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
  
  val texture_fetch_barrier_bit : enum
  
  val texture_fixed_sample_locations : enum
  
  val texture_gather : enum
  
  val texture_gather_shadow : enum
  
  val texture_green_size : enum
  
  val texture_green_type : enum
  
  val texture_height : enum
  
  val texture_image_format : enum
  
  val texture_image_type : enum
  
  val texture_immutable_format : enum
  
  val texture_immutable_levels : enum
  
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
  
  val texture_shadow : enum
  
  val texture_shared_size : enum
  
  val texture_stencil_size : enum
  
  val texture_swizzle_a : enum
  
  val texture_swizzle_b : enum
  
  val texture_swizzle_g : enum
  
  val texture_swizzle_r : enum
  
  val texture_swizzle_rgba : enum
  
  val texture_target : enum
  
  val texture_update_barrier_bit : enum
  
  val texture_view_enum : enum
  
  val texture_view_min_layer : enum
  
  val texture_view_min_level : enum
  
  val texture_view_num_layers : enum
  
  val texture_view_num_levels : enum
  
  val texture_width : enum
  
  val texture_wrap_r : enum
  
  val texture_wrap_s : enum
  
  val texture_wrap_t : enum
  
  val timeout_expired : enum
  
  val timeout_ignored : int64
  
  val timestamp : enum
  
  val time_elapsed : enum
  
  val top_level_array_size : enum
  
  val top_level_array_stride : enum
  
  val transform_feedback : enum
  
  val transform_feedback_active : enum
  
  val transform_feedback_barrier_bit : enum
  
  val transform_feedback_binding : enum
  
  val transform_feedback_buffer : enum
  
  val transform_feedback_buffer_active : enum
  
  val transform_feedback_buffer_binding : enum
  
  val transform_feedback_buffer_index : enum
  
  val transform_feedback_buffer_mode : enum
  
  val transform_feedback_buffer_paused : enum
  
  val transform_feedback_buffer_size : enum
  
  val transform_feedback_buffer_start : enum
  
  val transform_feedback_buffer_stride : enum
  
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
  
  val uniform_atomic_counter_buffer_index : enum
  
  val uniform_barrier_bit : enum
  
  val uniform_block : enum
  
  val uniform_block_active_uniforms : enum
  
  val uniform_block_active_uniform_indices : enum
  
  val uniform_block_binding_enum : enum
  
  val uniform_block_data_size : enum
  
  val uniform_block_index : enum
  
  val uniform_block_name_length : enum
  
  val uniform_block_referenced_by_compute_shader : enum
  
  val uniform_block_referenced_by_fragment_shader : enum
  
  val uniform_block_referenced_by_geometry_shader : enum
  
  val uniform_block_referenced_by_tess_control_shader : enum
  
  val uniform_block_referenced_by_tess_evaluation_shader : enum
  
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
  
  val unpack_compressed_block_depth : enum
  
  val unpack_compressed_block_height : enum
  
  val unpack_compressed_block_size : enum
  
  val unpack_compressed_block_width : enum
  
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
  
  val unsigned_int_atomic_counter : enum
  
  val unsigned_int_image_1d : enum
  
  val unsigned_int_image_1d_array : enum
  
  val unsigned_int_image_2d : enum
  
  val unsigned_int_image_2d_array : enum
  
  val unsigned_int_image_2d_multisample : enum
  
  val unsigned_int_image_2d_multisample_array : enum
  
  val unsigned_int_image_2d_rect : enum
  
  val unsigned_int_image_3d : enum
  
  val unsigned_int_image_buffer : enum
  
  val unsigned_int_image_cube : enum
  
  val unsigned_int_image_cube_map_array : enum
  
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
  
  val unsigned_int_sampler_cube_map_array : enum
  
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
  
  val vertex_array : enum
  
  val vertex_array_binding : enum
  
  val vertex_attrib_array_barrier_bit : enum
  
  val vertex_attrib_array_buffer_binding : enum
  
  val vertex_attrib_array_divisor : enum
  
  val vertex_attrib_array_enabled : enum
  
  val vertex_attrib_array_integer : enum
  
  val vertex_attrib_array_long : enum
  
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
  
  val vertex_program_point_size : enum
  
  val vertex_shader : enum
  
  val vertex_shader_bit : enum
  
  val vertex_subroutine : enum
  
  val vertex_subroutine_uniform : enum
  
  val vertex_texture : enum
  
  val viewport_enum : enum
  
  val viewport_bounds_range : enum
  
  val viewport_index_provoking_vertex : enum
  
  val viewport_subpixel_bits : enum
  
  val view_class_128_bits : enum
  
  val view_class_16_bits : enum
  
  val view_class_24_bits : enum
  
  val view_class_32_bits : enum
  
  val view_class_48_bits : enum
  
  val view_class_64_bits : enum
  
  val view_class_8_bits : enum
  
  val view_class_96_bits : enum
  
  val view_class_bptc_float : enum
  
  val view_class_bptc_unorm : enum
  
  val view_class_rgtc1_red : enum
  
  val view_class_rgtc2_rg : enum
  
  val view_class_s3tc_dxt1_rgb : enum
  
  val view_class_s3tc_dxt1_rgba : enum
  
  val view_class_s3tc_dxt3_rgba : enum
  
  val view_class_s3tc_dxt5_rgba : enum
  
  val view_compatibility_class : enum
  
  val wait_failed : enum
  
  val write_only : enum
  
  val xor : enum
  
  val zero : enum
  
  val zero_to_one : enum
  
end

(** {1:conventions Conventions}

    To find the name of an OCaml function corresponding to a C
    function name, map the [gl] prefix to the module name
    {!Tgl4.Gl},
    add an underscore between each minuscule and majuscule and lower
    case the result. For example [glGetError] maps to
    {!Tgl4.Gl.get_error}

    To find the name of an OCaml value corresponding to a C enumerant name,
    map the [GL_] prefix to the module name {!Tgl4.Gl}
    and lower case the rest. For example [GL_COLOR_BUFFER_BIT] maps to
    {!Tgl4.Gl.color_buffer_bit}.

    The following exceptions occur:
    {ul
    {- A few enumerant names do clash with functions name. In that case we
       postfix the enumerant name with [_enum]. For example we have
       {!Tgl4.Gl.viewport} and {!Tgl4.Gl.viewport_enum}.}
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
