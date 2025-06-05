(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated with:
   apiquery -mli -api gles2.0 *)

(** OpenGL ES 2 thin bindings.

    [Tgles2] can program  OpenGL ES 2 contexts.
    Consult the {{!conventions}binding conventions}.

    Open the module use it, this defines only the module [Gl]
    in your scope. To use in the toplevel with [findlib],
    just [#require "tgls.tgles2"], it automatically loads the library and
    opens the [Tgles2] module.

    {b References}
    {ul
    {- {{:http://www.khronos.org/opengles/2_X}OpenGL ES 2}}}

    {e %%VERSION%% — OpenGL ES 2 — {{:%%PKG_HOMEPAGE%% }homepage} } *)

(** {1 OpenGL ES 2} *)

(** OpenGL ES 2 bindings.
    
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
  type uint32_bigarray = (int32, Bigarray.int32_elt) bigarray
  type debug_proc = enum -> enum -> int -> enum -> string -> unit
  
  (** {1:funs Functions} *)

  val active_texture : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glActiveTexture.xml}
      [glActiveTexture]} [texture] *)
  
  val attach_shader : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glAttachShader.xml}
      [glAttachShader]} [program shader] *)
  
  val bind_attrib_location : int -> int -> string -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBindAttribLocation.xml}
      [glBindAttribLocation]} [program index name] *)
  
  val bind_buffer : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBindBuffer.xml}
      [glBindBuffer]} [target buffer] *)
  
  val bind_framebuffer : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBindFramebuffer.xml}
      [glBindFramebuffer]} [target framebuffer] *)
  
  val bind_renderbuffer : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBindRenderbuffer.xml}
      [glBindRenderbuffer]} [target renderbuffer] *)
  
  val bind_texture : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBindTexture.xml}
      [glBindTexture]} [target texture] *)
  
  val blend_color : float -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendColor.xml}
      [glBlendColor]} [red green blue alpha] *)
  
  val blend_equation : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendEquation.xml}
      [glBlendEquation]} [mode] *)
  
  val blend_equation_separate : enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendEquationSeparate.xml}
      [glBlendEquationSeparate]} [modeRGB modeAlpha] *)
  
  val blend_func : enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendFunc.xml}
      [glBlendFunc]} [sfactor dfactor] *)
  
  val blend_func_separate : enum -> enum -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendFuncSeparate.xml}
      [glBlendFuncSeparate]} [sfactorRGB dfactorRGB sfactorAlpha
        dfactorAlpha] *)
  
  val buffer_data : enum -> int -> ('a, 'b) bigarray option -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBufferData.xml}
      [glBufferData]} [target size data usage] *)
  
  val buffer_sub_data : enum -> int -> int -> ('a, 'b) bigarray option ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glBufferSubData.xml}
      [glBufferSubData]} [target offset size data] *)
  
  val check_framebuffer_status : enum -> enum
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCheckFramebufferStatus.xml}
      [glCheckFramebufferStatus]} [target] *)
  
  val clear : bitfield -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glClear.xml}
      [glClear]} [mask] *)
  
  val clear_color : float -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glClearColor.xml}
      [glClearColor]} [red green blue alpha] *)
  
  val clear_depthf : float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glClearDepth.xml}
      [glClearDepthf]} [d] *)
  
  val clear_stencil : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glClearStencil.xml}
      [glClearStencil]} [s] *)
  
  val color_mask : bool -> bool -> bool -> bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glColorMask.xml}
      [glColorMask]} [red green blue alpha] *)
  
  val compile_shader : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCompileShader.xml}
      [glCompileShader]} [shader] *)
  
  val compressed_tex_image2d : enum -> int -> enum -> int -> int -> int ->
    int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCompressedTexImage2D.xml}
      [glCompressedTexImage2D]} [target level internalformat width height
        border imageSize data] *)
  
  val compressed_tex_sub_image2d : enum -> int -> int -> int -> int -> int ->
    enum -> int -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCompressedTexSubImage2D.xml}
      [glCompressedTexSubImage2D]} [target level xoffset yoffset width height
        format imageSize data] *)
  
  val copy_tex_image2d : enum -> int -> enum -> int -> int -> int -> int ->
    int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCopyTexImage2D.xml}
      [glCopyTexImage2D]} [target level internalformat x y width height
        border] *)
  
  val copy_tex_sub_image2d : enum -> int -> int -> int -> int -> int ->
    int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCopyTexSubImage2D.xml}
      [glCopyTexSubImage2D]} [target level xoffset yoffset x y width height] *)
  
  val create_program : unit -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCreateProgram.xml}
      [glCreateProgram]} [()] *)
  
  val create_shader : enum -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCreateShader.xml}
      [glCreateShader]} [type_] *)
  
  val cull_face : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glCullFace.xml}
      [glCullFace]} [mode] *)
  
  val delete_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDeleteBuffers.xml}
      [glDeleteBuffers]} [n buffers] *)
  
  val delete_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDeleteFramebuffers.xml}
      [glDeleteFramebuffers]} [n framebuffers] *)
  
  val delete_program : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDeleteProgram.xml}
      [glDeleteProgram]} [program] *)
  
  val delete_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDeleteRenderbuffers.xml}
      [glDeleteRenderbuffers]} [n renderbuffers] *)
  
  val delete_shader : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDeleteShader.xml}
      [glDeleteShader]} [shader] *)
  
  val delete_textures : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDeleteTextures.xml}
      [glDeleteTextures]} [n textures] *)
  
  val depth_func : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDepthFunc.xml}
      [glDepthFunc]} [func] *)
  
  val depth_mask : bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDepthMask.xml}
      [glDepthMask]} [flag] *)
  
  val depth_rangef : float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDepthRange.xml}
      [glDepthRangef]} [n f] *)
  
  val detach_shader : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDetachShader.xml}
      [glDetachShader]} [program shader] *)
  
  val disable : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glEnable.xml}
      [glDisable]} [cap] *)
  
  val disable_vertex_attrib_array : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml}
      [glDisableVertexAttribArray]} [index] *)
  
  val draw_arrays : enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDrawArrays.xml}
      [glDrawArrays]} [mode first count] *)
  
  val draw_elements : enum -> int -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glDrawElements.xml}
      [glDrawElements]} [mode count type_ indices] *)
  
  val enable : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glEnable.xml}
      [glEnable]} [cap] *)
  
  val enable_vertex_attrib_array : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml}
      [glEnableVertexAttribArray]} [index] *)
  
  val finish : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glFinish.xml}
      [glFinish]} [()] *)
  
  val flush : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glFlush.xml}
      [glFlush]} [()] *)
  
  val framebuffer_renderbuffer : enum -> enum -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glFramebufferRenderbuffer.xml}
      [glFramebufferRenderbuffer]} [target attachment renderbuffertarget
        renderbuffer] *)
  
  val framebuffer_texture2d : enum -> enum -> enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glFramebufferTexture.xml}
      [glFramebufferTexture2D]} [target attachment textarget texture level] *)
  
  val front_face : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glFrontFace.xml}
      [glFrontFace]} [mode] *)
  
  val gen_buffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGenBuffers.xml}
      [glGenBuffers]} [n buffers] *)
  
  val gen_framebuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGenFramebuffers.xml}
      [glGenFramebuffers]} [n framebuffers] *)
  
  val gen_renderbuffers : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGenRenderbuffers.xml}
      [glGenRenderbuffers]} [n renderbuffers] *)
  
  val gen_textures : int -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGenTextures.xml}
      [glGenTextures]} [n textures] *)
  
  val generate_mipmap : enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGenerateMipmap.xml}
      [glGenerateMipmap]} [target] *)
  
  val get_active_attrib : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetActiveAttrib.xml}
      [glGetActiveAttrib]} [program index bufSize length size type_ name] *)
  
  val get_active_uniform : int -> int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (int32, Bigarray.int32_elt) bigarray -> enum_bigarray ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetActiveUniform.xml}
      [glGetActiveUniform]} [program index bufSize length size type_ name] *)
  
  val get_attached_shaders : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option -> uint32_bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetAttachedShaders.xml}
      [glGetAttachedShaders]} [program maxCount count shaders] *)
  
  val get_attrib_location : int -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetAttribLocation.xml}
      [glGetAttribLocation]} [program name] *)
  
  val get_booleanv : enum -> (int, Bigarray.int8_unsigned_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGet.xml}
      [glGetBooleanv]} [pname data] *)
  
  val get_buffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetBufferParameter.xml}
      [glGetBufferParameteriv]} [target pname params] *)
  
  val get_error : unit -> enum
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetError.xml}
      [glGetError]} [()] *)
  
  val get_floatv : enum -> (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGet.xml}
      [glGetFloatv]} [pname data] *)
  
  val get_framebuffer_attachment_parameteriv : enum -> enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetFramebufferAttachmentParameter.xml}
      [glGetFramebufferAttachmentParameteriv]} [target attachment pname
        params] *)
  
  val get_integerv : enum -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGet.xml}
      [glGetIntegerv]} [pname data] *)
  
  val get_program_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetProgramInfoLog.xml}
      [glGetProgramInfoLog]} [program bufSize length infoLog] *)
  
  val get_programiv : int -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetProgram.xml}
      [glGetProgramiv]} [program pname params] *)
  
  val get_renderbuffer_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetRenderbufferParameter.xml}
      [glGetRenderbufferParameteriv]} [target pname params] *)
  
  val get_shader_info_log : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetShaderInfoLog.xml}
      [glGetShaderInfoLog]} [shader bufSize length infoLog] *)
  
  val get_shader_precision_format : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetShaderPrecisionFormat.xml}
      [glGetShaderPrecisionFormat]} [shadertype precisiontype range
        precision] *)
  
  val get_shader_source : int -> int ->
    (int32, Bigarray.int32_elt) bigarray option ->
    (char, Bigarray.int8_unsigned_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetShaderSource.xml}
      [glGetShaderSource]} [shader bufSize length source] *)
  
  val get_shaderiv : int -> enum -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetShader.xml}
      [glGetShaderiv]} [shader pname params] *)
  
  val get_string : enum -> string option
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetString.xml}
      [glGetString]} [name] *)
  
  val get_tex_parameterfv : enum -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetTexParameter.xml}
      [glGetTexParameterfv]} [target pname params] *)
  
  val get_tex_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetTexParameter.xml}
      [glGetTexParameteriv]} [target pname params] *)
  
  val get_uniform_location : int -> string -> int
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetUniformLocation.xml}
      [glGetUniformLocation]} [program name] *)
  
  val get_uniformfv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetUniform.xml}
      [glGetUniformfv]} [program location params] *)
  
  val get_uniformiv : int -> int -> (int32, Bigarray.int32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetUniform.xml}
      [glGetUniformiv]} [program location params] *)
  
  val get_vertex_attrib_pointerv : int -> enum ->
    (nativeint, Bigarray.nativeint_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetVertexAttribPointerv.xml}
      [glGetVertexAttribPointerv]} [index pname pointer] *)
  
  val get_vertex_attribfv : int -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetVertexAttrib.xml}
      [glGetVertexAttribfv]} [index pname params] *)
  
  val get_vertex_attribiv : int -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetVertexAttrib.xml}
      [glGetVertexAttribiv]} [index pname params] *)
  
  val hint : enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glHint.xml}
      [glHint]} [target mode] *)
  
  val is_buffer : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glIsBuffer.xml}
      [glIsBuffer]} [buffer] *)
  
  val is_enabled : enum -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glIsEnabled.xml}
      [glIsEnabled]} [cap] *)
  
  val is_framebuffer : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glIsFramebuffer.xml}
      [glIsFramebuffer]} [framebuffer] *)
  
  val is_program : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glIsProgram.xml}
      [glIsProgram]} [program] *)
  
  val is_renderbuffer : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glIsRenderbuffer.xml}
      [glIsRenderbuffer]} [renderbuffer] *)
  
  val is_shader : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glIsShader.xml}
      [glIsShader]} [shader] *)
  
  val is_texture : int -> bool
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glIsTexture.xml}
      [glIsTexture]} [texture] *)
  
  val line_width : float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glLineWidth.xml}
      [glLineWidth]} [width] *)
  
  val link_program : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glLinkProgram.xml}
      [glLinkProgram]} [program] *)
  
  val pixel_storei : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glPixelStore.xml}
      [glPixelStorei]} [pname param] *)
  
  val polygon_offset : float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glPolygonOffset.xml}
      [glPolygonOffset]} [factor units] *)
  
  val read_pixels : int -> int -> int -> int -> enum -> enum ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glReadPixels.xml}
      [glReadPixels]} [x y width height format type_ pixels] *)
  
  val release_shader_compiler : unit -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glReleaseShaderCompiler.xml}
      [glReleaseShaderCompiler]} [()] *)
  
  val renderbuffer_storage : enum -> enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glRenderbufferStorage.xml}
      [glRenderbufferStorage]} [target internalformat width height] *)
  
  val sample_coverage : float -> bool -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glSampleCoverage.xml}
      [glSampleCoverage]} [value invert] *)
  
  val scissor : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glScissor.xml}
      [glScissor]} [x y width height] *)
  
  val shader_binary : int -> uint32_bigarray -> enum -> ('a, 'b) bigarray ->
    int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glShaderBinary.xml}
      [glShaderBinary]} [count shaders binaryformat binary length] *)
  
  val shader_source : int -> string -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glShaderSource.xml}
      [glShaderSource]} [shader source] *)
  
  val stencil_func : enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glStencilFunc.xml}
      [glStencilFunc]} [func ref mask] *)
  
  val stencil_func_separate : enum -> enum -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glStencilFuncSeparate.xml}
      [glStencilFuncSeparate]} [face func ref mask] *)
  
  val stencil_mask : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glStencilMask.xml}
      [glStencilMask]} [mask] *)
  
  val stencil_mask_separate : enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glStencilMaskSeparate.xml}
      [glStencilMaskSeparate]} [face mask] *)
  
  val stencil_op : enum -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glStencilOp.xml}
      [glStencilOp]} [fail zfail zpass] *)
  
  val stencil_op_separate : enum -> enum -> enum -> enum -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glStencilOpSeparate.xml}
      [glStencilOpSeparate]} [face sfail dpfail dppass] *)
  
  val tex_image2d : enum -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glTexImage2D.xml}
      [glTexImage2D]} [target level internalformat width height border format
        type_ pixels] *)
  
  val tex_parameterf : enum -> enum -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glTexParameter.xml}
      [glTexParameterf]} [target pname param] *)
  
  val tex_parameterfv : enum -> enum ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glTexParameter.xml}
      [glTexParameterfv]} [target pname params] *)
  
  val tex_parameteri : enum -> enum -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glTexParameter.xml}
      [glTexParameteri]} [target pname param] *)
  
  val tex_parameteriv : enum -> enum ->
    (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glTexParameter.xml}
      [glTexParameteriv]} [target pname params] *)
  
  val tex_sub_image2d : enum -> int -> int -> int -> int -> int -> enum ->
    enum -> [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glTexSubImage2D.xml}
      [glTexSubImage2D]} [target level xoffset yoffset width height format
        type_ pixels] *)
  
  val uniform1f : int -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform1f]} [location v0] *)
  
  val uniform1fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform1fv]} [location count value] *)
  
  val uniform1i : int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform1i]} [location v0] *)
  
  val uniform1iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform1iv]} [location count value] *)
  
  val uniform2f : int -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform2f]} [location v0 v1] *)
  
  val uniform2fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform2fv]} [location count value] *)
  
  val uniform2i : int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform2i]} [location v0 v1] *)
  
  val uniform2iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform2iv]} [location count value] *)
  
  val uniform3f : int -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform3f]} [location v0 v1 v2] *)
  
  val uniform3fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform3fv]} [location count value] *)
  
  val uniform3i : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform3i]} [location v0 v1 v2] *)
  
  val uniform3iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform3iv]} [location count value] *)
  
  val uniform4f : int -> float -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform4f]} [location v0 v1 v2 v3] *)
  
  val uniform4fv : int -> int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform4fv]} [location count value] *)
  
  val uniform4i : int -> int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform4i]} [location v0 v1 v2 v3] *)
  
  val uniform4iv : int -> int -> (int32, Bigarray.int32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniform4iv]} [location count value] *)
  
  val uniform_matrix2fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniformMatrix2fv]} [location count transpose value] *)
  
  val uniform_matrix3fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniformMatrix3fv]} [location count transpose value] *)
  
  val uniform_matrix4fv : int -> int -> bool ->
    (float, Bigarray.float32_elt) bigarray -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUniform.xml}
      [glUniformMatrix4fv]} [location count transpose value] *)
  
  val use_program : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glUseProgram.xml}
      [glUseProgram]} [program] *)
  
  val validate_program : int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glValidateProgram.xml}
      [glValidateProgram]} [program] *)
  
  val vertex_attrib1f : int -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttrib.xml}
      [glVertexAttrib1f]} [index x] *)
  
  val vertex_attrib1fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttrib.xml}
      [glVertexAttrib1fv]} [index v] *)
  
  val vertex_attrib2f : int -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttrib.xml}
      [glVertexAttrib2f]} [index x y] *)
  
  val vertex_attrib2fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttrib.xml}
      [glVertexAttrib2fv]} [index v] *)
  
  val vertex_attrib3f : int -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttrib.xml}
      [glVertexAttrib3f]} [index x y z] *)
  
  val vertex_attrib3fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttrib.xml}
      [glVertexAttrib3fv]} [index v] *)
  
  val vertex_attrib4f : int -> float -> float -> float -> float -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4f]} [index x y z w] *)
  
  val vertex_attrib4fv : int -> (float, Bigarray.float32_elt) bigarray ->
    unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttrib.xml}
      [glVertexAttrib4fv]} [index v] *)
  
  val vertex_attrib_pointer : int -> int -> enum -> bool -> int ->
    [ `Offset of int | `Data of ('a, 'b) bigarray ] -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glVertexAttribPointer.xml}
      [glVertexAttribPointer]} [index size type_ normalized stride pointer] *)
  
  val viewport : int -> int -> int -> int -> unit
  (** {{:http://www.khronos.org/opengles/sdk/docs/man/xhtml/glViewport.xml}
      [glViewport]} [x y width height] *)
  
  (** {1:enums Enums} *)

  val active_attributes : enum
  
  val active_attribute_max_length : enum
  
  val active_texture_enum : enum
  
  val active_uniforms : enum
  
  val active_uniform_max_length : enum
  
  val aliased_line_width_range : enum
  
  val aliased_point_size_range : enum
  
  val alpha : enum
  
  val alpha_bits : enum
  
  val always : enum
  
  val array_buffer : enum
  
  val array_buffer_binding : enum
  
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
  
  val blue_bits : enum
  
  val bool : enum
  
  val bool_vec2 : enum
  
  val bool_vec3 : enum
  
  val bool_vec4 : enum
  
  val buffer_size : enum
  
  val buffer_usage : enum
  
  val byte : enum
  
  val ccw : enum
  
  val clamp_to_edge : enum
  
  val color_attachment0 : enum
  
  val color_buffer_bit : enum
  
  val color_clear_value : enum
  
  val color_writemask : enum
  
  val compile_status : enum
  
  val compressed_texture_formats : enum
  
  val constant_alpha : enum
  
  val constant_color : enum
  
  val cull_face_enum : enum
  
  val cull_face_mode : enum
  
  val current_program : enum
  
  val current_vertex_attrib : enum
  
  val cw : enum
  
  val decr : enum
  
  val decr_wrap : enum
  
  val delete_status : enum
  
  val depth_attachment : enum
  
  val depth_bits : enum
  
  val depth_buffer_bit : enum
  
  val depth_clear_value : enum
  
  val depth_component : enum
  
  val depth_component16 : enum
  
  val depth_func_enum : enum
  
  val depth_range : enum
  
  val depth_test : enum
  
  val depth_writemask : enum
  
  val dither : enum
  
  val dont_care : enum
  
  val dst_alpha : enum
  
  val dst_color : enum
  
  val dynamic_draw : enum
  
  val element_array_buffer : enum
  
  val element_array_buffer_binding : enum
  
  val equal : enum
  
  val extensions : enum
  
  val false_ : enum
  
  val fastest : enum
  
  val fixed : enum
  
  val float : enum
  
  val float_mat2 : enum
  
  val float_mat3 : enum
  
  val float_mat4 : enum
  
  val float_vec2 : enum
  
  val float_vec3 : enum
  
  val float_vec4 : enum
  
  val fragment_shader : enum
  
  val framebuffer : enum
  
  val framebuffer_attachment_object_name : enum
  
  val framebuffer_attachment_object_type : enum
  
  val framebuffer_attachment_texture_cube_map_face : enum
  
  val framebuffer_attachment_texture_level : enum
  
  val framebuffer_binding : enum
  
  val framebuffer_complete : enum
  
  val framebuffer_incomplete_attachment : enum
  
  val framebuffer_incomplete_dimensions : enum
  
  val framebuffer_incomplete_missing_attachment : enum
  
  val framebuffer_unsupported : enum
  
  val front : enum
  
  val front_and_back : enum
  
  val front_face_enum : enum
  
  val func_add : enum
  
  val func_reverse_subtract : enum
  
  val func_subtract : enum
  
  val generate_mipmap_hint : enum
  
  val gequal : enum
  
  val greater : enum
  
  val green_bits : enum
  
  val high_float : enum
  
  val high_int : enum
  
  val implementation_color_read_format : enum
  
  val implementation_color_read_type : enum
  
  val incr : enum
  
  val incr_wrap : enum
  
  val info_log_length : enum
  
  val int : enum
  
  val int_vec2 : enum
  
  val int_vec3 : enum
  
  val int_vec4 : enum
  
  val invalid_enum : enum
  
  val invalid_framebuffer_operation : enum
  
  val invalid_operation : enum
  
  val invalid_value : enum
  
  val invert : enum
  
  val keep : enum
  
  val lequal : enum
  
  val less : enum
  
  val linear : enum
  
  val linear_mipmap_linear : enum
  
  val linear_mipmap_nearest : enum
  
  val lines : enum
  
  val line_loop : enum
  
  val line_strip : enum
  
  val line_width_enum : enum
  
  val link_status : enum
  
  val low_float : enum
  
  val low_int : enum
  
  val luminance : enum
  
  val luminance_alpha : enum
  
  val max_combined_texture_image_units : enum
  
  val max_cube_map_texture_size : enum
  
  val max_fragment_uniform_vectors : enum
  
  val max_renderbuffer_size : enum
  
  val max_texture_image_units : enum
  
  val max_texture_size : enum
  
  val max_varying_vectors : enum
  
  val max_vertex_attribs : enum
  
  val max_vertex_texture_image_units : enum
  
  val max_vertex_uniform_vectors : enum
  
  val max_viewport_dims : enum
  
  val medium_float : enum
  
  val medium_int : enum
  
  val mirrored_repeat : enum
  
  val nearest : enum
  
  val nearest_mipmap_linear : enum
  
  val nearest_mipmap_nearest : enum
  
  val never : enum
  
  val nicest : enum
  
  val none : enum
  
  val notequal : enum
  
  val no_error : enum
  
  val num_compressed_texture_formats : enum
  
  val num_shader_binary_formats : enum
  
  val one : enum
  
  val one_minus_constant_alpha : enum
  
  val one_minus_constant_color : enum
  
  val one_minus_dst_alpha : enum
  
  val one_minus_dst_color : enum
  
  val one_minus_src_alpha : enum
  
  val one_minus_src_color : enum
  
  val out_of_memory : enum
  
  val pack_alignment : enum
  
  val points : enum
  
  val polygon_offset_factor : enum
  
  val polygon_offset_fill : enum
  
  val polygon_offset_units : enum
  
  val red_bits : enum
  
  val renderbuffer : enum
  
  val renderbuffer_alpha_size : enum
  
  val renderbuffer_binding : enum
  
  val renderbuffer_blue_size : enum
  
  val renderbuffer_depth_size : enum
  
  val renderbuffer_green_size : enum
  
  val renderbuffer_height : enum
  
  val renderbuffer_internal_format : enum
  
  val renderbuffer_red_size : enum
  
  val renderbuffer_stencil_size : enum
  
  val renderbuffer_width : enum
  
  val renderer : enum
  
  val repeat : enum
  
  val replace : enum
  
  val rgb : enum
  
  val rgb565 : enum
  
  val rgb5_a1 : enum
  
  val rgba : enum
  
  val rgba4 : enum
  
  val sampler_2d : enum
  
  val sampler_cube : enum
  
  val samples : enum
  
  val sample_alpha_to_coverage : enum
  
  val sample_buffers : enum
  
  val sample_coverage_enum : enum
  
  val sample_coverage_invert : enum
  
  val sample_coverage_value : enum
  
  val scissor_box : enum
  
  val scissor_test : enum
  
  val shader_binary_formats : enum
  
  val shader_compiler : enum
  
  val shader_source_length : enum
  
  val shader_type : enum
  
  val shading_language_version : enum
  
  val short : enum
  
  val src_alpha : enum
  
  val src_alpha_saturate : enum
  
  val src_color : enum
  
  val static_draw : enum
  
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
  
  val stencil_index8 : enum
  
  val stencil_pass_depth_fail : enum
  
  val stencil_pass_depth_pass : enum
  
  val stencil_ref : enum
  
  val stencil_test : enum
  
  val stencil_value_mask : enum
  
  val stencil_writemask : enum
  
  val stream_draw : enum
  
  val subpixel_bits : enum
  
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
  
  val texture_binding_2d : enum
  
  val texture_binding_cube_map : enum
  
  val texture_cube_map : enum
  
  val texture_cube_map_negative_x : enum
  
  val texture_cube_map_negative_y : enum
  
  val texture_cube_map_negative_z : enum
  
  val texture_cube_map_positive_x : enum
  
  val texture_cube_map_positive_y : enum
  
  val texture_cube_map_positive_z : enum
  
  val texture_mag_filter : enum
  
  val texture_min_filter : enum
  
  val texture_wrap_s : enum
  
  val texture_wrap_t : enum
  
  val triangles : enum
  
  val triangle_fan : enum
  
  val triangle_strip : enum
  
  val true_ : enum
  
  val unpack_alignment : enum
  
  val unsigned_byte : enum
  
  val unsigned_int : enum
  
  val unsigned_short : enum
  
  val unsigned_short_4_4_4_4 : enum
  
  val unsigned_short_5_5_5_1 : enum
  
  val unsigned_short_5_6_5 : enum
  
  val validate_status : enum
  
  val vendor : enum
  
  val version : enum
  
  val vertex_attrib_array_buffer_binding : enum
  
  val vertex_attrib_array_enabled : enum
  
  val vertex_attrib_array_normalized : enum
  
  val vertex_attrib_array_pointer : enum
  
  val vertex_attrib_array_size : enum
  
  val vertex_attrib_array_stride : enum
  
  val vertex_attrib_array_type : enum
  
  val vertex_shader : enum
  
  val viewport_enum : enum
  
  val zero : enum
  
end

(** {1:conventions Conventions}

    To find the name of an OCaml function corresponding to a C
    function name, map the [gl] prefix to the module name
    {!Tgles2.Gl},
    add an underscore between each minuscule and majuscule and lower
    case the result. For example [glGetError] maps to
    {!Tgles2.Gl.get_error}

    To find the name of an OCaml value corresponding to a C enumerant name,
    map the [GL_] prefix to the module name {!Tgles2.Gl}
    and lower case the rest. For example [GL_COLOR_BUFFER_BIT] maps to
    {!Tgles2.Gl.color_buffer_bit}.

    The following exceptions occur:
    {ul
    {- A few enumerant names do clash with functions name. In that case we
       postfix the enumerant name with [_enum]. For example we have
       {!Tgles2.Gl.viewport} and {!Tgles2.Gl.viewport_enum}.}
    {- If applying the above procedures results in an identifier that
       doesn't start with a letter, prefix the identifier with a ['_'].}
    {- If applying the above procedures results in an identifier that
       is an OCaml keyword, suffix the identifier with a ['_'].}} *)