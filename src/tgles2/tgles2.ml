(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated with:
   apiquery -ml -api gles2.0 *)

open Ctypes
open Foreign

let from =
  if Sys.win32 then
    try
      Some (Dl.(dlopen ~filename:"opengl32.dll" ~flags:[ RTLD_NOW ]))
    with _ ->
      (* In case some setups don't have the standard [opengl32.dll],
         don't prevent running by failing at toplevel. *)
      None
  else None

let abi =
  if Sys.win32 && Sys.word_size = 32 then
    (* On X86 (32-bit) under Windows, [opengl32.dll] uses the [__stdcall] FFI ABI.
       This is not the default for [libffi], so it may require passing a [~abi] paraameter.
       Just in case, we try to look for one procedure, and revert to default if it fails.
       In all other situations, we use the default FFI ABI. *)
    try
      ignore (foreign ?from ~abi:Libffi_abi.stdcall "glClear" (int @-> returning void)) ;
      Libffi_abi.stdcall
    with _ -> Libffi_abi.default_abi
  else Libffi_abi.default_abi

let foreign ?stub ?check_errno ?release_runtime_lock f fn =
  if Sys.win32 then
    (* In [opengl32.dll], non OpenGL 1.1 procedures must be looked up up via [wglGetProcAddress].
       To simplify things, we don't hardcode the list but do a two-step auto-detection.
       Some functions can only be resolved after OpenGL is initialized, so we delay the
       lookup until the first call and cache the lookup result.*)
    let cache = ref None in
    fun x -> 
      match !cache with
      | Some f -> f x
      | None ->
        try
          let fp = foreign ~abi ?from ~stub:false ?check_errno ?release_runtime_lock f fn in
          cache := Some fp;
          fp x
        with Dl.DL_error _ ->
          let ftyp = funptr_opt fn in
          match foreign ~abi ?from "wglGetProcAddress" (string @-> returning ftyp) f with
          | None -> failwith ("Could not resolve OpenGL procedure " ^ f)
          | Some fpp ->
            cache := Some fpp ;
            fpp x
  else foreign ~abi ?from ?stub ?check_errno ?release_runtime_lock f fn 

(* OpenGL ES 2 bindings *)

module Gl = struct

  (* Bigarrays *)

  type ('a, 'b) bigarray = ('a,'b, Bigarray.c_layout) Bigarray.Array1.t

  let ba_kind_byte_size : ('a, 'b) Bigarray.kind -> int = fun k ->
    let open Bigarray in
    (* FIXME: see http://caml.inria.fr/mantis/view.php?id=6263 *)
    match Obj.magic k with
    | k when k = char || k = int8_signed || k = int8_unsigned -> 1
    | k when k = int16_signed || k = int16_unsigned -> 2
    | k when k = int32 || k = float32 -> 4
    | k when k = float64 || k = int64 || k = complex32 -> 8
    | k when k = complex64 -> 16
    | k when k = int || k = nativeint -> Sys.word_size / 8
    | k -> assert false

 let bigarray_byte_size ba =
   let el_size = ba_kind_byte_size (Bigarray.Array1.kind ba) in
   el_size * Bigarray.Array1.dim ba

 let access_ptr_typ_of_ba_kind : ('a, 'b) Bigarray.kind -> 'a ptr typ =
   fun k ->
   let open Bigarray in
   (* FIXME: use typ_of_bigarray_kind when ctypes support it. *)
   match Obj.magic k with
   | k when k = float32 -> Obj.magic (ptr Ctypes.float)
   | k when k = float64 -> Obj.magic (ptr Ctypes.double)
   | k when k = complex32 -> Obj.magic (ptr Ctypes.complex32)
   | k when k = complex64 -> Obj.magic (ptr Ctypes.complex64)
   | k when k = int8_signed -> Obj.magic (ptr Ctypes.int8_t)
   | k when k = int8_unsigned -> Obj.magic (ptr Ctypes.uint8_t)
   | k when k = int16_signed -> Obj.magic (ptr Ctypes.int16_t)
   | k when k = int16_unsigned -> Obj.magic (ptr Ctypes.uint16_t)
   | k when k = int -> Obj.magic (ptr Ctypes.camlint)
   | k when k = int32 -> Obj.magic (ptr Ctypes.int32_t)
   | k when k = int64 -> Obj.magic (ptr Ctypes.int64_t)
   | k when k = nativeint -> Obj.magic (ptr Ctypes.nativeint)
   | k when k = char -> Obj.magic (ptr Ctypes.char)
   | _ -> assert false

 let string_of_bigarray ba =
   let len = Bigarray.Array1.dim ba in
   let b = Buffer.create (len - 1) in
   try
     for i = 0 to len - 1 do
       if ba.{i} = '\x00' then raise Exit else Buffer.add_char b ba.{i}
     done;
     raise Exit;
   with Exit -> Buffer.contents b

  (* Types *)

  let ba_as_charp =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  let ba_as_float32p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  let ba_as_uint8p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  let ba_as_int32p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  let ba_opt_as_int32p =
    view ~read:(fun _ -> assert false)
         ~write:(function
          | None -> null
          | Some b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  let ba_as_nativeint =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  type bitfield = int
  let int_as_uint =
    view ~read:Unsigned.UInt.to_int
         ~write:Unsigned.UInt.of_int
         uint
  
  let bool =
    view ~read:(fun u -> Unsigned.UChar.(compare u zero <> 0))
         ~write:(fun b -> Unsigned.UChar.(of_int (Stdlib.compare b false)))
         uchar
  
  type enum = int
  type enum_bigarray = (int32, Bigarray.int32_elt) bigarray
  let ba_as_enump =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  type uint32_bigarray = (int32, Bigarray.int32_elt) bigarray
  let ba_as_uint32p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  type debug_proc = enum -> enum -> int -> enum -> string -> unit
  
  (* Functions *)

  let stub = true (* If changed, will need updating Windows specific [foreign]. *)

  let active_texture =
    foreign ~stub "glActiveTexture" (int_as_uint @-> returning void)
  
  let attach_shader =
    foreign ~stub "glAttachShader"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_attrib_location =
    foreign ~stub "glBindAttribLocation"
      (int_as_uint @-> int_as_uint @-> string @-> returning void)
  
  let bind_buffer =
    foreign ~stub "glBindBuffer"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_framebuffer =
    foreign ~stub "glBindFramebuffer"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_renderbuffer =
    foreign ~stub "glBindRenderbuffer"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_texture =
    foreign ~stub "glBindTexture"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let blend_color =
    foreign ~stub "glBlendColor"
      (float @-> float @-> float @-> float @-> returning void)
  
  let blend_equation =
    foreign ~stub "glBlendEquation" (int_as_uint @-> returning void)
  
  let blend_equation_separate =
    foreign ~stub "glBlendEquationSeparate"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let blend_func =
    foreign ~stub "glBlendFunc"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let blend_func_separate =
    foreign ~stub "glBlendFuncSeparate"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let buffer_data =
    foreign ~stub "glBufferData"
      (int_as_uint @-> int @-> (ptr void) @-> int_as_uint @-> returning void)
  
  let buffer_data target size data usage =
    let data = match data with
    | None -> null | Some b -> to_voidp (bigarray_start array1 b)
    in
    buffer_data target size data usage
  
  let buffer_sub_data =
    foreign ~stub "glBufferSubData"
      (int_as_uint @-> int @-> int @-> (ptr void) @-> returning void)
  
  let buffer_sub_data target offset size data =
    let data = match data with
    | None -> null | Some b -> to_voidp (bigarray_start array1 b)
    in
    buffer_sub_data target offset size data
  
  let check_framebuffer_status =
    foreign ~stub "glCheckFramebufferStatus"
      (int_as_uint @-> returning int_as_uint)
  
  let clear =
    foreign ~stub "glClear" (int_as_uint @-> returning void)
  
  let clear_color =
    foreign ~stub "glClearColor"
      (float @-> float @-> float @-> float @-> returning void)
  
  let clear_depthf =
    foreign ~stub "glClearDepthf" (float @-> returning void)
  
  let clear_stencil =
    foreign ~stub "glClearStencil" (int @-> returning void)
  
  let color_mask =
    foreign ~stub "glColorMask"
      (bool @-> bool @-> bool @-> bool @-> returning void)
  
  let compile_shader =
    foreign ~stub "glCompileShader" (int_as_uint @-> returning void)
  
  let compressed_tex_image2d =
    foreign ~stub "glCompressedTexImage2D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       int @-> (ptr void) @-> returning void)
  
  let compressed_tex_image2d target level internalformat width height border
                             imageSize data =
    let data = match data with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    compressed_tex_image2d target level internalformat width height border
      imageSize data
  
  let compressed_tex_sub_image2d =
    foreign ~stub "glCompressedTexSubImage2D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @->
       int_as_uint @-> int @-> (ptr void) @-> returning void)
  
  let compressed_tex_sub_image2d target level xoffset yoffset width height
                                 format imageSize data =
    let data = match data with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    compressed_tex_sub_image2d target level xoffset yoffset width height
      format imageSize data
  
  let copy_tex_image2d =
    foreign ~stub "glCopyTexImage2D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       int @-> int @-> returning void)
  
  let copy_tex_sub_image2d =
    foreign ~stub "glCopyTexSubImage2D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @-> int @->
       int @-> returning void)
  
  let create_program =
    foreign ~stub "glCreateProgram" (void @-> returning int_as_uint)
  
  let create_shader =
    foreign ~stub "glCreateShader" (int_as_uint @-> returning int_as_uint)
  
  let cull_face =
    foreign ~stub "glCullFace" (int_as_uint @-> returning void)
  
  let delete_buffers =
    foreign ~stub "glDeleteBuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_framebuffers =
    foreign ~stub "glDeleteFramebuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_program =
    foreign ~stub "glDeleteProgram" (int_as_uint @-> returning void)
  
  let delete_renderbuffers =
    foreign ~stub "glDeleteRenderbuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_shader =
    foreign ~stub "glDeleteShader" (int_as_uint @-> returning void)
  
  let delete_textures =
    foreign ~stub "glDeleteTextures"
      (int @-> ba_as_uint32p @-> returning void)
  
  let depth_func =
    foreign ~stub "glDepthFunc" (int_as_uint @-> returning void)
  
  let depth_mask =
    foreign ~stub "glDepthMask" (bool @-> returning void)
  
  let depth_rangef =
    foreign ~stub "glDepthRangef" (float @-> float @-> returning void)
  
  let detach_shader =
    foreign ~stub "glDetachShader"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let disable =
    foreign ~stub "glDisable" (int_as_uint @-> returning void)
  
  let disable_vertex_attrib_array =
    foreign ~stub "glDisableVertexAttribArray"
      (int_as_uint @-> returning void)
  
  let draw_arrays =
    foreign ~stub "glDrawArrays"
      (int_as_uint @-> int @-> int @-> returning void)
  
  let draw_elements =
    foreign ~stub "glDrawElements"
      (int_as_uint @-> int @-> int_as_uint @-> (ptr void) @-> returning void)
  
  let draw_elements mode count type_ indices =
    let indices = match indices with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_elements mode count type_ indices
  
  let enable =
    foreign ~stub "glEnable" (int_as_uint @-> returning void)
  
  let enable_vertex_attrib_array =
    foreign ~stub "glEnableVertexAttribArray"
      (int_as_uint @-> returning void)
  
  let finish =
    foreign ~stub "glFinish" (void @-> returning void)
  
  let flush =
    foreign ~stub "glFlush" (void @-> returning void)
  
  let framebuffer_renderbuffer =
    foreign ~stub "glFramebufferRenderbuffer"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let framebuffer_texture2d =
    foreign ~stub "glFramebufferTexture2D"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int @-> returning void)
  
  let front_face =
    foreign ~stub "glFrontFace" (int_as_uint @-> returning void)
  
  let gen_buffers =
    foreign ~stub "glGenBuffers" (int @-> ba_as_uint32p @-> returning void)
  
  let gen_framebuffers =
    foreign ~stub "glGenFramebuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let gen_renderbuffers =
    foreign ~stub "glGenRenderbuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let gen_textures =
    foreign ~stub "glGenTextures" (int @-> ba_as_uint32p @-> returning void)
  
  let generate_mipmap =
    foreign ~stub "glGenerateMipmap" (int_as_uint @-> returning void)
  
  let get_active_attrib =
    foreign ~stub "glGetActiveAttrib"
      (int_as_uint @-> int_as_uint @-> int @-> ba_opt_as_int32p @->
       ba_as_int32p @-> ba_as_enump @-> ba_as_charp @-> returning void)
  
  let get_active_uniform =
    foreign ~stub "glGetActiveUniform"
      (int_as_uint @-> int_as_uint @-> int @-> ba_opt_as_int32p @->
       ba_as_int32p @-> ba_as_enump @-> ba_as_charp @-> returning void)
  
  let get_attached_shaders =
    foreign ~stub "glGetAttachedShaders"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_uint32p @->
       returning void)
  
  let get_attrib_location =
    foreign ~stub "glGetAttribLocation"
      (int_as_uint @-> string @-> returning int)
  
  let get_booleanv =
    foreign ~stub "glGetBooleanv"
      (int_as_uint @-> ba_as_uint8p @-> returning void)
  
  let get_buffer_parameteriv =
    foreign ~stub "glGetBufferParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_error =
    foreign ~stub "glGetError" (void @-> returning int_as_uint)
  
  let get_floatv =
    foreign ~stub "glGetFloatv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_framebuffer_attachment_parameteriv =
    foreign ~stub "glGetFramebufferAttachmentParameteriv"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> ba_as_int32p @->
       returning void)
  
  let get_integerv =
    foreign ~stub "glGetIntegerv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_program_info_log =
    foreign ~stub "glGetProgramInfoLog"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp @->
       returning void)
  
  let get_programiv =
    foreign ~stub "glGetProgramiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_renderbuffer_parameteriv =
    foreign ~stub "glGetRenderbufferParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_shader_info_log =
    foreign ~stub "glGetShaderInfoLog"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp @->
       returning void)
  
  let get_shader_precision_format =
    foreign ~stub "glGetShaderPrecisionFormat"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> ba_as_int32p @->
       returning void)
  
  let get_shader_source =
    foreign ~stub "glGetShaderSource"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp @->
       returning void)
  
  let get_shaderiv =
    foreign ~stub "glGetShaderiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_string =
    foreign ~stub "glGetString" (int_as_uint @-> returning string_opt)
  
  let get_tex_parameterfv =
    foreign ~stub "glGetTexParameterfv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_tex_parameteriv =
    foreign ~stub "glGetTexParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_uniform_location =
    foreign ~stub "glGetUniformLocation"
      (int_as_uint @-> string @-> returning int)
  
  let get_uniformfv =
    foreign ~stub "glGetUniformfv"
      (int_as_uint @-> int @-> ba_as_float32p @-> returning void)
  
  let get_uniformiv =
    foreign ~stub "glGetUniformiv"
      (int_as_uint @-> int @-> ba_as_int32p @-> returning void)
  
  let get_vertex_attrib_pointerv =
    foreign ~stub "glGetVertexAttribPointerv"
      (int_as_uint @-> int_as_uint @-> ba_as_nativeint @-> returning void)
  
  let get_vertex_attribfv =
    foreign ~stub "glGetVertexAttribfv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_vertex_attribiv =
    foreign ~stub "glGetVertexAttribiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let hint =
    foreign ~stub "glHint" (int_as_uint @-> int_as_uint @-> returning void)
  
  let is_buffer =
    foreign ~stub "glIsBuffer" (int_as_uint @-> returning bool)
  
  let is_enabled =
    foreign ~stub "glIsEnabled" (int_as_uint @-> returning bool)
  
  let is_framebuffer =
    foreign ~stub "glIsFramebuffer" (int_as_uint @-> returning bool)
  
  let is_program =
    foreign ~stub "glIsProgram" (int_as_uint @-> returning bool)
  
  let is_renderbuffer =
    foreign ~stub "glIsRenderbuffer" (int_as_uint @-> returning bool)
  
  let is_shader =
    foreign ~stub "glIsShader" (int_as_uint @-> returning bool)
  
  let is_texture =
    foreign ~stub "glIsTexture" (int_as_uint @-> returning bool)
  
  let line_width =
    foreign ~stub "glLineWidth" (float @-> returning void)
  
  let link_program =
    foreign ~stub "glLinkProgram" (int_as_uint @-> returning void)
  
  let pixel_storei =
    foreign ~stub "glPixelStorei" (int_as_uint @-> int @-> returning void)
  
  let polygon_offset =
    foreign ~stub "glPolygonOffset" (float @-> float @-> returning void)
  
  let read_pixels =
    foreign ~stub "glReadPixels"
      (int @-> int @-> int @-> int @-> int_as_uint @-> int_as_uint @->
       (ptr void) @-> returning void)
  
  let read_pixels x y width height format type_ pixels =
    let pixels = match pixels with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    read_pixels x y width height format type_ pixels
  
  let release_shader_compiler =
    foreign ~stub "glReleaseShaderCompiler" (void @-> returning void)
  
  let renderbuffer_storage =
    foreign ~stub "glRenderbufferStorage"
      (int_as_uint @-> int_as_uint @-> int @-> int @-> returning void)
  
  let sample_coverage =
    foreign ~stub "glSampleCoverage" (float @-> bool @-> returning void)
  
  let scissor =
    foreign ~stub "glScissor"
      (int @-> int @-> int @-> int @-> returning void)
  
  let shader_binary =
    foreign ~stub "glShaderBinary"
      (int @-> ba_as_uint32p @-> int_as_uint @-> (ptr void) @-> int @->
       returning void)
  
  let shader_binary count shaders binaryformat binary length =
    let binary = to_voidp (bigarray_start array1 binary) in
    shader_binary count shaders binaryformat binary length
  
  let shader_source =
    foreign ~stub "glShaderSource"
      (int_as_uint @-> int @-> ptr string @-> ptr void @-> returning void)
  
  let shader_source sh src =
    let src = allocate string src in
    shader_source sh 1 src null
  
  let stencil_func =
    foreign ~stub "glStencilFunc"
      (int_as_uint @-> int @-> int_as_uint @-> returning void)
  
  let stencil_func_separate =
    foreign ~stub "glStencilFuncSeparate"
      (int_as_uint @-> int_as_uint @-> int @-> int_as_uint @->
       returning void)
  
  let stencil_mask =
    foreign ~stub "glStencilMask" (int_as_uint @-> returning void)
  
  let stencil_mask_separate =
    foreign ~stub "glStencilMaskSeparate"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let stencil_op =
    foreign ~stub "glStencilOp"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let stencil_op_separate =
    foreign ~stub "glStencilOpSeparate"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let tex_image2d =
    foreign ~stub "glTexImage2D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @->
       int_as_uint @-> int_as_uint @-> (ptr void) @-> returning void)
  
  let tex_image2d target level internalformat width height border format
                  type_ pixels =
    let pixels = match pixels with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    tex_image2d target level internalformat width height border format type_
      pixels
  
  let tex_parameterf =
    foreign ~stub "glTexParameterf"
      (int_as_uint @-> int_as_uint @-> float @-> returning void)
  
  let tex_parameterfv =
    foreign ~stub "glTexParameterfv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let tex_parameteri =
    foreign ~stub "glTexParameteri"
      (int_as_uint @-> int_as_uint @-> int @-> returning void)
  
  let tex_parameteriv =
    foreign ~stub "glTexParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let tex_sub_image2d =
    foreign ~stub "glTexSubImage2D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @->
       int_as_uint @-> int_as_uint @-> (ptr void) @-> returning void)
  
  let tex_sub_image2d target level xoffset yoffset width height format type_
                      pixels =
    let pixels = match pixels with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    tex_sub_image2d target level xoffset yoffset width height format type_
      pixels
  
  let uniform1f =
    foreign ~stub "glUniform1f" (int @-> float @-> returning void)
  
  let uniform1fv =
    foreign ~stub "glUniform1fv"
      (int @-> int @-> ba_as_float32p @-> returning void)
  
  let uniform1i =
    foreign ~stub "glUniform1i" (int @-> int @-> returning void)
  
  let uniform1iv =
    foreign ~stub "glUniform1iv"
      (int @-> int @-> ba_as_int32p @-> returning void)
  
  let uniform2f =
    foreign ~stub "glUniform2f" (int @-> float @-> float @-> returning void)
  
  let uniform2fv =
    foreign ~stub "glUniform2fv"
      (int @-> int @-> ba_as_float32p @-> returning void)
  
  let uniform2i =
    foreign ~stub "glUniform2i" (int @-> int @-> int @-> returning void)
  
  let uniform2iv =
    foreign ~stub "glUniform2iv"
      (int @-> int @-> ba_as_int32p @-> returning void)
  
  let uniform3f =
    foreign ~stub "glUniform3f"
      (int @-> float @-> float @-> float @-> returning void)
  
  let uniform3fv =
    foreign ~stub "glUniform3fv"
      (int @-> int @-> ba_as_float32p @-> returning void)
  
  let uniform3i =
    foreign ~stub "glUniform3i"
      (int @-> int @-> int @-> int @-> returning void)
  
  let uniform3iv =
    foreign ~stub "glUniform3iv"
      (int @-> int @-> ba_as_int32p @-> returning void)
  
  let uniform4f =
    foreign ~stub "glUniform4f"
      (int @-> float @-> float @-> float @-> float @-> returning void)
  
  let uniform4fv =
    foreign ~stub "glUniform4fv"
      (int @-> int @-> ba_as_float32p @-> returning void)
  
  let uniform4i =
    foreign ~stub "glUniform4i"
      (int @-> int @-> int @-> int @-> int @-> returning void)
  
  let uniform4iv =
    foreign ~stub "glUniform4iv"
      (int @-> int @-> ba_as_int32p @-> returning void)
  
  let uniform_matrix2fv =
    foreign ~stub "glUniformMatrix2fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix3fv =
    foreign ~stub "glUniformMatrix3fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix4fv =
    foreign ~stub "glUniformMatrix4fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let use_program =
    foreign ~stub "glUseProgram" (int_as_uint @-> returning void)
  
  let validate_program =
    foreign ~stub "glValidateProgram" (int_as_uint @-> returning void)
  
  let vertex_attrib1f =
    foreign ~stub "glVertexAttrib1f"
      (int_as_uint @-> float @-> returning void)
  
  let vertex_attrib1fv =
    foreign ~stub "glVertexAttrib1fv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let vertex_attrib2f =
    foreign ~stub "glVertexAttrib2f"
      (int_as_uint @-> float @-> float @-> returning void)
  
  let vertex_attrib2fv =
    foreign ~stub "glVertexAttrib2fv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let vertex_attrib3f =
    foreign ~stub "glVertexAttrib3f"
      (int_as_uint @-> float @-> float @-> float @-> returning void)
  
  let vertex_attrib3fv =
    foreign ~stub "glVertexAttrib3fv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let vertex_attrib4f =
    foreign ~stub "glVertexAttrib4f"
      (int_as_uint @-> float @-> float @-> float @-> float @->
       returning void)
  
  let vertex_attrib4fv =
    foreign ~stub "glVertexAttrib4fv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let vertex_attrib_pointer =
    foreign ~stub "glVertexAttribPointer"
      (int_as_uint @-> int @-> int_as_uint @-> bool @-> int @->
       (ptr void) @-> returning void)
  
  let vertex_attrib_pointer index size type_ normalized stride pointer =
    let pointer = match pointer with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    vertex_attrib_pointer index size type_ normalized stride pointer
  
  let viewport =
    foreign ~stub "glViewport"
      (int @-> int @-> int @-> int @-> returning void)
  

  (* Enums *)

  let active_attributes = 0x8B89
  let active_attribute_max_length = 0x8B8A
  let active_texture_enum = 0x84E0
  let active_uniforms = 0x8B86
  let active_uniform_max_length = 0x8B87
  let aliased_line_width_range = 0x846E
  let aliased_point_size_range = 0x846D
  let alpha = 0x1906
  let alpha_bits = 0xD55
  let always = 0x207
  let array_buffer = 0x8892
  let array_buffer_binding = 0x8894
  let attached_shaders = 0x8B85
  let back = 0x405
  let blend = 0xBE2
  let blend_color_enum = 0x8005
  let blend_dst_alpha = 0x80CA
  let blend_dst_rgb = 0x80C8
  let blend_equation_enum = 0x8009
  let blend_equation_alpha = 0x883D
  let blend_equation_rgb = 0x8009
  let blend_src_alpha = 0x80CB
  let blend_src_rgb = 0x80C9
  let blue_bits = 0xD54
  let bool = 0x8B56
  let bool_vec2 = 0x8B57
  let bool_vec3 = 0x8B58
  let bool_vec4 = 0x8B59
  let buffer_size = 0x8764
  let buffer_usage = 0x8765
  let byte = 0x1400
  let ccw = 0x901
  let clamp_to_edge = 0x812F
  let color_attachment0 = 0x8CE0
  let color_buffer_bit = 0x4000
  let color_clear_value = 0xC22
  let color_writemask = 0xC23
  let compile_status = 0x8B81
  let compressed_texture_formats = 0x86A3
  let constant_alpha = 0x8003
  let constant_color = 0x8001
  let cull_face_enum = 0xB44
  let cull_face_mode = 0xB45
  let current_program = 0x8B8D
  let current_vertex_attrib = 0x8626
  let cw = 0x900
  let decr = 0x1E03
  let decr_wrap = 0x8508
  let delete_status = 0x8B80
  let depth_attachment = 0x8D00
  let depth_bits = 0xD56
  let depth_buffer_bit = 0x100
  let depth_clear_value = 0xB73
  let depth_component = 0x1902
  let depth_component16 = 0x81A5
  let depth_func_enum = 0xB74
  let depth_range = 0xB70
  let depth_test = 0xB71
  let depth_writemask = 0xB72
  let dither = 0xBD0
  let dont_care = 0x1100
  let dst_alpha = 0x304
  let dst_color = 0x306
  let dynamic_draw = 0x88E8
  let element_array_buffer = 0x8893
  let element_array_buffer_binding = 0x8895
  let equal = 0x202
  let extensions = 0x1F03
  let false_ = 0x0
  let fastest = 0x1101
  let fixed = 0x140C
  let float = 0x1406
  let float_mat2 = 0x8B5A
  let float_mat3 = 0x8B5B
  let float_mat4 = 0x8B5C
  let float_vec2 = 0x8B50
  let float_vec3 = 0x8B51
  let float_vec4 = 0x8B52
  let fragment_shader = 0x8B30
  let framebuffer = 0x8D40
  let framebuffer_attachment_object_name = 0x8CD1
  let framebuffer_attachment_object_type = 0x8CD0
  let framebuffer_attachment_texture_cube_map_face = 0x8CD3
  let framebuffer_attachment_texture_level = 0x8CD2
  let framebuffer_binding = 0x8CA6
  let framebuffer_complete = 0x8CD5
  let framebuffer_incomplete_attachment = 0x8CD6
  let framebuffer_incomplete_dimensions = 0x8CD9
  let framebuffer_incomplete_missing_attachment = 0x8CD7
  let framebuffer_unsupported = 0x8CDD
  let front = 0x404
  let front_and_back = 0x408
  let front_face_enum = 0xB46
  let func_add = 0x8006
  let func_reverse_subtract = 0x800B
  let func_subtract = 0x800A
  let generate_mipmap_hint = 0x8192
  let gequal = 0x206
  let greater = 0x204
  let green_bits = 0xD53
  let high_float = 0x8DF2
  let high_int = 0x8DF5
  let implementation_color_read_format = 0x8B9B
  let implementation_color_read_type = 0x8B9A
  let incr = 0x1E02
  let incr_wrap = 0x8507
  let info_log_length = 0x8B84
  let int = 0x1404
  let int_vec2 = 0x8B53
  let int_vec3 = 0x8B54
  let int_vec4 = 0x8B55
  let invalid_enum = 0x500
  let invalid_framebuffer_operation = 0x506
  let invalid_operation = 0x502
  let invalid_value = 0x501
  let invert = 0x150A
  let keep = 0x1E00
  let lequal = 0x203
  let less = 0x201
  let linear = 0x2601
  let linear_mipmap_linear = 0x2703
  let linear_mipmap_nearest = 0x2701
  let lines = 0x1
  let line_loop = 0x2
  let line_strip = 0x3
  let line_width_enum = 0xB21
  let link_status = 0x8B82
  let low_float = 0x8DF0
  let low_int = 0x8DF3
  let luminance = 0x1909
  let luminance_alpha = 0x190A
  let max_combined_texture_image_units = 0x8B4D
  let max_cube_map_texture_size = 0x851C
  let max_fragment_uniform_vectors = 0x8DFD
  let max_renderbuffer_size = 0x84E8
  let max_texture_image_units = 0x8872
  let max_texture_size = 0xD33
  let max_varying_vectors = 0x8DFC
  let max_vertex_attribs = 0x8869
  let max_vertex_texture_image_units = 0x8B4C
  let max_vertex_uniform_vectors = 0x8DFB
  let max_viewport_dims = 0xD3A
  let medium_float = 0x8DF1
  let medium_int = 0x8DF4
  let mirrored_repeat = 0x8370
  let nearest = 0x2600
  let nearest_mipmap_linear = 0x2702
  let nearest_mipmap_nearest = 0x2700
  let never = 0x200
  let nicest = 0x1102
  let none = 0x0
  let notequal = 0x205
  let no_error = 0x0
  let num_compressed_texture_formats = 0x86A2
  let num_shader_binary_formats = 0x8DF9
  let one = 0x1
  let one_minus_constant_alpha = 0x8004
  let one_minus_constant_color = 0x8002
  let one_minus_dst_alpha = 0x305
  let one_minus_dst_color = 0x307
  let one_minus_src_alpha = 0x303
  let one_minus_src_color = 0x301
  let out_of_memory = 0x505
  let pack_alignment = 0xD05
  let points = 0x0
  let polygon_offset_factor = 0x8038
  let polygon_offset_fill = 0x8037
  let polygon_offset_units = 0x2A00
  let red_bits = 0xD52
  let renderbuffer = 0x8D41
  let renderbuffer_alpha_size = 0x8D53
  let renderbuffer_binding = 0x8CA7
  let renderbuffer_blue_size = 0x8D52
  let renderbuffer_depth_size = 0x8D54
  let renderbuffer_green_size = 0x8D51
  let renderbuffer_height = 0x8D43
  let renderbuffer_internal_format = 0x8D44
  let renderbuffer_red_size = 0x8D50
  let renderbuffer_stencil_size = 0x8D55
  let renderbuffer_width = 0x8D42
  let renderer = 0x1F01
  let repeat = 0x2901
  let replace = 0x1E01
  let rgb = 0x1907
  let rgb565 = 0x8D62
  let rgb5_a1 = 0x8057
  let rgba = 0x1908
  let rgba4 = 0x8056
  let sampler_2d = 0x8B5E
  let sampler_cube = 0x8B60
  let samples = 0x80A9
  let sample_alpha_to_coverage = 0x809E
  let sample_buffers = 0x80A8
  let sample_coverage_enum = 0x80A0
  let sample_coverage_invert = 0x80AB
  let sample_coverage_value = 0x80AA
  let scissor_box = 0xC10
  let scissor_test = 0xC11
  let shader_binary_formats = 0x8DF8
  let shader_compiler = 0x8DFA
  let shader_source_length = 0x8B88
  let shader_type = 0x8B4F
  let shading_language_version = 0x8B8C
  let short = 0x1402
  let src_alpha = 0x302
  let src_alpha_saturate = 0x308
  let src_color = 0x300
  let static_draw = 0x88E4
  let stencil_attachment = 0x8D20
  let stencil_back_fail = 0x8801
  let stencil_back_func = 0x8800
  let stencil_back_pass_depth_fail = 0x8802
  let stencil_back_pass_depth_pass = 0x8803
  let stencil_back_ref = 0x8CA3
  let stencil_back_value_mask = 0x8CA4
  let stencil_back_writemask = 0x8CA5
  let stencil_bits = 0xD57
  let stencil_buffer_bit = 0x400
  let stencil_clear_value = 0xB91
  let stencil_fail = 0xB94
  let stencil_func_enum = 0xB92
  let stencil_index8 = 0x8D48
  let stencil_pass_depth_fail = 0xB95
  let stencil_pass_depth_pass = 0xB96
  let stencil_ref = 0xB97
  let stencil_test = 0xB90
  let stencil_value_mask = 0xB93
  let stencil_writemask = 0xB98
  let stream_draw = 0x88E0
  let subpixel_bits = 0xD50
  let texture = 0x1702
  let texture0 = 0x84C0
  let texture1 = 0x84C1
  let texture10 = 0x84CA
  let texture11 = 0x84CB
  let texture12 = 0x84CC
  let texture13 = 0x84CD
  let texture14 = 0x84CE
  let texture15 = 0x84CF
  let texture16 = 0x84D0
  let texture17 = 0x84D1
  let texture18 = 0x84D2
  let texture19 = 0x84D3
  let texture2 = 0x84C2
  let texture20 = 0x84D4
  let texture21 = 0x84D5
  let texture22 = 0x84D6
  let texture23 = 0x84D7
  let texture24 = 0x84D8
  let texture25 = 0x84D9
  let texture26 = 0x84DA
  let texture27 = 0x84DB
  let texture28 = 0x84DC
  let texture29 = 0x84DD
  let texture3 = 0x84C3
  let texture30 = 0x84DE
  let texture31 = 0x84DF
  let texture4 = 0x84C4
  let texture5 = 0x84C5
  let texture6 = 0x84C6
  let texture7 = 0x84C7
  let texture8 = 0x84C8
  let texture9 = 0x84C9
  let texture_2d = 0xDE1
  let texture_binding_2d = 0x8069
  let texture_binding_cube_map = 0x8514
  let texture_cube_map = 0x8513
  let texture_cube_map_negative_x = 0x8516
  let texture_cube_map_negative_y = 0x8518
  let texture_cube_map_negative_z = 0x851A
  let texture_cube_map_positive_x = 0x8515
  let texture_cube_map_positive_y = 0x8517
  let texture_cube_map_positive_z = 0x8519
  let texture_mag_filter = 0x2800
  let texture_min_filter = 0x2801
  let texture_wrap_s = 0x2802
  let texture_wrap_t = 0x2803
  let triangles = 0x4
  let triangle_fan = 0x6
  let triangle_strip = 0x5
  let true_ = 0x1
  let unpack_alignment = 0xCF5
  let unsigned_byte = 0x1401
  let unsigned_int = 0x1405
  let unsigned_short = 0x1403
  let unsigned_short_4_4_4_4 = 0x8033
  let unsigned_short_5_5_5_1 = 0x8034
  let unsigned_short_5_6_5 = 0x8363
  let validate_status = 0x8B83
  let vendor = 0x1F00
  let version = 0x1F02
  let vertex_attrib_array_buffer_binding = 0x889F
  let vertex_attrib_array_enabled = 0x8622
  let vertex_attrib_array_normalized = 0x886A
  let vertex_attrib_array_pointer = 0x8645
  let vertex_attrib_array_size = 0x8623
  let vertex_attrib_array_stride = 0x8624
  let vertex_attrib_array_type = 0x8625
  let vertex_shader = 0x8B31
  let viewport_enum = 0xBA2
  let zero = 0x0
end
