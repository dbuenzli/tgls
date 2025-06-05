(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated with:
   apiquery -ml -api gl3.3 *)

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

(* OpenGL 3.x bindings *)

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
  
  let ba_as_float64p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  let ba_as_uint16p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  let ba_as_int8p =
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
  
  let ba_as_int64p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
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
  
  type int16 = int
  type sync = unit ptr
  let sync : sync typ = ptr void
  let sync_opt : sync option typ = ptr_opt void
  
  type uint32_bigarray = (int32, Bigarray.int32_elt) bigarray
  let ba_as_uint32p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  type uint64 = int64
  let int64_as_uint64_t =
    view ~read:Unsigned.UInt64.to_int64
         ~write:Unsigned.UInt64.of_int64
         uint64_t
  
  type uint64_bigarray = (int64, Bigarray.int64_elt) bigarray
  let ba_as_uint64p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  type uint8 = int
  let int_as_uint8_t =
    view ~read:Unsigned.UInt8.to_int
         ~write:Unsigned.UInt8.of_int
         uint8_t
  
  type debug_proc = enum -> enum -> int -> enum -> string -> unit
  
  (* Functions *)

  let stub = true (* If changed, will need updating Windows specific [foreign]. *)

  let active_texture =
    foreign ~stub "glActiveTexture" (int_as_uint @-> returning void)
  
  let attach_shader =
    foreign ~stub "glAttachShader"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let begin_conditional_render =
    foreign ~stub "glBeginConditionalRender"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let begin_query =
    foreign ~stub "glBeginQuery"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let begin_transform_feedback =
    foreign ~stub "glBeginTransformFeedback" (int_as_uint @-> returning void)
  
  let bind_attrib_location =
    foreign ~stub "glBindAttribLocation"
      (int_as_uint @-> int_as_uint @-> string @-> returning void)
  
  let bind_buffer =
    foreign ~stub "glBindBuffer"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_buffer_base =
    foreign ~stub "glBindBufferBase"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_buffer_range =
    foreign ~stub "glBindBufferRange"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @-> int @->
       returning void)
  
  let bind_frag_data_location =
    foreign ~stub "glBindFragDataLocation"
      (int_as_uint @-> int_as_uint @-> string @-> returning void)
  
  let bind_frag_data_location_indexed =
    foreign ~stub "glBindFragDataLocationIndexed"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> string @->
       returning void)
  
  let bind_framebuffer =
    foreign ~stub "glBindFramebuffer"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_renderbuffer =
    foreign ~stub "glBindRenderbuffer"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_sampler =
    foreign ~stub "glBindSampler"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_texture =
    foreign ~stub "glBindTexture"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_vertex_array =
    foreign ~stub "glBindVertexArray" (int_as_uint @-> returning void)
  
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
  
  let blit_framebuffer =
    foreign ~stub "glBlitFramebuffer"
      (int @-> int @-> int @-> int @-> int @-> int @-> int @-> int @->
       int_as_uint @-> int_as_uint @-> returning void)
  
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
  
  let clamp_color =
    foreign ~stub "glClampColor"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let clear =
    foreign ~stub "glClear" (int_as_uint @-> returning void)
  
  let clear_bufferfi =
    foreign ~stub "glClearBufferfi"
      (int_as_uint @-> int @-> float @-> int @-> returning void)
  
  let clear_bufferfv =
    foreign ~stub "glClearBufferfv"
      (int_as_uint @-> int @-> ba_as_float32p @-> returning void)
  
  let clear_bufferiv =
    foreign ~stub "glClearBufferiv"
      (int_as_uint @-> int @-> ba_as_int32p @-> returning void)
  
  let clear_bufferuiv =
    foreign ~stub "glClearBufferuiv"
      (int_as_uint @-> int @-> ba_as_uint32p @-> returning void)
  
  let clear_color =
    foreign ~stub "glClearColor"
      (float @-> float @-> float @-> float @-> returning void)
  
  let clear_depth =
    foreign ~stub "glClearDepth" (double @-> returning void)
  
  let clear_stencil =
    foreign ~stub "glClearStencil" (int @-> returning void)
  
  let client_wait_sync =
    foreign ~stub "glClientWaitSync"
      (sync @-> int_as_uint @-> int64_as_uint64_t @-> returning int_as_uint)
  
  let color_mask =
    foreign ~stub "glColorMask"
      (bool @-> bool @-> bool @-> bool @-> returning void)
  
  let color_maski =
    foreign ~stub "glColorMaski"
      (int_as_uint @-> bool @-> bool @-> bool @-> bool @-> returning void)
  
  let compile_shader =
    foreign ~stub "glCompileShader" (int_as_uint @-> returning void)
  
  let compressed_tex_image1d =
    foreign ~stub "glCompressedTexImage1D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       (ptr void) @-> returning void)
  
  let compressed_tex_image1d target level internalformat width border
                             imageSize data =
    let data = match data with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    compressed_tex_image1d target level internalformat width border imageSize
      data
  
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
  
  let compressed_tex_image3d =
    foreign ~stub "glCompressedTexImage3D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       int @-> int @-> (ptr void) @-> returning void)
  
  let compressed_tex_image3d target level internalformat width height depth
                             border imageSize data =
    let data = match data with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    compressed_tex_image3d target level internalformat width height depth
      border imageSize data
  
  let compressed_tex_sub_image1d =
    foreign ~stub "glCompressedTexSubImage1D"
      (int_as_uint @-> int @-> int @-> int @-> int_as_uint @-> int @->
       (ptr void) @-> returning void)
  
  let compressed_tex_sub_image1d target level xoffset width format imageSize
                                 data =
    let data = match data with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    compressed_tex_sub_image1d target level xoffset width format imageSize
      data
  
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
  
  let compressed_tex_sub_image3d =
    foreign ~stub "glCompressedTexSubImage3D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @-> int @->
       int @-> int_as_uint @-> int @-> (ptr void) @-> returning void)
  
  let compressed_tex_sub_image3d target level xoffset yoffset zoffset width
                                 height depth format imageSize data =
    let data = match data with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    compressed_tex_sub_image3d target level xoffset yoffset zoffset width
      height depth format imageSize data
  
  let copy_buffer_sub_data =
    foreign ~stub "glCopyBufferSubData"
      (int_as_uint @-> int_as_uint @-> int @-> int @-> int @->
       returning void)
  
  let copy_tex_image1d =
    foreign ~stub "glCopyTexImage1D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       int @-> returning void)
  
  let copy_tex_image2d =
    foreign ~stub "glCopyTexImage2D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       int @-> int @-> returning void)
  
  let copy_tex_sub_image1d =
    foreign ~stub "glCopyTexSubImage1D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @->
       returning void)
  
  let copy_tex_sub_image2d =
    foreign ~stub "glCopyTexSubImage2D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @-> int @->
       int @-> returning void)
  
  let copy_tex_sub_image3d =
    foreign ~stub "glCopyTexSubImage3D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @-> int @->
       int @-> int @-> returning void)
  
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
  
  let delete_queries =
    foreign ~stub "glDeleteQueries"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_renderbuffers =
    foreign ~stub "glDeleteRenderbuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_samplers =
    foreign ~stub "glDeleteSamplers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_shader =
    foreign ~stub "glDeleteShader" (int_as_uint @-> returning void)
  
  let delete_sync =
    foreign ~stub "glDeleteSync" (sync @-> returning void)
  
  let delete_textures =
    foreign ~stub "glDeleteTextures"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_vertex_arrays =
    foreign ~stub "glDeleteVertexArrays"
      (int @-> ba_as_uint32p @-> returning void)
  
  let depth_func =
    foreign ~stub "glDepthFunc" (int_as_uint @-> returning void)
  
  let depth_mask =
    foreign ~stub "glDepthMask" (bool @-> returning void)
  
  let depth_range =
    foreign ~stub "glDepthRange" (double @-> double @-> returning void)
  
  let detach_shader =
    foreign ~stub "glDetachShader"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let disable =
    foreign ~stub "glDisable" (int_as_uint @-> returning void)
  
  let disable_vertex_attrib_array =
    foreign ~stub "glDisableVertexAttribArray"
      (int_as_uint @-> returning void)
  
  let disablei =
    foreign ~stub "glDisablei"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let draw_arrays =
    foreign ~stub "glDrawArrays"
      (int_as_uint @-> int @-> int @-> returning void)
  
  let draw_arrays_instanced =
    foreign ~stub "glDrawArraysInstanced"
      (int_as_uint @-> int @-> int @-> int @-> returning void)
  
  let draw_buffer =
    foreign ~stub "glDrawBuffer" (int_as_uint @-> returning void)
  
  let draw_buffers =
    foreign ~stub "glDrawBuffers" (int @-> ba_as_enump @-> returning void)
  
  let draw_elements =
    foreign ~stub "glDrawElements"
      (int_as_uint @-> int @-> int_as_uint @-> (ptr void) @-> returning void)
  
  let draw_elements mode count type_ indices =
    let indices = match indices with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_elements mode count type_ indices
  
  let draw_elements_base_vertex =
    foreign ~stub "glDrawElementsBaseVertex"
      (int_as_uint @-> int @-> int_as_uint @-> (ptr void) @-> int @->
       returning void)
  
  let draw_elements_base_vertex mode count type_ indices basevertex =
    let indices = match indices with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_elements_base_vertex mode count type_ indices basevertex
  
  let draw_elements_instanced =
    foreign ~stub "glDrawElementsInstanced"
      (int_as_uint @-> int @-> int_as_uint @-> (ptr void) @-> int @->
       returning void)
  
  let draw_elements_instanced mode count type_ indices instancecount =
    let indices = match indices with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_elements_instanced mode count type_ indices instancecount
  
  let draw_elements_instanced_base_vertex =
    foreign ~stub "glDrawElementsInstancedBaseVertex"
      (int_as_uint @-> int @-> int_as_uint @-> (ptr void) @-> int @-> int @->
       returning void)
  
  let draw_elements_instanced_base_vertex mode count type_ indices
                                          instancecount basevertex =
    let indices = match indices with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_elements_instanced_base_vertex mode count type_ indices
      instancecount basevertex
  
  let draw_range_elements =
    foreign ~stub "glDrawRangeElements"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @->
       int_as_uint @-> (ptr void) @-> returning void)
  
  let draw_range_elements mode start end_ count type_ indices =
    let indices = match indices with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_range_elements mode start end_ count type_ indices
  
  let draw_range_elements_base_vertex =
    foreign ~stub "glDrawRangeElementsBaseVertex"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @->
       int_as_uint @-> (ptr void) @-> int @-> returning void)
  
  let draw_range_elements_base_vertex mode start end_ count type_ indices
                                      basevertex =
    let indices = match indices with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_range_elements_base_vertex mode start end_ count type_ indices
      basevertex
  
  let enable =
    foreign ~stub "glEnable" (int_as_uint @-> returning void)
  
  let enable_vertex_attrib_array =
    foreign ~stub "glEnableVertexAttribArray"
      (int_as_uint @-> returning void)
  
  let enablei =
    foreign ~stub "glEnablei"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let end_conditional_render =
    foreign ~stub "glEndConditionalRender" (void @-> returning void)
  
  let end_query =
    foreign ~stub "glEndQuery" (int_as_uint @-> returning void)
  
  let end_transform_feedback =
    foreign ~stub "glEndTransformFeedback" (void @-> returning void)
  
  let fence_sync =
    foreign ~stub "glFenceSync"
      (int_as_uint @-> int_as_uint @-> returning sync)
  
  let finish =
    foreign ~stub "glFinish" (void @-> returning void)
  
  let flush =
    foreign ~stub "glFlush" (void @-> returning void)
  
  let flush_mapped_buffer_range =
    foreign ~stub "glFlushMappedBufferRange"
      (int_as_uint @-> int @-> int @-> returning void)
  
  let framebuffer_renderbuffer =
    foreign ~stub "glFramebufferRenderbuffer"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let framebuffer_texture =
    foreign ~stub "glFramebufferTexture"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @->
       returning void)
  
  let framebuffer_texture1d =
    foreign ~stub "glFramebufferTexture1D"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int @-> returning void)
  
  let framebuffer_texture2d =
    foreign ~stub "glFramebufferTexture2D"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int @-> returning void)
  
  let framebuffer_texture3d =
    foreign ~stub "glFramebufferTexture3D"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int @-> int @-> returning void)
  
  let framebuffer_texture_layer =
    foreign ~stub "glFramebufferTextureLayer"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @-> int @->
       returning void)
  
  let front_face =
    foreign ~stub "glFrontFace" (int_as_uint @-> returning void)
  
  let gen_buffers =
    foreign ~stub "glGenBuffers" (int @-> ba_as_uint32p @-> returning void)
  
  let gen_framebuffers =
    foreign ~stub "glGenFramebuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let gen_queries =
    foreign ~stub "glGenQueries" (int @-> ba_as_uint32p @-> returning void)
  
  let gen_renderbuffers =
    foreign ~stub "glGenRenderbuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let gen_samplers =
    foreign ~stub "glGenSamplers" (int @-> ba_as_uint32p @-> returning void)
  
  let gen_textures =
    foreign ~stub "glGenTextures" (int @-> ba_as_uint32p @-> returning void)
  
  let gen_vertex_arrays =
    foreign ~stub "glGenVertexArrays"
      (int @-> ba_as_uint32p @-> returning void)
  
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
  
  let get_active_uniform_block_name =
    foreign ~stub "glGetActiveUniformBlockName"
      (int_as_uint @-> int_as_uint @-> int @-> ba_opt_as_int32p @->
       ba_as_charp @-> returning void)
  
  let get_active_uniform_blockiv =
    foreign ~stub "glGetActiveUniformBlockiv"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> ba_as_int32p @->
       returning void)
  
  let get_active_uniform_name =
    foreign ~stub "glGetActiveUniformName"
      (int_as_uint @-> int_as_uint @-> int @-> ba_opt_as_int32p @->
       ba_as_charp @-> returning void)
  
  let get_active_uniformsiv =
    foreign ~stub "glGetActiveUniformsiv"
      (int_as_uint @-> int @-> ba_as_uint32p @-> int_as_uint @->
       ba_as_int32p @-> returning void)
  
  let get_attached_shaders =
    foreign ~stub "glGetAttachedShaders"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_uint32p @->
       returning void)
  
  let get_attrib_location =
    foreign ~stub "glGetAttribLocation"
      (int_as_uint @-> string @-> returning int)
  
  let get_booleani_v =
    foreign ~stub "glGetBooleani_v"
      (int_as_uint @-> int_as_uint @-> ba_as_uint8p @-> returning void)
  
  let get_booleanv =
    foreign ~stub "glGetBooleanv"
      (int_as_uint @-> ba_as_uint8p @-> returning void)
  
  let get_buffer_parameteri64v =
    foreign ~stub "glGetBufferParameteri64v"
      (int_as_uint @-> int_as_uint @-> ba_as_int64p @-> returning void)
  
  let get_buffer_parameteriv =
    foreign ~stub "glGetBufferParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_buffer_pointerv =
    foreign ~stub "glGetBufferPointerv"
      (int_as_uint @-> int_as_uint @-> ba_as_nativeint @-> returning void)
  
  let get_buffer_sub_data =
    foreign ~stub "glGetBufferSubData"
      (int_as_uint @-> int @-> int @-> (ptr void) @-> returning void)
  
  let get_buffer_sub_data target offset size data =
    let data = to_voidp (bigarray_start array1 data) in
    get_buffer_sub_data target offset size data
  
  let get_compressed_tex_image =
    foreign ~stub "glGetCompressedTexImage"
      (int_as_uint @-> int @-> (ptr void) @-> returning void)
  
  let get_compressed_tex_image target level img =
    let img = match img with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    get_compressed_tex_image target level img
  
  let get_doublev =
    foreign ~stub "glGetDoublev"
      (int_as_uint @-> ba_as_float64p @-> returning void)
  
  let get_error =
    foreign ~stub "glGetError" (void @-> returning int_as_uint)
  
  let get_floatv =
    foreign ~stub "glGetFloatv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_frag_data_index =
    foreign ~stub "glGetFragDataIndex"
      (int_as_uint @-> string @-> returning int)
  
  let get_frag_data_location =
    foreign ~stub "glGetFragDataLocation"
      (int_as_uint @-> string @-> returning int)
  
  let get_framebuffer_attachment_parameteriv =
    foreign ~stub "glGetFramebufferAttachmentParameteriv"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> ba_as_int32p @->
       returning void)
  
  let get_integer64i_v =
    foreign ~stub "glGetInteger64i_v"
      (int_as_uint @-> int_as_uint @-> ba_as_int64p @-> returning void)
  
  let get_integer64v =
    foreign ~stub "glGetInteger64v"
      (int_as_uint @-> ba_as_int64p @-> returning void)
  
  let get_integeri_v =
    foreign ~stub "glGetIntegeri_v"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_integerv =
    foreign ~stub "glGetIntegerv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_multisamplefv =
    foreign ~stub "glGetMultisamplefv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_program_info_log =
    foreign ~stub "glGetProgramInfoLog"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp @->
       returning void)
  
  let get_programiv =
    foreign ~stub "glGetProgramiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_query_objecti64v =
    foreign ~stub "glGetQueryObjecti64v"
      (int_as_uint @-> int_as_uint @-> ba_as_int64p @-> returning void)
  
  let get_query_objectiv =
    foreign ~stub "glGetQueryObjectiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_query_objectui64v =
    foreign ~stub "glGetQueryObjectui64v"
      (int_as_uint @-> int_as_uint @-> ba_as_uint64p @-> returning void)
  
  let get_query_objectuiv =
    foreign ~stub "glGetQueryObjectuiv"
      (int_as_uint @-> int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let get_queryiv =
    foreign ~stub "glGetQueryiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_renderbuffer_parameteriv =
    foreign ~stub "glGetRenderbufferParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_sampler_parameter_iiv =
    foreign ~stub "glGetSamplerParameterIiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_sampler_parameter_iuiv =
    foreign ~stub "glGetSamplerParameterIuiv"
      (int_as_uint @-> int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let get_sampler_parameterfv =
    foreign ~stub "glGetSamplerParameterfv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_sampler_parameteriv =
    foreign ~stub "glGetSamplerParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_shader_info_log =
    foreign ~stub "glGetShaderInfoLog"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp @->
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
  
  let get_stringi =
    foreign ~stub "glGetStringi"
      (int_as_uint @-> int_as_uint @-> returning string_opt)
  
  let get_synciv =
    foreign ~stub "glGetSynciv"
      (sync @-> int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_int32p @->
       returning void)
  
  let get_tex_image =
    foreign ~stub "glGetTexImage"
      (int_as_uint @-> int @-> int_as_uint @-> int_as_uint @-> (ptr void) @->
       returning void)
  
  let get_tex_image target level format type_ pixels =
    let pixels = to_voidp (bigarray_start array1 pixels) in
    get_tex_image target level format type_ pixels
  
  let get_tex_level_parameterfv =
    foreign ~stub "glGetTexLevelParameterfv"
      (int_as_uint @-> int @-> int_as_uint @-> ba_as_float32p @->
       returning void)
  
  let get_tex_level_parameteriv =
    foreign ~stub "glGetTexLevelParameteriv"
      (int_as_uint @-> int @-> int_as_uint @-> ba_as_int32p @->
       returning void)
  
  let get_tex_parameter_iiv =
    foreign ~stub "glGetTexParameterIiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_tex_parameter_iuiv =
    foreign ~stub "glGetTexParameterIuiv"
      (int_as_uint @-> int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let get_tex_parameterfv =
    foreign ~stub "glGetTexParameterfv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_tex_parameteriv =
    foreign ~stub "glGetTexParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_transform_feedback_varying =
    foreign ~stub "glGetTransformFeedbackVarying"
      (int_as_uint @-> int_as_uint @-> int @-> ba_opt_as_int32p @->
       ba_as_int32p @-> ba_as_enump @-> ba_as_charp @-> returning void)
  
  let get_uniform_block_index =
    foreign ~stub "glGetUniformBlockIndex"
      (int_as_uint @-> string @-> returning int_as_uint)
  
  let get_uniform_indices =
    foreign ~stub "glGetUniformIndices"
      (int_as_uint @-> int @-> ptr string @-> ptr void @-> returning void)
  
  let get_uniform_indices program names indices =
    let count = List.length names in
    let names = CArray.(start (of_list string names)) in
    let indices = to_voidp (bigarray_start array1 indices) in
    get_uniform_indices program count names indices
  
  let get_uniform_location =
    foreign ~stub "glGetUniformLocation"
      (int_as_uint @-> string @-> returning int)
  
  let get_uniformfv =
    foreign ~stub "glGetUniformfv"
      (int_as_uint @-> int @-> ba_as_float32p @-> returning void)
  
  let get_uniformiv =
    foreign ~stub "glGetUniformiv"
      (int_as_uint @-> int @-> ba_as_int32p @-> returning void)
  
  let get_uniformuiv =
    foreign ~stub "glGetUniformuiv"
      (int_as_uint @-> int @-> ba_as_uint32p @-> returning void)
  
  let get_vertex_attrib_iiv =
    foreign ~stub "glGetVertexAttribIiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_vertex_attrib_iuiv =
    foreign ~stub "glGetVertexAttribIuiv"
      (int_as_uint @-> int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let get_vertex_attrib_pointerv =
    foreign ~stub "glGetVertexAttribPointerv"
      (int_as_uint @-> int_as_uint @-> ba_as_nativeint @-> returning void)
  
  let get_vertex_attribdv =
    foreign ~stub "glGetVertexAttribdv"
      (int_as_uint @-> int_as_uint @-> ba_as_float64p @-> returning void)
  
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
  
  let is_enabledi =
    foreign ~stub "glIsEnabledi"
      (int_as_uint @-> int_as_uint @-> returning bool)
  
  let is_framebuffer =
    foreign ~stub "glIsFramebuffer" (int_as_uint @-> returning bool)
  
  let is_program =
    foreign ~stub "glIsProgram" (int_as_uint @-> returning bool)
  
  let is_query =
    foreign ~stub "glIsQuery" (int_as_uint @-> returning bool)
  
  let is_renderbuffer =
    foreign ~stub "glIsRenderbuffer" (int_as_uint @-> returning bool)
  
  let is_sampler =
    foreign ~stub "glIsSampler" (int_as_uint @-> returning bool)
  
  let is_shader =
    foreign ~stub "glIsShader" (int_as_uint @-> returning bool)
  
  let is_sync =
    foreign ~stub "glIsSync" (sync @-> returning bool)
  
  let is_texture =
    foreign ~stub "glIsTexture" (int_as_uint @-> returning bool)
  
  let is_vertex_array =
    foreign ~stub "glIsVertexArray" (int_as_uint @-> returning bool)
  
  let line_width =
    foreign ~stub "glLineWidth" (float @-> returning void)
  
  let link_program =
    foreign ~stub "glLinkProgram" (int_as_uint @-> returning void)
  
  let logic_op =
    foreign ~stub "glLogicOp" (int_as_uint @-> returning void)
  
  let map_buffer =
    foreign ~stub "glMapBuffer"
      (int_as_uint @-> int_as_uint @-> returning (ptr void))
  
  let map_buffer target len access kind =
    let p = map_buffer target access in
    let p = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
    bigarray_of_ptr array1 len kind p
  
  let map_buffer_range =
    foreign ~stub "glMapBufferRange"
      (int_as_uint @-> int @-> int @-> int_as_uint @-> returning (ptr void))
  
  let map_buffer_range target offset len access kind =
    let len_bytes = ba_kind_byte_size kind * len in
    let p = map_buffer_range target offset len_bytes access in
    let p = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
    bigarray_of_ptr array1 len kind p
  
  let multi_draw_arrays =
    foreign ~stub "glMultiDrawArrays"
      (int_as_uint @-> ba_as_int32p @-> ba_as_int32p @-> int @->
       returning void)
  
  let multi_draw_elements =
    foreign ~stub "glMultiDrawElements"
      (int_as_uint @-> ba_as_int32p @-> int_as_uint @-> (ptr void) @->
       int @-> returning void)
  
  let multi_draw_elements mode count type_ indices drawcount =
    let indices = to_voidp (bigarray_start array1 indices) in
    multi_draw_elements mode count type_ indices drawcount
  
  let multi_draw_elements_base_vertex =
    foreign ~stub "glMultiDrawElementsBaseVertex"
      (int_as_uint @-> ba_as_int32p @-> int_as_uint @-> (ptr void) @->
       int @-> ba_as_int32p @-> returning void)
  
  let multi_draw_elements_base_vertex mode count type_ indices drawcount
                                      basevertex =
    let indices = to_voidp (bigarray_start array1 indices) in
    multi_draw_elements_base_vertex mode count type_ indices drawcount
      basevertex
  
  let pixel_storef =
    foreign ~stub "glPixelStoref" (int_as_uint @-> float @-> returning void)
  
  let pixel_storei =
    foreign ~stub "glPixelStorei" (int_as_uint @-> int @-> returning void)
  
  let point_parameterf =
    foreign ~stub "glPointParameterf"
      (int_as_uint @-> float @-> returning void)
  
  let point_parameterfv =
    foreign ~stub "glPointParameterfv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let point_parameteri =
    foreign ~stub "glPointParameteri"
      (int_as_uint @-> int @-> returning void)
  
  let point_parameteriv =
    foreign ~stub "glPointParameteriv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let point_size =
    foreign ~stub "glPointSize" (float @-> returning void)
  
  let polygon_mode =
    foreign ~stub "glPolygonMode"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let polygon_offset =
    foreign ~stub "glPolygonOffset" (float @-> float @-> returning void)
  
  let primitive_restart_index =
    foreign ~stub "glPrimitiveRestartIndex" (int_as_uint @-> returning void)
  
  let provoking_vertex =
    foreign ~stub "glProvokingVertex" (int_as_uint @-> returning void)
  
  let query_counter =
    foreign ~stub "glQueryCounter"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let read_buffer =
    foreign ~stub "glReadBuffer" (int_as_uint @-> returning void)
  
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
  
  let renderbuffer_storage =
    foreign ~stub "glRenderbufferStorage"
      (int_as_uint @-> int_as_uint @-> int @-> int @-> returning void)
  
  let renderbuffer_storage_multisample =
    foreign ~stub "glRenderbufferStorageMultisample"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @->
       returning void)
  
  let sample_coverage =
    foreign ~stub "glSampleCoverage" (float @-> bool @-> returning void)
  
  let sample_maski =
    foreign ~stub "glSampleMaski"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let sampler_parameter_iiv =
    foreign ~stub "glSamplerParameterIiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let sampler_parameter_iuiv =
    foreign ~stub "glSamplerParameterIuiv"
      (int_as_uint @-> int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let sampler_parameterf =
    foreign ~stub "glSamplerParameterf"
      (int_as_uint @-> int_as_uint @-> float @-> returning void)
  
  let sampler_parameterfv =
    foreign ~stub "glSamplerParameterfv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let sampler_parameteri =
    foreign ~stub "glSamplerParameteri"
      (int_as_uint @-> int_as_uint @-> int @-> returning void)
  
  let sampler_parameteriv =
    foreign ~stub "glSamplerParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let scissor =
    foreign ~stub "glScissor"
      (int @-> int @-> int @-> int @-> returning void)
  
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
  
  let tex_buffer =
    foreign ~stub "glTexBuffer"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let tex_image1d =
    foreign ~stub "glTexImage1D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int_as_uint @->
       int_as_uint @-> (ptr void) @-> returning void)
  
  let tex_image1d target level internalformat width border format type_
                  pixels =
    let pixels = match pixels with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    tex_image1d target level internalformat width border format type_ pixels
  
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
  
  let tex_image2d_multisample =
    foreign ~stub "glTexImage2DMultisample"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> bool @->
       returning void)
  
  let tex_image3d =
    foreign ~stub "glTexImage3D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @-> int @->
       int_as_uint @-> int_as_uint @-> (ptr void) @-> returning void)
  
  let tex_image3d target level internalformat width height depth border
                  format type_ pixels =
    let pixels = match pixels with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    tex_image3d target level internalformat width height depth border format
      type_ pixels
  
  let tex_image3d_multisample =
    foreign ~stub "glTexImage3DMultisample"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       bool @-> returning void)
  
  let tex_parameter_iiv =
    foreign ~stub "glTexParameterIiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let tex_parameter_iuiv =
    foreign ~stub "glTexParameterIuiv"
      (int_as_uint @-> int_as_uint @-> ba_as_uint32p @-> returning void)
  
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
  
  let tex_sub_image1d =
    foreign ~stub "glTexSubImage1D"
      (int_as_uint @-> int @-> int @-> int @-> int_as_uint @->
       int_as_uint @-> (ptr void) @-> returning void)
  
  let tex_sub_image1d target level xoffset width format type_ pixels =
    let pixels = match pixels with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    tex_sub_image1d target level xoffset width format type_ pixels
  
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
  
  let tex_sub_image3d =
    foreign ~stub "glTexSubImage3D"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @-> int @->
       int @-> int_as_uint @-> int_as_uint @-> (ptr void) @-> returning void)
  
  let tex_sub_image3d target level xoffset yoffset zoffset width height depth
                      format type_ pixels =
    let pixels = match pixels with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    tex_sub_image3d target level xoffset yoffset zoffset width height depth
      format type_ pixels
  
  let transform_feedback_varyings =
    foreign ~stub "glTransformFeedbackVaryings"
      (int_as_uint @-> int @-> ptr string @-> int_as_uint @-> returning void)
  
  let transform_feedback_varyings program varyings mode =
    let count = List.length varyings in
    let varyings = CArray.(start (of_list string varyings)) in
    transform_feedback_varyings program count varyings mode
  
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
  
  let uniform1ui =
    foreign ~stub "glUniform1ui" (int @-> int_as_uint @-> returning void)
  
  let uniform1uiv =
    foreign ~stub "glUniform1uiv"
      (int @-> int @-> ba_as_uint32p @-> returning void)
  
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
  
  let uniform2ui =
    foreign ~stub "glUniform2ui"
      (int @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let uniform2uiv =
    foreign ~stub "glUniform2uiv"
      (int @-> int @-> ba_as_uint32p @-> returning void)
  
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
  
  let uniform3ui =
    foreign ~stub "glUniform3ui"
      (int @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let uniform3uiv =
    foreign ~stub "glUniform3uiv"
      (int @-> int @-> ba_as_uint32p @-> returning void)
  
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
  
  let uniform4ui =
    foreign ~stub "glUniform4ui"
      (int @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int_as_uint @-> returning void)
  
  let uniform4uiv =
    foreign ~stub "glUniform4uiv"
      (int @-> int @-> ba_as_uint32p @-> returning void)
  
  let uniform_block_binding =
    foreign ~stub "glUniformBlockBinding"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let uniform_matrix2fv =
    foreign ~stub "glUniformMatrix2fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix2x3fv =
    foreign ~stub "glUniformMatrix2x3fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix2x4fv =
    foreign ~stub "glUniformMatrix2x4fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix3fv =
    foreign ~stub "glUniformMatrix3fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix3x2fv =
    foreign ~stub "glUniformMatrix3x2fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix3x4fv =
    foreign ~stub "glUniformMatrix3x4fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix4fv =
    foreign ~stub "glUniformMatrix4fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix4x2fv =
    foreign ~stub "glUniformMatrix4x2fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let uniform_matrix4x3fv =
    foreign ~stub "glUniformMatrix4x3fv"
      (int @-> int @-> bool @-> ba_as_float32p @-> returning void)
  
  let unmap_buffer =
    foreign ~stub "glUnmapBuffer" (int_as_uint @-> returning bool)
  
  let use_program =
    foreign ~stub "glUseProgram" (int_as_uint @-> returning void)
  
  let validate_program =
    foreign ~stub "glValidateProgram" (int_as_uint @-> returning void)
  
  let vertex_attrib1d =
    foreign ~stub "glVertexAttrib1d"
      (int_as_uint @-> double @-> returning void)
  
  let vertex_attrib1dv =
    foreign ~stub "glVertexAttrib1dv"
      (int_as_uint @-> ba_as_float64p @-> returning void)
  
  let vertex_attrib1f =
    foreign ~stub "glVertexAttrib1f"
      (int_as_uint @-> float @-> returning void)
  
  let vertex_attrib1fv =
    foreign ~stub "glVertexAttrib1fv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let vertex_attrib1s =
    foreign ~stub "glVertexAttrib1s"
      (int_as_uint @-> short @-> returning void)
  
  let vertex_attrib1sv =
    foreign ~stub "glVertexAttrib1sv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib2d =
    foreign ~stub "glVertexAttrib2d"
      (int_as_uint @-> double @-> double @-> returning void)
  
  let vertex_attrib2dv =
    foreign ~stub "glVertexAttrib2dv"
      (int_as_uint @-> ba_as_float64p @-> returning void)
  
  let vertex_attrib2f =
    foreign ~stub "glVertexAttrib2f"
      (int_as_uint @-> float @-> float @-> returning void)
  
  let vertex_attrib2fv =
    foreign ~stub "glVertexAttrib2fv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let vertex_attrib2s =
    foreign ~stub "glVertexAttrib2s"
      (int_as_uint @-> short @-> short @-> returning void)
  
  let vertex_attrib2sv =
    foreign ~stub "glVertexAttrib2sv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib3d =
    foreign ~stub "glVertexAttrib3d"
      (int_as_uint @-> double @-> double @-> double @-> returning void)
  
  let vertex_attrib3dv =
    foreign ~stub "glVertexAttrib3dv"
      (int_as_uint @-> ba_as_float64p @-> returning void)
  
  let vertex_attrib3f =
    foreign ~stub "glVertexAttrib3f"
      (int_as_uint @-> float @-> float @-> float @-> returning void)
  
  let vertex_attrib3fv =
    foreign ~stub "glVertexAttrib3fv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let vertex_attrib3s =
    foreign ~stub "glVertexAttrib3s"
      (int_as_uint @-> short @-> short @-> short @-> returning void)
  
  let vertex_attrib3sv =
    foreign ~stub "glVertexAttrib3sv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib4nbv =
    foreign ~stub "glVertexAttrib4Nbv"
      (int_as_uint @-> ba_as_int8p @-> returning void)
  
  let vertex_attrib4niv =
    foreign ~stub "glVertexAttrib4Niv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let vertex_attrib4nsv =
    foreign ~stub "glVertexAttrib4Nsv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib4nub =
    foreign ~stub "glVertexAttrib4Nub"
      (int_as_uint @-> int_as_uint8_t @-> int_as_uint8_t @->
       int_as_uint8_t @-> int_as_uint8_t @-> returning void)
  
  let vertex_attrib4nubv =
    foreign ~stub "glVertexAttrib4Nubv"
      (int_as_uint @-> ba_as_uint8p @-> returning void)
  
  let vertex_attrib4nuiv =
    foreign ~stub "glVertexAttrib4Nuiv"
      (int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let vertex_attrib4nusv =
    foreign ~stub "glVertexAttrib4Nusv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib4bv =
    foreign ~stub "glVertexAttrib4bv"
      (int_as_uint @-> ba_as_int8p @-> returning void)
  
  let vertex_attrib4d =
    foreign ~stub "glVertexAttrib4d"
      (int_as_uint @-> double @-> double @-> double @-> double @->
       returning void)
  
  let vertex_attrib4dv =
    foreign ~stub "glVertexAttrib4dv"
      (int_as_uint @-> ba_as_float64p @-> returning void)
  
  let vertex_attrib4f =
    foreign ~stub "glVertexAttrib4f"
      (int_as_uint @-> float @-> float @-> float @-> float @->
       returning void)
  
  let vertex_attrib4fv =
    foreign ~stub "glVertexAttrib4fv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let vertex_attrib4iv =
    foreign ~stub "glVertexAttrib4iv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let vertex_attrib4s =
    foreign ~stub "glVertexAttrib4s"
      (int_as_uint @-> short @-> short @-> short @-> short @->
       returning void)
  
  let vertex_attrib4sv =
    foreign ~stub "glVertexAttrib4sv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib4ubv =
    foreign ~stub "glVertexAttrib4ubv"
      (int_as_uint @-> ba_as_uint8p @-> returning void)
  
  let vertex_attrib4uiv =
    foreign ~stub "glVertexAttrib4uiv"
      (int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let vertex_attrib4usv =
    foreign ~stub "glVertexAttrib4usv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib_divisor =
    foreign ~stub "glVertexAttribDivisor"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let vertex_attrib_i1i =
    foreign ~stub "glVertexAttribI1i"
      (int_as_uint @-> int @-> returning void)
  
  let vertex_attrib_i1iv =
    foreign ~stub "glVertexAttribI1iv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let vertex_attrib_i1ui =
    foreign ~stub "glVertexAttribI1ui"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let vertex_attrib_i1uiv =
    foreign ~stub "glVertexAttribI1uiv"
      (int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let vertex_attrib_i2i =
    foreign ~stub "glVertexAttribI2i"
      (int_as_uint @-> int @-> int @-> returning void)
  
  let vertex_attrib_i2iv =
    foreign ~stub "glVertexAttribI2iv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let vertex_attrib_i2ui =
    foreign ~stub "glVertexAttribI2ui"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let vertex_attrib_i2uiv =
    foreign ~stub "glVertexAttribI2uiv"
      (int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let vertex_attrib_i3i =
    foreign ~stub "glVertexAttribI3i"
      (int_as_uint @-> int @-> int @-> int @-> returning void)
  
  let vertex_attrib_i3iv =
    foreign ~stub "glVertexAttribI3iv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let vertex_attrib_i3ui =
    foreign ~stub "glVertexAttribI3ui"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let vertex_attrib_i3uiv =
    foreign ~stub "glVertexAttribI3uiv"
      (int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let vertex_attrib_i4bv =
    foreign ~stub "glVertexAttribI4bv"
      (int_as_uint @-> ba_as_int8p @-> returning void)
  
  let vertex_attrib_i4i =
    foreign ~stub "glVertexAttribI4i"
      (int_as_uint @-> int @-> int @-> int @-> int @-> returning void)
  
  let vertex_attrib_i4iv =
    foreign ~stub "glVertexAttribI4iv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let vertex_attrib_i4sv =
    foreign ~stub "glVertexAttribI4sv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib_i4ubv =
    foreign ~stub "glVertexAttribI4ubv"
      (int_as_uint @-> ba_as_uint8p @-> returning void)
  
  let vertex_attrib_i4ui =
    foreign ~stub "glVertexAttribI4ui"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int_as_uint @-> returning void)
  
  let vertex_attrib_i4uiv =
    foreign ~stub "glVertexAttribI4uiv"
      (int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let vertex_attrib_i4usv =
    foreign ~stub "glVertexAttribI4usv"
      (int_as_uint @-> ba_as_uint16p @-> returning void)
  
  let vertex_attrib_ipointer =
    foreign ~stub "glVertexAttribIPointer"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> (ptr void) @->
       returning void)
  
  let vertex_attrib_ipointer index size type_ stride pointer =
    let pointer = match pointer with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    vertex_attrib_ipointer index size type_ stride pointer
  
  let vertex_attrib_p1ui =
    foreign ~stub "glVertexAttribP1ui"
      (int_as_uint @-> int_as_uint @-> bool @-> int_as_uint @->
       returning void)
  
  let vertex_attrib_p1uiv =
    foreign ~stub "glVertexAttribP1uiv"
      (int_as_uint @-> int_as_uint @-> bool @-> ba_as_uint32p @->
       returning void)
  
  let vertex_attrib_p2ui =
    foreign ~stub "glVertexAttribP2ui"
      (int_as_uint @-> int_as_uint @-> bool @-> int_as_uint @->
       returning void)
  
  let vertex_attrib_p2uiv =
    foreign ~stub "glVertexAttribP2uiv"
      (int_as_uint @-> int_as_uint @-> bool @-> ba_as_uint32p @->
       returning void)
  
  let vertex_attrib_p3ui =
    foreign ~stub "glVertexAttribP3ui"
      (int_as_uint @-> int_as_uint @-> bool @-> int_as_uint @->
       returning void)
  
  let vertex_attrib_p3uiv =
    foreign ~stub "glVertexAttribP3uiv"
      (int_as_uint @-> int_as_uint @-> bool @-> ba_as_uint32p @->
       returning void)
  
  let vertex_attrib_p4ui =
    foreign ~stub "glVertexAttribP4ui"
      (int_as_uint @-> int_as_uint @-> bool @-> int_as_uint @->
       returning void)
  
  let vertex_attrib_p4uiv =
    foreign ~stub "glVertexAttribP4uiv"
      (int_as_uint @-> int_as_uint @-> bool @-> ba_as_uint32p @->
       returning void)
  
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
  
  let wait_sync =
    foreign ~stub "glWaitSync"
      (sync @-> int_as_uint @-> int64_as_uint64_t @-> returning void)
  

  (* Enums *)

  let active_attributes = 0x8B89
  let active_attribute_max_length = 0x8B8A
  let active_texture_enum = 0x84E0
  let active_uniforms = 0x8B86
  let active_uniform_blocks = 0x8A36
  let active_uniform_block_max_name_length = 0x8A35
  let active_uniform_max_length = 0x8B87
  let aliased_line_width_range = 0x846E
  let alpha = 0x1906
  let already_signaled = 0x911A
  let always = 0x207
  let and_ = 0x1501
  let and_inverted = 0x1504
  let and_reverse = 0x1502
  let any_samples_passed = 0x8C2F
  let array_buffer = 0x8892
  let array_buffer_binding = 0x8894
  let attached_shaders = 0x8B85
  let back = 0x405
  let back_left = 0x402
  let back_right = 0x403
  let bgr = 0x80E0
  let bgra = 0x80E1
  let bgra_integer = 0x8D9B
  let bgr_integer = 0x8D9A
  let blend = 0xBE2
  let blend_dst = 0xBE0
  let blend_dst_alpha = 0x80CA
  let blend_dst_rgb = 0x80C8
  let blend_equation_alpha = 0x883D
  let blend_equation_rgb = 0x8009
  let blend_src = 0xBE1
  let blend_src_alpha = 0x80CB
  let blend_src_rgb = 0x80C9
  let blue = 0x1905
  let blue_integer = 0x8D96
  let bool = 0x8B56
  let bool_vec2 = 0x8B57
  let bool_vec3 = 0x8B58
  let bool_vec4 = 0x8B59
  let buffer_access = 0x88BB
  let buffer_access_flags = 0x911F
  let buffer_mapped = 0x88BC
  let buffer_map_length = 0x9120
  let buffer_map_offset = 0x9121
  let buffer_map_pointer = 0x88BD
  let buffer_size = 0x8764
  let buffer_usage = 0x8765
  let byte = 0x1400
  let ccw = 0x901
  let clamp_read_color = 0x891C
  let clamp_to_border = 0x812D
  let clamp_to_edge = 0x812F
  let clear_enum = 0x1500
  let clip_distance0 = 0x3000
  let clip_distance1 = 0x3001
  let clip_distance2 = 0x3002
  let clip_distance3 = 0x3003
  let clip_distance4 = 0x3004
  let clip_distance5 = 0x3005
  let clip_distance6 = 0x3006
  let clip_distance7 = 0x3007
  let color = 0x1800
  let color_attachment0 = 0x8CE0
  let color_attachment1 = 0x8CE1
  let color_attachment10 = 0x8CEA
  let color_attachment11 = 0x8CEB
  let color_attachment12 = 0x8CEC
  let color_attachment13 = 0x8CED
  let color_attachment14 = 0x8CEE
  let color_attachment15 = 0x8CEF
  let color_attachment16 = 0x8CF0
  let color_attachment17 = 0x8CF1
  let color_attachment18 = 0x8CF2
  let color_attachment19 = 0x8CF3
  let color_attachment2 = 0x8CE2
  let color_attachment20 = 0x8CF4
  let color_attachment21 = 0x8CF5
  let color_attachment22 = 0x8CF6
  let color_attachment23 = 0x8CF7
  let color_attachment24 = 0x8CF8
  let color_attachment25 = 0x8CF9
  let color_attachment26 = 0x8CFA
  let color_attachment27 = 0x8CFB
  let color_attachment28 = 0x8CFC
  let color_attachment29 = 0x8CFD
  let color_attachment3 = 0x8CE3
  let color_attachment30 = 0x8CFE
  let color_attachment31 = 0x8CFF
  let color_attachment4 = 0x8CE4
  let color_attachment5 = 0x8CE5
  let color_attachment6 = 0x8CE6
  let color_attachment7 = 0x8CE7
  let color_attachment8 = 0x8CE8
  let color_attachment9 = 0x8CE9
  let color_buffer_bit = 0x4000
  let color_clear_value = 0xC22
  let color_logic_op = 0xBF2
  let color_writemask = 0xC23
  let compare_ref_to_texture = 0x884E
  let compile_status = 0x8B81
  let compressed_red = 0x8225
  let compressed_red_rgtc1 = 0x8DBB
  let compressed_rg = 0x8226
  let compressed_rgb = 0x84ED
  let compressed_rgba = 0x84EE
  let compressed_rg_rgtc2 = 0x8DBD
  let compressed_signed_red_rgtc1 = 0x8DBC
  let compressed_signed_rg_rgtc2 = 0x8DBE
  let compressed_srgb = 0x8C48
  let compressed_srgb_alpha = 0x8C49
  let compressed_texture_formats = 0x86A3
  let condition_satisfied = 0x911C
  let constant_alpha = 0x8003
  let constant_color = 0x8001
  let context_compatibility_profile_bit = 0x2
  let context_core_profile_bit = 0x1
  let context_flags = 0x821E
  let context_flag_forward_compatible_bit = 0x1
  let context_profile_mask = 0x9126
  let copy = 0x1503
  let copy_inverted = 0x150C
  let copy_read_buffer = 0x8F36
  let copy_write_buffer = 0x8F37
  let cull_face_enum = 0xB44
  let cull_face_mode = 0xB45
  let current_program = 0x8B8D
  let current_query = 0x8865
  let current_vertex_attrib = 0x8626
  let cw = 0x900
  let decr = 0x1E03
  let decr_wrap = 0x8508
  let delete_status = 0x8B80
  let depth = 0x1801
  let depth24_stencil8 = 0x88F0
  let depth32f_stencil8 = 0x8CAD
  let depth_attachment = 0x8D00
  let depth_buffer_bit = 0x100
  let depth_clamp = 0x864F
  let depth_clear_value = 0xB73
  let depth_component = 0x1902
  let depth_component16 = 0x81A5
  let depth_component24 = 0x81A6
  let depth_component32 = 0x81A7
  let depth_component32f = 0x8CAC
  let depth_func_enum = 0xB74
  let depth_range_enum = 0xB70
  let depth_stencil = 0x84F9
  let depth_stencil_attachment = 0x821A
  let depth_test = 0xB71
  let depth_writemask = 0xB72
  let dither = 0xBD0
  let dont_care = 0x1100
  let double = 0x140A
  let doublebuffer = 0xC32
  let draw_buffer_enum = 0xC01
  let draw_buffer0 = 0x8825
  let draw_buffer1 = 0x8826
  let draw_buffer10 = 0x882F
  let draw_buffer11 = 0x8830
  let draw_buffer12 = 0x8831
  let draw_buffer13 = 0x8832
  let draw_buffer14 = 0x8833
  let draw_buffer15 = 0x8834
  let draw_buffer2 = 0x8827
  let draw_buffer3 = 0x8828
  let draw_buffer4 = 0x8829
  let draw_buffer5 = 0x882A
  let draw_buffer6 = 0x882B
  let draw_buffer7 = 0x882C
  let draw_buffer8 = 0x882D
  let draw_buffer9 = 0x882E
  let draw_framebuffer = 0x8CA9
  let draw_framebuffer_binding = 0x8CA6
  let dst_alpha = 0x304
  let dst_color = 0x306
  let dynamic_copy = 0x88EA
  let dynamic_draw = 0x88E8
  let dynamic_read = 0x88E9
  let element_array_buffer = 0x8893
  let element_array_buffer_binding = 0x8895
  let equal = 0x202
  let equiv = 0x1509
  let extensions = 0x1F03
  let false_ = 0x0
  let fastest = 0x1101
  let fill = 0x1B02
  let first_vertex_convention = 0x8E4D
  let fixed_only = 0x891D
  let float = 0x1406
  let float_32_unsigned_int_24_8_rev = 0x8DAD
  let float_mat2 = 0x8B5A
  let float_mat2x3 = 0x8B65
  let float_mat2x4 = 0x8B66
  let float_mat3 = 0x8B5B
  let float_mat3x2 = 0x8B67
  let float_mat3x4 = 0x8B68
  let float_mat4 = 0x8B5C
  let float_mat4x2 = 0x8B69
  let float_mat4x3 = 0x8B6A
  let float_vec2 = 0x8B50
  let float_vec3 = 0x8B51
  let float_vec4 = 0x8B52
  let fragment_shader = 0x8B30
  let fragment_shader_derivative_hint = 0x8B8B
  let framebuffer = 0x8D40
  let framebuffer_attachment_alpha_size = 0x8215
  let framebuffer_attachment_blue_size = 0x8214
  let framebuffer_attachment_color_encoding = 0x8210
  let framebuffer_attachment_component_type = 0x8211
  let framebuffer_attachment_depth_size = 0x8216
  let framebuffer_attachment_green_size = 0x8213
  let framebuffer_attachment_layered = 0x8DA7
  let framebuffer_attachment_object_name = 0x8CD1
  let framebuffer_attachment_object_type = 0x8CD0
  let framebuffer_attachment_red_size = 0x8212
  let framebuffer_attachment_stencil_size = 0x8217
  let framebuffer_attachment_texture_cube_map_face = 0x8CD3
  let framebuffer_attachment_texture_layer = 0x8CD4
  let framebuffer_attachment_texture_level = 0x8CD2
  let framebuffer_binding = 0x8CA6
  let framebuffer_complete = 0x8CD5
  let framebuffer_default = 0x8218
  let framebuffer_incomplete_attachment = 0x8CD6
  let framebuffer_incomplete_draw_buffer = 0x8CDB
  let framebuffer_incomplete_layer_targets = 0x8DA8
  let framebuffer_incomplete_missing_attachment = 0x8CD7
  let framebuffer_incomplete_multisample = 0x8D56
  let framebuffer_incomplete_read_buffer = 0x8CDC
  let framebuffer_srgb = 0x8DB9
  let framebuffer_undefined = 0x8219
  let framebuffer_unsupported = 0x8CDD
  let front = 0x404
  let front_and_back = 0x408
  let front_face_enum = 0xB46
  let front_left = 0x400
  let front_right = 0x401
  let func_add = 0x8006
  let func_reverse_subtract = 0x800B
  let func_subtract = 0x800A
  let geometry_input_type = 0x8917
  let geometry_output_type = 0x8918
  let geometry_shader = 0x8DD9
  let geometry_vertices_out = 0x8916
  let gequal = 0x206
  let greater = 0x204
  let green = 0x1904
  let green_integer = 0x8D95
  let half_float = 0x140B
  let incr = 0x1E02
  let incr_wrap = 0x8507
  let info_log_length = 0x8B84
  let int = 0x1404
  let interleaved_attribs = 0x8C8C
  let int_2_10_10_10_rev = 0x8D9F
  let int_sampler_1d = 0x8DC9
  let int_sampler_1d_array = 0x8DCE
  let int_sampler_2d = 0x8DCA
  let int_sampler_2d_array = 0x8DCF
  let int_sampler_2d_multisample = 0x9109
  let int_sampler_2d_multisample_array = 0x910C
  let int_sampler_2d_rect = 0x8DCD
  let int_sampler_3d = 0x8DCB
  let int_sampler_buffer = 0x8DD0
  let int_sampler_cube = 0x8DCC
  let int_vec2 = 0x8B53
  let int_vec3 = 0x8B54
  let int_vec4 = 0x8B55
  let invalid_enum = 0x500
  let invalid_framebuffer_operation = 0x506
  let invalid_index = 0xFFFFFFFFl
  let invalid_operation = 0x502
  let invalid_value = 0x501
  let invert = 0x150A
  let keep = 0x1E00
  let last_vertex_convention = 0x8E4E
  let left = 0x406
  let lequal = 0x203
  let less = 0x201
  let line = 0x1B01
  let linear = 0x2601
  let linear_mipmap_linear = 0x2703
  let linear_mipmap_nearest = 0x2701
  let lines = 0x1
  let lines_adjacency = 0xA
  let line_loop = 0x2
  let line_smooth = 0xB20
  let line_smooth_hint = 0xC52
  let line_strip = 0x3
  let line_strip_adjacency = 0xB
  let line_width_enum = 0xB21
  let line_width_granularity = 0xB23
  let line_width_range = 0xB22
  let link_status = 0x8B82
  let logic_op_mode = 0xBF0
  let lower_left = 0x8CA1
  let major_version = 0x821B
  let map_flush_explicit_bit = 0x10
  let map_invalidate_buffer_bit = 0x8
  let map_invalidate_range_bit = 0x4
  let map_read_bit = 0x1
  let map_unsynchronized_bit = 0x20
  let map_write_bit = 0x2
  let max = 0x8008
  let max_3d_texture_size = 0x8073
  let max_array_texture_layers = 0x88FF
  let max_clip_distances = 0xD32
  let max_color_attachments = 0x8CDF
  let max_color_texture_samples = 0x910E
  let max_combined_fragment_uniform_components = 0x8A33
  let max_combined_geometry_uniform_components = 0x8A32
  let max_combined_texture_image_units = 0x8B4D
  let max_combined_uniform_blocks = 0x8A2E
  let max_combined_vertex_uniform_components = 0x8A31
  let max_cube_map_texture_size = 0x851C
  let max_depth_texture_samples = 0x910F
  let max_draw_buffers = 0x8824
  let max_dual_source_draw_buffers = 0x88FC
  let max_elements_indices = 0x80E9
  let max_elements_vertices = 0x80E8
  let max_fragment_input_components = 0x9125
  let max_fragment_uniform_blocks = 0x8A2D
  let max_fragment_uniform_components = 0x8B49
  let max_geometry_input_components = 0x9123
  let max_geometry_output_components = 0x9124
  let max_geometry_output_vertices = 0x8DE0
  let max_geometry_texture_image_units = 0x8C29
  let max_geometry_total_output_components = 0x8DE1
  let max_geometry_uniform_blocks = 0x8A2C
  let max_geometry_uniform_components = 0x8DDF
  let max_integer_samples = 0x9110
  let max_program_texel_offset = 0x8905
  let max_rectangle_texture_size = 0x84F8
  let max_renderbuffer_size = 0x84E8
  let max_samples = 0x8D57
  let max_sample_mask_words = 0x8E59
  let max_server_wait_timeout = 0x9111
  let max_texture_buffer_size = 0x8C2B
  let max_texture_image_units = 0x8872
  let max_texture_lod_bias = 0x84FD
  let max_texture_size = 0xD33
  let max_transform_feedback_interleaved_components = 0x8C8A
  let max_transform_feedback_separate_attribs = 0x8C8B
  let max_transform_feedback_separate_components = 0x8C80
  let max_uniform_block_size = 0x8A30
  let max_uniform_buffer_bindings = 0x8A2F
  let max_varying_components = 0x8B4B
  let max_varying_floats = 0x8B4B
  let max_vertex_attribs = 0x8869
  let max_vertex_output_components = 0x9122
  let max_vertex_texture_image_units = 0x8B4C
  let max_vertex_uniform_blocks = 0x8A2B
  let max_vertex_uniform_components = 0x8B4A
  let max_viewport_dims = 0xD3A
  let min = 0x8007
  let minor_version = 0x821C
  let min_program_texel_offset = 0x8904
  let mirrored_repeat = 0x8370
  let multisample = 0x809D
  let nand = 0x150E
  let nearest = 0x2600
  let nearest_mipmap_linear = 0x2702
  let nearest_mipmap_nearest = 0x2700
  let never = 0x200
  let nicest = 0x1102
  let none = 0x0
  let noop = 0x1505
  let nor = 0x1508
  let notequal = 0x205
  let no_error = 0x0
  let num_compressed_texture_formats = 0x86A2
  let num_extensions = 0x821D
  let object_type = 0x9112
  let one = 0x1
  let one_minus_constant_alpha = 0x8004
  let one_minus_constant_color = 0x8002
  let one_minus_dst_alpha = 0x305
  let one_minus_dst_color = 0x307
  let one_minus_src1_alpha = 0x88FB
  let one_minus_src1_color = 0x88FA
  let one_minus_src_alpha = 0x303
  let one_minus_src_color = 0x301
  let or_ = 0x1507
  let or_inverted = 0x150D
  let or_reverse = 0x150B
  let out_of_memory = 0x505
  let pack_alignment = 0xD05
  let pack_image_height = 0x806C
  let pack_lsb_first = 0xD01
  let pack_row_length = 0xD02
  let pack_skip_images = 0x806B
  let pack_skip_pixels = 0xD04
  let pack_skip_rows = 0xD03
  let pack_swap_bytes = 0xD00
  let pixel_pack_buffer = 0x88EB
  let pixel_pack_buffer_binding = 0x88ED
  let pixel_unpack_buffer = 0x88EC
  let pixel_unpack_buffer_binding = 0x88EF
  let point = 0x1B00
  let points = 0x0
  let point_fade_threshold_size = 0x8128
  let point_size_enum = 0xB11
  let point_size_granularity = 0xB13
  let point_size_range = 0xB12
  let point_sprite_coord_origin = 0x8CA0
  let polygon_mode_enum = 0xB40
  let polygon_offset_factor = 0x8038
  let polygon_offset_fill = 0x8037
  let polygon_offset_line = 0x2A02
  let polygon_offset_point = 0x2A01
  let polygon_offset_units = 0x2A00
  let polygon_smooth = 0xB41
  let polygon_smooth_hint = 0xC53
  let primitives_generated = 0x8C87
  let primitive_restart = 0x8F9D
  let primitive_restart_index_enum = 0x8F9E
  let program_point_size = 0x8642
  let provoking_vertex_enum = 0x8E4F
  let proxy_texture_1d = 0x8063
  let proxy_texture_1d_array = 0x8C19
  let proxy_texture_2d = 0x8064
  let proxy_texture_2d_array = 0x8C1B
  let proxy_texture_2d_multisample = 0x9101
  let proxy_texture_2d_multisample_array = 0x9103
  let proxy_texture_3d = 0x8070
  let proxy_texture_cube_map = 0x851B
  let proxy_texture_rectangle = 0x84F7
  let quads_follow_provoking_vertex_convention = 0x8E4C
  let query_by_region_no_wait = 0x8E16
  let query_by_region_wait = 0x8E15
  let query_counter_bits = 0x8864
  let query_no_wait = 0x8E14
  let query_result = 0x8866
  let query_result_available = 0x8867
  let query_wait = 0x8E13
  let r11f_g11f_b10f = 0x8C3A
  let r16 = 0x822A
  let r16f = 0x822D
  let r16i = 0x8233
  let r16ui = 0x8234
  let r16_snorm = 0x8F98
  let r32f = 0x822E
  let r32i = 0x8235
  let r32ui = 0x8236
  let r3_g3_b2 = 0x2A10
  let r8 = 0x8229
  let r8i = 0x8231
  let r8ui = 0x8232
  let r8_snorm = 0x8F94
  let rasterizer_discard = 0x8C89
  let read_buffer_enum = 0xC02
  let read_framebuffer = 0x8CA8
  let read_framebuffer_binding = 0x8CAA
  let read_only = 0x88B8
  let read_write = 0x88BA
  let red = 0x1903
  let red_integer = 0x8D94
  let renderbuffer = 0x8D41
  let renderbuffer_alpha_size = 0x8D53
  let renderbuffer_binding = 0x8CA7
  let renderbuffer_blue_size = 0x8D52
  let renderbuffer_depth_size = 0x8D54
  let renderbuffer_green_size = 0x8D51
  let renderbuffer_height = 0x8D43
  let renderbuffer_internal_format = 0x8D44
  let renderbuffer_red_size = 0x8D50
  let renderbuffer_samples = 0x8CAB
  let renderbuffer_stencil_size = 0x8D55
  let renderbuffer_width = 0x8D42
  let renderer = 0x1F01
  let repeat = 0x2901
  let replace = 0x1E01
  let rg = 0x8227
  let rg16 = 0x822C
  let rg16f = 0x822F
  let rg16i = 0x8239
  let rg16ui = 0x823A
  let rg16_snorm = 0x8F99
  let rg32f = 0x8230
  let rg32i = 0x823B
  let rg32ui = 0x823C
  let rg8 = 0x822B
  let rg8i = 0x8237
  let rg8ui = 0x8238
  let rg8_snorm = 0x8F95
  let rgb = 0x1907
  let rgb10 = 0x8052
  let rgb10_a2 = 0x8059
  let rgb10_a2ui = 0x906F
  let rgb12 = 0x8053
  let rgb16 = 0x8054
  let rgb16f = 0x881B
  let rgb16i = 0x8D89
  let rgb16ui = 0x8D77
  let rgb16_snorm = 0x8F9A
  let rgb32f = 0x8815
  let rgb32i = 0x8D83
  let rgb32ui = 0x8D71
  let rgb4 = 0x804F
  let rgb5 = 0x8050
  let rgb5_a1 = 0x8057
  let rgb8 = 0x8051
  let rgb8i = 0x8D8F
  let rgb8ui = 0x8D7D
  let rgb8_snorm = 0x8F96
  let rgb9_e5 = 0x8C3D
  let rgba = 0x1908
  let rgba12 = 0x805A
  let rgba16 = 0x805B
  let rgba16f = 0x881A
  let rgba16i = 0x8D88
  let rgba16ui = 0x8D76
  let rgba16_snorm = 0x8F9B
  let rgba2 = 0x8055
  let rgba32f = 0x8814
  let rgba32i = 0x8D82
  let rgba32ui = 0x8D70
  let rgba4 = 0x8056
  let rgba8 = 0x8058
  let rgba8i = 0x8D8E
  let rgba8ui = 0x8D7C
  let rgba8_snorm = 0x8F97
  let rgba_integer = 0x8D99
  let rgb_integer = 0x8D98
  let rg_integer = 0x8228
  let right = 0x407
  let sampler_1d = 0x8B5D
  let sampler_1d_array = 0x8DC0
  let sampler_1d_array_shadow = 0x8DC3
  let sampler_1d_shadow = 0x8B61
  let sampler_2d = 0x8B5E
  let sampler_2d_array = 0x8DC1
  let sampler_2d_array_shadow = 0x8DC4
  let sampler_2d_multisample = 0x9108
  let sampler_2d_multisample_array = 0x910B
  let sampler_2d_rect = 0x8B63
  let sampler_2d_rect_shadow = 0x8B64
  let sampler_2d_shadow = 0x8B62
  let sampler_3d = 0x8B5F
  let sampler_binding = 0x8919
  let sampler_buffer = 0x8DC2
  let sampler_cube = 0x8B60
  let sampler_cube_shadow = 0x8DC5
  let samples = 0x80A9
  let samples_passed = 0x8914
  let sample_alpha_to_coverage = 0x809E
  let sample_alpha_to_one = 0x809F
  let sample_buffers = 0x80A8
  let sample_coverage_enum = 0x80A0
  let sample_coverage_invert = 0x80AB
  let sample_coverage_value = 0x80AA
  let sample_mask = 0x8E51
  let sample_mask_value = 0x8E52
  let sample_position = 0x8E50
  let scissor_box = 0xC10
  let scissor_test = 0xC11
  let separate_attribs = 0x8C8D
  let set = 0x150F
  let shader_source_length = 0x8B88
  let shader_type = 0x8B4F
  let shading_language_version = 0x8B8C
  let short = 0x1402
  let signaled = 0x9119
  let signed_normalized = 0x8F9C
  let smooth_line_width_granularity = 0xB23
  let smooth_line_width_range = 0xB22
  let smooth_point_size_granularity = 0xB13
  let smooth_point_size_range = 0xB12
  let src1_alpha = 0x8589
  let src1_color = 0x88F9
  let src_alpha = 0x302
  let src_alpha_saturate = 0x308
  let src_color = 0x300
  let srgb = 0x8C40
  let srgb8 = 0x8C41
  let srgb8_alpha8 = 0x8C43
  let srgb_alpha = 0x8C42
  let static_copy = 0x88E6
  let static_draw = 0x88E4
  let static_read = 0x88E5
  let stencil = 0x1802
  let stencil_attachment = 0x8D20
  let stencil_back_fail = 0x8801
  let stencil_back_func = 0x8800
  let stencil_back_pass_depth_fail = 0x8802
  let stencil_back_pass_depth_pass = 0x8803
  let stencil_back_ref = 0x8CA3
  let stencil_back_value_mask = 0x8CA4
  let stencil_back_writemask = 0x8CA5
  let stencil_buffer_bit = 0x400
  let stencil_clear_value = 0xB91
  let stencil_fail = 0xB94
  let stencil_func_enum = 0xB92
  let stencil_index = 0x1901
  let stencil_index1 = 0x8D46
  let stencil_index16 = 0x8D49
  let stencil_index4 = 0x8D47
  let stencil_index8 = 0x8D48
  let stencil_pass_depth_fail = 0xB95
  let stencil_pass_depth_pass = 0xB96
  let stencil_ref = 0xB97
  let stencil_test = 0xB90
  let stencil_value_mask = 0xB93
  let stencil_writemask = 0xB98
  let stereo = 0xC33
  let stream_copy = 0x88E2
  let stream_draw = 0x88E0
  let stream_read = 0x88E1
  let subpixel_bits = 0xD50
  let sync_condition = 0x9113
  let sync_fence = 0x9116
  let sync_flags = 0x9115
  let sync_flush_commands_bit = 0x1
  let sync_gpu_commands_complete = 0x9117
  let sync_status = 0x9114
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
  let texture_1d = 0xDE0
  let texture_1d_array = 0x8C18
  let texture_2d = 0xDE1
  let texture_2d_array = 0x8C1A
  let texture_2d_multisample = 0x9100
  let texture_2d_multisample_array = 0x9102
  let texture_3d = 0x806F
  let texture_alpha_size = 0x805F
  let texture_alpha_type = 0x8C13
  let texture_base_level = 0x813C
  let texture_binding_1d = 0x8068
  let texture_binding_1d_array = 0x8C1C
  let texture_binding_2d = 0x8069
  let texture_binding_2d_array = 0x8C1D
  let texture_binding_2d_multisample = 0x9104
  let texture_binding_2d_multisample_array = 0x9105
  let texture_binding_3d = 0x806A
  let texture_binding_buffer = 0x8C2C
  let texture_binding_cube_map = 0x8514
  let texture_binding_rectangle = 0x84F6
  let texture_blue_size = 0x805E
  let texture_blue_type = 0x8C12
  let texture_border_color = 0x1004
  let texture_buffer = 0x8C2A
  let texture_buffer_data_store_binding = 0x8C2D
  let texture_compare_func = 0x884D
  let texture_compare_mode = 0x884C
  let texture_compressed = 0x86A1
  let texture_compressed_image_size = 0x86A0
  let texture_compression_hint = 0x84EF
  let texture_cube_map = 0x8513
  let texture_cube_map_negative_x = 0x8516
  let texture_cube_map_negative_y = 0x8518
  let texture_cube_map_negative_z = 0x851A
  let texture_cube_map_positive_x = 0x8515
  let texture_cube_map_positive_y = 0x8517
  let texture_cube_map_positive_z = 0x8519
  let texture_cube_map_seamless = 0x884F
  let texture_depth = 0x8071
  let texture_depth_size = 0x884A
  let texture_depth_type = 0x8C16
  let texture_fixed_sample_locations = 0x9107
  let texture_green_size = 0x805D
  let texture_green_type = 0x8C11
  let texture_height = 0x1001
  let texture_internal_format = 0x1003
  let texture_lod_bias = 0x8501
  let texture_mag_filter = 0x2800
  let texture_max_level = 0x813D
  let texture_max_lod = 0x813B
  let texture_min_filter = 0x2801
  let texture_min_lod = 0x813A
  let texture_rectangle = 0x84F5
  let texture_red_size = 0x805C
  let texture_red_type = 0x8C10
  let texture_samples = 0x9106
  let texture_shared_size = 0x8C3F
  let texture_stencil_size = 0x88F1
  let texture_swizzle_a = 0x8E45
  let texture_swizzle_b = 0x8E44
  let texture_swizzle_g = 0x8E43
  let texture_swizzle_r = 0x8E42
  let texture_swizzle_rgba = 0x8E46
  let texture_width = 0x1000
  let texture_wrap_r = 0x8072
  let texture_wrap_s = 0x2802
  let texture_wrap_t = 0x2803
  let timeout_expired = 0x911B
  let timeout_ignored = 0xFFFFFFFFFFFFFFFFL
  let timestamp = 0x8E28
  let time_elapsed = 0x88BF
  let transform_feedback_buffer = 0x8C8E
  let transform_feedback_buffer_binding = 0x8C8F
  let transform_feedback_buffer_mode = 0x8C7F
  let transform_feedback_buffer_size = 0x8C85
  let transform_feedback_buffer_start = 0x8C84
  let transform_feedback_primitives_written = 0x8C88
  let transform_feedback_varyings_enum = 0x8C83
  let transform_feedback_varying_max_length = 0x8C76
  let triangles = 0x4
  let triangles_adjacency = 0xC
  let triangle_fan = 0x6
  let triangle_strip = 0x5
  let triangle_strip_adjacency = 0xD
  let true_ = 0x1
  let uniform_array_stride = 0x8A3C
  let uniform_block_active_uniforms = 0x8A42
  let uniform_block_active_uniform_indices = 0x8A43
  let uniform_block_binding_enum = 0x8A3F
  let uniform_block_data_size = 0x8A40
  let uniform_block_index = 0x8A3A
  let uniform_block_name_length = 0x8A41
  let uniform_block_referenced_by_fragment_shader = 0x8A46
  let uniform_block_referenced_by_geometry_shader = 0x8A45
  let uniform_block_referenced_by_vertex_shader = 0x8A44
  let uniform_buffer = 0x8A11
  let uniform_buffer_binding = 0x8A28
  let uniform_buffer_offset_alignment = 0x8A34
  let uniform_buffer_size = 0x8A2A
  let uniform_buffer_start = 0x8A29
  let uniform_is_row_major = 0x8A3E
  let uniform_matrix_stride = 0x8A3D
  let uniform_name_length = 0x8A39
  let uniform_offset = 0x8A3B
  let uniform_size = 0x8A38
  let uniform_type = 0x8A37
  let unpack_alignment = 0xCF5
  let unpack_image_height = 0x806E
  let unpack_lsb_first = 0xCF1
  let unpack_row_length = 0xCF2
  let unpack_skip_images = 0x806D
  let unpack_skip_pixels = 0xCF4
  let unpack_skip_rows = 0xCF3
  let unpack_swap_bytes = 0xCF0
  let unsignaled = 0x9118
  let unsigned_byte = 0x1401
  let unsigned_byte_2_3_3_rev = 0x8362
  let unsigned_byte_3_3_2 = 0x8032
  let unsigned_int = 0x1405
  let unsigned_int_10f_11f_11f_rev = 0x8C3B
  let unsigned_int_10_10_10_2 = 0x8036
  let unsigned_int_24_8 = 0x84FA
  let unsigned_int_2_10_10_10_rev = 0x8368
  let unsigned_int_5_9_9_9_rev = 0x8C3E
  let unsigned_int_8_8_8_8 = 0x8035
  let unsigned_int_8_8_8_8_rev = 0x8367
  let unsigned_int_sampler_1d = 0x8DD1
  let unsigned_int_sampler_1d_array = 0x8DD6
  let unsigned_int_sampler_2d = 0x8DD2
  let unsigned_int_sampler_2d_array = 0x8DD7
  let unsigned_int_sampler_2d_multisample = 0x910A
  let unsigned_int_sampler_2d_multisample_array = 0x910D
  let unsigned_int_sampler_2d_rect = 0x8DD5
  let unsigned_int_sampler_3d = 0x8DD3
  let unsigned_int_sampler_buffer = 0x8DD8
  let unsigned_int_sampler_cube = 0x8DD4
  let unsigned_int_vec2 = 0x8DC6
  let unsigned_int_vec3 = 0x8DC7
  let unsigned_int_vec4 = 0x8DC8
  let unsigned_normalized = 0x8C17
  let unsigned_short = 0x1403
  let unsigned_short_1_5_5_5_rev = 0x8366
  let unsigned_short_4_4_4_4 = 0x8033
  let unsigned_short_4_4_4_4_rev = 0x8365
  let unsigned_short_5_5_5_1 = 0x8034
  let unsigned_short_5_6_5 = 0x8363
  let unsigned_short_5_6_5_rev = 0x8364
  let upper_left = 0x8CA2
  let validate_status = 0x8B83
  let vendor = 0x1F00
  let version = 0x1F02
  let vertex_array_binding = 0x85B5
  let vertex_attrib_array_buffer_binding = 0x889F
  let vertex_attrib_array_divisor = 0x88FE
  let vertex_attrib_array_enabled = 0x8622
  let vertex_attrib_array_integer = 0x88FD
  let vertex_attrib_array_normalized = 0x886A
  let vertex_attrib_array_pointer = 0x8645
  let vertex_attrib_array_size = 0x8623
  let vertex_attrib_array_stride = 0x8624
  let vertex_attrib_array_type = 0x8625
  let vertex_program_point_size = 0x8642
  let vertex_shader = 0x8B31
  let viewport_enum = 0xBA2
  let wait_failed = 0x911D
  let write_only = 0x88B9
  let xor = 0x1506
  let zero = 0x0
end
