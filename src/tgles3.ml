(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated with:
   apiquery -ml -api gles3.2 *)

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

(* OpenGL ES 3.x bindings *)

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
  
  let ba_opt_as_charp =
    view ~read:(fun _ -> assert false)
         ~write:(function
          | None -> null
          | Some b -> to_voidp (bigarray_start array1 b))
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
  
  type sync = unit ptr
  let sync : sync typ = ptr void
  let sync_opt : sync option typ = ptr_opt void
  
  type uint32_bigarray = (int32, Bigarray.int32_elt) bigarray
  let ba_as_uint32p =
    view ~read:(fun _ -> assert false)
         ~write:(fun b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  let ba_opt_as_uint32p =
    view ~read:(fun _ -> assert false)
         ~write:(function
          | None -> null
          | Some b -> to_voidp (bigarray_start array1 b))
         (ptr void)
  
  type uint64 = int64
  let int64_as_uint64_t =
    view ~read:Unsigned.UInt64.to_int64
         ~write:Unsigned.UInt64.of_int64
         uint64_t
  
  type debug_proc = enum -> enum -> int -> enum -> string -> unit
  
  (* Functions *)

  let stub = true (* If changed, will need updating Windows specific [foreign]. *)

  let active_shader_program =
    foreign ~stub "glActiveShaderProgram"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let active_texture =
    foreign ~stub "glActiveTexture" (int_as_uint @-> returning void)
  
  let attach_shader =
    foreign ~stub "glAttachShader"
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
  
  let bind_framebuffer =
    foreign ~stub "glBindFramebuffer"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_image_texture =
    foreign ~stub "glBindImageTexture"
      (int_as_uint @-> int_as_uint @-> int @-> bool @-> int @->
       int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_program_pipeline =
    foreign ~stub "glBindProgramPipeline" (int_as_uint @-> returning void)
  
  let bind_renderbuffer =
    foreign ~stub "glBindRenderbuffer"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_sampler =
    foreign ~stub "glBindSampler"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_texture =
    foreign ~stub "glBindTexture"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_transform_feedback =
    foreign ~stub "glBindTransformFeedback"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let bind_vertex_array =
    foreign ~stub "glBindVertexArray" (int_as_uint @-> returning void)
  
  let bind_vertex_buffer =
    foreign ~stub "glBindVertexBuffer"
      (int_as_uint @-> int_as_uint @-> int @-> int @-> returning void)
  
  let blend_barrier =
    foreign ~stub "glBlendBarrier" (void @-> returning void)
  
  let blend_color =
    foreign ~stub "glBlendColor"
      (float @-> float @-> float @-> float @-> returning void)
  
  let blend_equation =
    foreign ~stub "glBlendEquation" (int_as_uint @-> returning void)
  
  let blend_equation_separate =
    foreign ~stub "glBlendEquationSeparate"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let blend_equation_separatei =
    foreign ~stub "glBlendEquationSeparatei"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let blend_equationi =
    foreign ~stub "glBlendEquationi"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let blend_func =
    foreign ~stub "glBlendFunc"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let blend_func_separate =
    foreign ~stub "glBlendFuncSeparate"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let blend_func_separatei =
    foreign ~stub "glBlendFuncSeparatei"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int_as_uint @-> returning void)
  
  let blend_funci =
    foreign ~stub "glBlendFunci"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
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
  
  let clear_depthf =
    foreign ~stub "glClearDepthf" (float @-> returning void)
  
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
  
  let copy_image_sub_data =
    foreign ~stub "glCopyImageSubData"
      (int_as_uint @-> int_as_uint @-> int @-> int @-> int @-> int @->
       int_as_uint @-> int_as_uint @-> int @-> int @-> int @-> int @->
       int @-> int @-> int @-> returning void)
  
  let copy_tex_image2d =
    foreign ~stub "glCopyTexImage2D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       int @-> int @-> returning void)
  
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
  
  let create_shader_programv =
    foreign ~stub "glCreateShaderProgramv"
      (int_as_uint @-> int @-> ptr string @-> returning int_as_uint)
  
  let create_shader_programv type_ src =
    let src = allocate string src in
    create_shader_programv type_ 1 src
  
  let cull_face =
    foreign ~stub "glCullFace" (int_as_uint @-> returning void)
  
  module DebugMessageCallback =
    (val (dynamic_funptr (int_as_uint @-> int_as_uint @-> int_as_uint @->
              int_as_uint @-> int @-> ptr char @-> ptr void @->
              returning void)))
  
  let debug_message_callback =
    foreign ~stub "glDebugMessageCallback"
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
  
  let debug_message_control =
    foreign ~stub "glDebugMessageControl"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @->
       ba_opt_as_uint32p @-> bool @-> returning void)
  
  let debug_message_insert =
    foreign ~stub "glDebugMessageInsert"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int @-> string @-> returning void)
  
  let delete_buffers =
    foreign ~stub "glDeleteBuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_framebuffers =
    foreign ~stub "glDeleteFramebuffers"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_program =
    foreign ~stub "glDeleteProgram" (int_as_uint @-> returning void)
  
  let delete_program_pipelines =
    foreign ~stub "glDeleteProgramPipelines"
      (int @-> ba_as_uint32p @-> returning void)
  
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
  
  let delete_transform_feedbacks =
    foreign ~stub "glDeleteTransformFeedbacks"
      (int @-> ba_as_uint32p @-> returning void)
  
  let delete_vertex_arrays =
    foreign ~stub "glDeleteVertexArrays"
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
  
  let disablei =
    foreign ~stub "glDisablei"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let dispatch_compute =
    foreign ~stub "glDispatchCompute"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let dispatch_compute_indirect =
    foreign ~stub "glDispatchComputeIndirect" (int @-> returning void)
  
  let draw_arrays =
    foreign ~stub "glDrawArrays"
      (int_as_uint @-> int @-> int @-> returning void)
  
  let draw_arrays_indirect =
    foreign ~stub "glDrawArraysIndirect"
      (int_as_uint @-> (ptr void) @-> returning void)
  
  let draw_arrays_indirect mode indirect =
    let indirect = match indirect with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_arrays_indirect mode indirect
  
  let draw_arrays_instanced =
    foreign ~stub "glDrawArraysInstanced"
      (int_as_uint @-> int @-> int @-> int @-> returning void)
  
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
  
  let draw_elements_indirect =
    foreign ~stub "glDrawElementsIndirect"
      (int_as_uint @-> int_as_uint @-> (ptr void) @-> returning void)
  
  let draw_elements_indirect mode type_ indirect =
    let indirect = match indirect with
    | `Offset o -> ptr_of_raw_address (Nativeint.of_int o)
    | `Data b -> to_voidp (bigarray_start array1 b)
    in
    draw_elements_indirect mode type_ indirect
  
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
  
  let framebuffer_parameteri =
    foreign ~stub "glFramebufferParameteri"
      (int_as_uint @-> int_as_uint @-> int @-> returning void)
  
  let framebuffer_renderbuffer =
    foreign ~stub "glFramebufferRenderbuffer"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let framebuffer_texture =
    foreign ~stub "glFramebufferTexture"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @->
       returning void)
  
  let framebuffer_texture2d =
    foreign ~stub "glFramebufferTexture2D"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int @-> returning void)
  
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
  
  let gen_program_pipelines =
    foreign ~stub "glGenProgramPipelines"
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
  
  let gen_transform_feedbacks =
    foreign ~stub "glGenTransformFeedbacks"
      (int @-> ba_as_uint32p @-> returning void)
  
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
  
  let get_debug_message_log =
    foreign ~stub "glGetDebugMessageLog"
      (int_as_uint @-> int @-> ba_as_enump @-> ba_as_enump @->
       ba_opt_as_uint32p @-> ba_as_enump @-> ba_opt_as_int32p @->
       ba_opt_as_charp @-> returning int_as_uint)
  
  let get_error =
    foreign ~stub "glGetError" (void @-> returning int_as_uint)
  
  let get_floatv =
    foreign ~stub "glGetFloatv"
      (int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_frag_data_location =
    foreign ~stub "glGetFragDataLocation"
      (int_as_uint @-> string @-> returning int)
  
  let get_framebuffer_attachment_parameteriv =
    foreign ~stub "glGetFramebufferAttachmentParameteriv"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> ba_as_int32p @->
       returning void)
  
  let get_framebuffer_parameteriv =
    foreign ~stub "glGetFramebufferParameteriv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_graphics_reset_status =
    foreign ~stub "glGetGraphicsResetStatus" (void @-> returning int_as_uint)
  
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
  
  let get_internalformativ =
    foreign ~stub "glGetInternalformativ"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @->
       ba_as_int32p @-> returning void)
  
  let get_multisamplefv =
    foreign ~stub "glGetMultisamplefv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_object_label =
    foreign ~stub "glGetObjectLabel"
      (int_as_uint @-> int_as_uint @-> int @-> ba_opt_as_int32p @->
       ba_as_charp @-> returning void)
  
  let get_object_ptr_label =
    foreign ~stub "glGetObjectPtrLabel"
      ((ptr void) @-> int @-> ba_opt_as_int32p @-> ba_as_charp @->
       returning void)
  
  let get_object_ptr_label ptr bufSize length label =
    let ptr = to_voidp (bigarray_start array1 ptr) in
    get_object_ptr_label ptr bufSize length label
  
  let get_pointerv =
    foreign ~stub "glGetPointerv"
      (int_as_uint @-> ba_as_nativeint @-> returning void)
  
  let get_program_binary =
    foreign ~stub "glGetProgramBinary"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_enump @->
       (ptr void) @-> returning void)
  
  let get_program_binary program bufSize length binaryFormat binary =
    let binary = to_voidp (bigarray_start array1 binary) in
    get_program_binary program bufSize length binaryFormat binary
  
  let get_program_info_log =
    foreign ~stub "glGetProgramInfoLog"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp @->
       returning void)
  
  let get_program_interfaceiv =
    foreign ~stub "glGetProgramInterfaceiv"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> ba_as_int32p @->
       returning void)
  
  let get_program_pipeline_info_log =
    foreign ~stub "glGetProgramPipelineInfoLog"
      (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp @->
       returning void)
  
  let get_program_pipelineiv =
    foreign ~stub "glGetProgramPipelineiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let get_program_resource_index =
    foreign ~stub "glGetProgramResourceIndex"
      (int_as_uint @-> int_as_uint @-> string @-> returning int_as_uint)
  
  let get_program_resource_location =
    foreign ~stub "glGetProgramResourceLocation"
      (int_as_uint @-> int_as_uint @-> string @-> returning int)
  
  let get_program_resource_name =
    foreign ~stub "glGetProgramResourceName"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @->
       ba_opt_as_int32p @-> ba_as_charp @-> returning void)
  
  let get_program_resourceiv =
    foreign ~stub "glGetProgramResourceiv"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @->
       ba_as_enump @-> int @-> ba_as_int32p @-> ba_as_int32p @->
       returning void)
  
  let get_programiv =
    foreign ~stub "glGetProgramiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
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
  
  let get_stringi =
    foreign ~stub "glGetStringi"
      (int_as_uint @-> int_as_uint @-> returning string_opt)
  
  let get_synciv =
    foreign ~stub "glGetSynciv"
      (sync @-> int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_int32p @->
       returning void)
  
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
  
  let get_vertex_attribfv =
    foreign ~stub "glGetVertexAttribfv"
      (int_as_uint @-> int_as_uint @-> ba_as_float32p @-> returning void)
  
  let get_vertex_attribiv =
    foreign ~stub "glGetVertexAttribiv"
      (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)
  
  let getn_uniformfv =
    foreign ~stub "glGetnUniformfv"
      (int_as_uint @-> int @-> int @-> ba_as_float32p @-> returning void)
  
  let getn_uniformiv =
    foreign ~stub "glGetnUniformiv"
      (int_as_uint @-> int @-> int @-> ba_as_int32p @-> returning void)
  
  let getn_uniformuiv =
    foreign ~stub "glGetnUniformuiv"
      (int_as_uint @-> int @-> int @-> ba_as_uint32p @-> returning void)
  
  let hint =
    foreign ~stub "glHint" (int_as_uint @-> int_as_uint @-> returning void)
  
  let invalidate_framebuffer =
    foreign ~stub "glInvalidateFramebuffer"
      (int_as_uint @-> int @-> ba_as_enump @-> returning void)
  
  let invalidate_sub_framebuffer =
    foreign ~stub "glInvalidateSubFramebuffer"
      (int_as_uint @-> int @-> ba_as_enump @-> int @-> int @-> int @->
       int @-> returning void)
  
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
  
  let is_program_pipeline =
    foreign ~stub "glIsProgramPipeline" (int_as_uint @-> returning bool)
  
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
  
  let is_transform_feedback =
    foreign ~stub "glIsTransformFeedback" (int_as_uint @-> returning bool)
  
  let is_vertex_array =
    foreign ~stub "glIsVertexArray" (int_as_uint @-> returning bool)
  
  let line_width =
    foreign ~stub "glLineWidth" (float @-> returning void)
  
  let link_program =
    foreign ~stub "glLinkProgram" (int_as_uint @-> returning void)
  
  let map_buffer_range =
    foreign ~stub "glMapBufferRange"
      (int_as_uint @-> int @-> int @-> int_as_uint @-> returning (ptr void))
  
  let map_buffer_range target offset len access kind =
    let len_bytes = ba_kind_byte_size kind * len in
    let p = map_buffer_range target offset len_bytes access in
    let p = coerce (ptr void) (access_ptr_typ_of_ba_kind kind) p in
    bigarray_of_ptr array1 len kind p
  
  let memory_barrier =
    foreign ~stub "glMemoryBarrier" (int_as_uint @-> returning void)
  
  let memory_barrier_by_region =
    foreign ~stub "glMemoryBarrierByRegion" (int_as_uint @-> returning void)
  
  let min_sample_shading =
    foreign ~stub "glMinSampleShading" (float @-> returning void)
  
  let object_label =
    foreign ~stub "glObjectLabel"
      (int_as_uint @-> int_as_uint @-> int @-> string_opt @-> returning void)
  
  let object_ptr_label =
    foreign ~stub "glObjectPtrLabel"
      ((ptr void) @-> int @-> string_opt @-> returning void)
  
  let object_ptr_label ptr length label =
    let ptr = to_voidp (bigarray_start array1 ptr) in
    object_ptr_label ptr length label
  
  let patch_parameteri =
    foreign ~stub "glPatchParameteri"
      (int_as_uint @-> int @-> returning void)
  
  let pause_transform_feedback =
    foreign ~stub "glPauseTransformFeedback" (void @-> returning void)
  
  let pixel_storei =
    foreign ~stub "glPixelStorei" (int_as_uint @-> int @-> returning void)
  
  let polygon_offset =
    foreign ~stub "glPolygonOffset" (float @-> float @-> returning void)
  
  let pop_debug_group =
    foreign ~stub "glPopDebugGroup" (void @-> returning void)
  
  let primitive_bounding_box =
    foreign ~stub "glPrimitiveBoundingBox"
      (float @-> float @-> float @-> float @-> float @-> float @-> float @->
       float @-> returning void)
  
  let program_binary =
    foreign ~stub "glProgramBinary"
      (int_as_uint @-> int_as_uint @-> (ptr void) @-> int @-> returning void)
  
  let program_binary program binaryFormat binary length =
    let binary = to_voidp (bigarray_start array1 binary) in
    program_binary program binaryFormat binary length
  
  let program_parameteri =
    foreign ~stub "glProgramParameteri"
      (int_as_uint @-> int_as_uint @-> int @-> returning void)
  
  let program_uniform1f =
    foreign ~stub "glProgramUniform1f"
      (int_as_uint @-> int @-> float @-> returning void)
  
  let program_uniform1fv =
    foreign ~stub "glProgramUniform1fv"
      (int_as_uint @-> int @-> int @-> ba_as_float32p @-> returning void)
  
  let program_uniform1i =
    foreign ~stub "glProgramUniform1i"
      (int_as_uint @-> int @-> int @-> returning void)
  
  let program_uniform1iv =
    foreign ~stub "glProgramUniform1iv"
      (int_as_uint @-> int @-> int @-> ba_as_int32p @-> returning void)
  
  let program_uniform1ui =
    foreign ~stub "glProgramUniform1ui"
      (int_as_uint @-> int @-> int_as_uint @-> returning void)
  
  let program_uniform1uiv =
    foreign ~stub "glProgramUniform1uiv"
      (int_as_uint @-> int @-> int @-> ba_as_uint32p @-> returning void)
  
  let program_uniform2f =
    foreign ~stub "glProgramUniform2f"
      (int_as_uint @-> int @-> float @-> float @-> returning void)
  
  let program_uniform2fv =
    foreign ~stub "glProgramUniform2fv"
      (int_as_uint @-> int @-> int @-> ba_as_float32p @-> returning void)
  
  let program_uniform2i =
    foreign ~stub "glProgramUniform2i"
      (int_as_uint @-> int @-> int @-> int @-> returning void)
  
  let program_uniform2iv =
    foreign ~stub "glProgramUniform2iv"
      (int_as_uint @-> int @-> int @-> ba_as_int32p @-> returning void)
  
  let program_uniform2ui =
    foreign ~stub "glProgramUniform2ui"
      (int_as_uint @-> int @-> int_as_uint @-> int_as_uint @->
       returning void)
  
  let program_uniform2uiv =
    foreign ~stub "glProgramUniform2uiv"
      (int_as_uint @-> int @-> int @-> ba_as_uint32p @-> returning void)
  
  let program_uniform3f =
    foreign ~stub "glProgramUniform3f"
      (int_as_uint @-> int @-> float @-> float @-> float @-> returning void)
  
  let program_uniform3fv =
    foreign ~stub "glProgramUniform3fv"
      (int_as_uint @-> int @-> int @-> ba_as_float32p @-> returning void)
  
  let program_uniform3i =
    foreign ~stub "glProgramUniform3i"
      (int_as_uint @-> int @-> int @-> int @-> int @-> returning void)
  
  let program_uniform3iv =
    foreign ~stub "glProgramUniform3iv"
      (int_as_uint @-> int @-> int @-> ba_as_int32p @-> returning void)
  
  let program_uniform3ui =
    foreign ~stub "glProgramUniform3ui"
      (int_as_uint @-> int @-> int_as_uint @-> int_as_uint @->
       int_as_uint @-> returning void)
  
  let program_uniform3uiv =
    foreign ~stub "glProgramUniform3uiv"
      (int_as_uint @-> int @-> int @-> ba_as_uint32p @-> returning void)
  
  let program_uniform4f =
    foreign ~stub "glProgramUniform4f"
      (int_as_uint @-> int @-> float @-> float @-> float @-> float @->
       returning void)
  
  let program_uniform4fv =
    foreign ~stub "glProgramUniform4fv"
      (int_as_uint @-> int @-> int @-> ba_as_float32p @-> returning void)
  
  let program_uniform4i =
    foreign ~stub "glProgramUniform4i"
      (int_as_uint @-> int @-> int @-> int @-> int @-> int @->
       returning void)
  
  let program_uniform4iv =
    foreign ~stub "glProgramUniform4iv"
      (int_as_uint @-> int @-> int @-> ba_as_int32p @-> returning void)
  
  let program_uniform4ui =
    foreign ~stub "glProgramUniform4ui"
      (int_as_uint @-> int @-> int_as_uint @-> int_as_uint @->
       int_as_uint @-> int_as_uint @-> returning void)
  
  let program_uniform4uiv =
    foreign ~stub "glProgramUniform4uiv"
      (int_as_uint @-> int @-> int @-> ba_as_uint32p @-> returning void)
  
  let program_uniform_matrix2fv =
    foreign ~stub "glProgramUniformMatrix2fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let program_uniform_matrix2x3fv =
    foreign ~stub "glProgramUniformMatrix2x3fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let program_uniform_matrix2x4fv =
    foreign ~stub "glProgramUniformMatrix2x4fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let program_uniform_matrix3fv =
    foreign ~stub "glProgramUniformMatrix3fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let program_uniform_matrix3x2fv =
    foreign ~stub "glProgramUniformMatrix3x2fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let program_uniform_matrix3x4fv =
    foreign ~stub "glProgramUniformMatrix3x4fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let program_uniform_matrix4fv =
    foreign ~stub "glProgramUniformMatrix4fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let program_uniform_matrix4x2fv =
    foreign ~stub "glProgramUniformMatrix4x2fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let program_uniform_matrix4x3fv =
    foreign ~stub "glProgramUniformMatrix4x3fv"
      (int_as_uint @-> int @-> int @-> bool @-> ba_as_float32p @->
       returning void)
  
  let push_debug_group =
    foreign ~stub "glPushDebugGroup"
      (int_as_uint @-> int_as_uint @-> int @-> string @-> returning void)
  
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
  
  let readn_pixels =
    foreign ~stub "glReadnPixels"
      (int @-> int @-> int @-> int @-> int_as_uint @-> int_as_uint @->
       int @-> (ptr void) @-> returning void)
  
  let readn_pixels x y width height format type_ bufSize data =
    let data = to_voidp (bigarray_start array1 data) in
    readn_pixels x y width height format type_ bufSize data
  
  let release_shader_compiler =
    foreign ~stub "glReleaseShaderCompiler" (void @-> returning void)
  
  let renderbuffer_storage =
    foreign ~stub "glRenderbufferStorage"
      (int_as_uint @-> int_as_uint @-> int @-> int @-> returning void)
  
  let renderbuffer_storage_multisample =
    foreign ~stub "glRenderbufferStorageMultisample"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @->
       returning void)
  
  let resume_transform_feedback =
    foreign ~stub "glResumeTransformFeedback" (void @-> returning void)
  
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
  
  let tex_buffer =
    foreign ~stub "glTexBuffer"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let tex_buffer_range =
    foreign ~stub "glTexBufferRange"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int @-> int @->
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
  
  let tex_storage2d =
    foreign ~stub "glTexStorage2D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @->
       returning void)
  
  let tex_storage2d_multisample =
    foreign ~stub "glTexStorage2DMultisample"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> bool @->
       returning void)
  
  let tex_storage3d =
    foreign ~stub "glTexStorage3D"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       returning void)
  
  let tex_storage3d_multisample =
    foreign ~stub "glTexStorage3DMultisample"
      (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> int @->
       bool @-> returning void)
  
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
  
  let use_program_stages =
    foreign ~stub "glUseProgramStages"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> returning void)
  
  let validate_program =
    foreign ~stub "glValidateProgram" (int_as_uint @-> returning void)
  
  let validate_program_pipeline =
    foreign ~stub "glValidateProgramPipeline"
      (int_as_uint @-> returning void)
  
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
  
  let vertex_attrib_binding =
    foreign ~stub "glVertexAttribBinding"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let vertex_attrib_divisor =
    foreign ~stub "glVertexAttribDivisor"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let vertex_attrib_format =
    foreign ~stub "glVertexAttribFormat"
      (int_as_uint @-> int @-> int_as_uint @-> bool @-> int_as_uint @->
       returning void)
  
  let vertex_attrib_i4i =
    foreign ~stub "glVertexAttribI4i"
      (int_as_uint @-> int @-> int @-> int @-> int @-> returning void)
  
  let vertex_attrib_i4iv =
    foreign ~stub "glVertexAttribI4iv"
      (int_as_uint @-> ba_as_int32p @-> returning void)
  
  let vertex_attrib_i4ui =
    foreign ~stub "glVertexAttribI4ui"
      (int_as_uint @-> int_as_uint @-> int_as_uint @-> int_as_uint @->
       int_as_uint @-> returning void)
  
  let vertex_attrib_i4uiv =
    foreign ~stub "glVertexAttribI4uiv"
      (int_as_uint @-> ba_as_uint32p @-> returning void)
  
  let vertex_attrib_iformat =
    foreign ~stub "glVertexAttribIFormat"
      (int_as_uint @-> int @-> int_as_uint @-> int_as_uint @->
       returning void)
  
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
  
  let vertex_binding_divisor =
    foreign ~stub "glVertexBindingDivisor"
      (int_as_uint @-> int_as_uint @-> returning void)
  
  let viewport =
    foreign ~stub "glViewport"
      (int @-> int @-> int @-> int @-> returning void)
  
  let wait_sync =
    foreign ~stub "glWaitSync"
      (sync @-> int_as_uint @-> int64_as_uint64_t @-> returning void)
  

  (* Enums *)

  let active_atomic_counter_buffers = 0x92D9
  let active_attributes = 0x8B89
  let active_attribute_max_length = 0x8B8A
  let active_program = 0x8259
  let active_resources = 0x92F5
  let active_texture_enum = 0x84E0
  let active_uniforms = 0x8B86
  let active_uniform_blocks = 0x8A36
  let active_uniform_block_max_name_length = 0x8A35
  let active_uniform_max_length = 0x8B87
  let active_variables = 0x9305
  let aliased_line_width_range = 0x846E
  let aliased_point_size_range = 0x846D
  let all_barrier_bits = Int32.to_int 0xFFFF_FFFFl
  let all_shader_bits = Int32.to_int 0xFFFF_FFFFl
  let alpha = 0x1906
  let alpha_bits = 0xD55
  let already_signaled = 0x911A
  let always = 0x207
  let any_samples_passed = 0x8C2F
  let any_samples_passed_conservative = 0x8D6A
  let array_buffer = 0x8892
  let array_buffer_binding = 0x8894
  let array_size = 0x92FB
  let array_stride = 0x92FE
  let atomic_counter_barrier_bit = 0x1000
  let atomic_counter_buffer = 0x92C0
  let atomic_counter_buffer_binding = 0x92C1
  let atomic_counter_buffer_index = 0x9301
  let atomic_counter_buffer_size = 0x92C3
  let atomic_counter_buffer_start = 0x92C2
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
  let block_index = 0x92FD
  let blue = 0x1905
  let blue_bits = 0xD54
  let bool = 0x8B56
  let bool_vec2 = 0x8B57
  let bool_vec3 = 0x8B58
  let bool_vec4 = 0x8B59
  let buffer = 0x82E0
  let buffer_access_flags = 0x911F
  let buffer_binding = 0x9302
  let buffer_data_size = 0x9303
  let buffer_mapped = 0x88BC
  let buffer_map_length = 0x9120
  let buffer_map_offset = 0x9121
  let buffer_map_pointer = 0x88BD
  let buffer_size = 0x8764
  let buffer_update_barrier_bit = 0x200
  let buffer_usage = 0x8765
  let buffer_variable = 0x92E5
  let byte = 0x1400
  let ccw = 0x901
  let clamp_to_border = 0x812D
  let clamp_to_edge = 0x812F
  let color = 0x1800
  let colorburn = 0x929A
  let colordodge = 0x9299
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
  let color_writemask = 0xC23
  let command_barrier_bit = 0x40
  let compare_ref_to_texture = 0x884E
  let compile_status = 0x8B81
  let compressed_r11_eac = 0x9270
  let compressed_rg11_eac = 0x9272
  let compressed_rgb8_etc2 = 0x9274
  let compressed_rgb8_punchthrough_alpha1_etc2 = 0x9276
  let compressed_rgba8_etc2_eac = 0x9278
  let compressed_rgba_astc_10x10 = 0x93BB
  let compressed_rgba_astc_10x5 = 0x93B8
  let compressed_rgba_astc_10x6 = 0x93B9
  let compressed_rgba_astc_10x8 = 0x93BA
  let compressed_rgba_astc_12x10 = 0x93BC
  let compressed_rgba_astc_12x12 = 0x93BD
  let compressed_rgba_astc_4x4 = 0x93B0
  let compressed_rgba_astc_5x4 = 0x93B1
  let compressed_rgba_astc_5x5 = 0x93B2
  let compressed_rgba_astc_6x5 = 0x93B3
  let compressed_rgba_astc_6x6 = 0x93B4
  let compressed_rgba_astc_8x5 = 0x93B5
  let compressed_rgba_astc_8x6 = 0x93B6
  let compressed_rgba_astc_8x8 = 0x93B7
  let compressed_signed_r11_eac = 0x9271
  let compressed_signed_rg11_eac = 0x9273
  let compressed_srgb8_alpha8_astc_10x10 = 0x93DB
  let compressed_srgb8_alpha8_astc_10x5 = 0x93D8
  let compressed_srgb8_alpha8_astc_10x6 = 0x93D9
  let compressed_srgb8_alpha8_astc_10x8 = 0x93DA
  let compressed_srgb8_alpha8_astc_12x10 = 0x93DC
  let compressed_srgb8_alpha8_astc_12x12 = 0x93DD
  let compressed_srgb8_alpha8_astc_4x4 = 0x93D0
  let compressed_srgb8_alpha8_astc_5x4 = 0x93D1
  let compressed_srgb8_alpha8_astc_5x5 = 0x93D2
  let compressed_srgb8_alpha8_astc_6x5 = 0x93D3
  let compressed_srgb8_alpha8_astc_6x6 = 0x93D4
  let compressed_srgb8_alpha8_astc_8x5 = 0x93D5
  let compressed_srgb8_alpha8_astc_8x6 = 0x93D6
  let compressed_srgb8_alpha8_astc_8x8 = 0x93D7
  let compressed_srgb8_alpha8_etc2_eac = 0x9279
  let compressed_srgb8_etc2 = 0x9275
  let compressed_srgb8_punchthrough_alpha1_etc2 = 0x9277
  let compressed_texture_formats = 0x86A3
  let compute_shader = 0x91B9
  let compute_shader_bit = 0x20
  let compute_work_group_size = 0x8267
  let condition_satisfied = 0x911C
  let constant_alpha = 0x8003
  let constant_color = 0x8001
  let context_flags = 0x821E
  let context_flag_debug_bit = 0x2
  let context_flag_robust_access_bit = 0x4
  let context_lost = 0x507
  let copy_read_buffer = 0x8F36
  let copy_read_buffer_binding = 0x8F36
  let copy_write_buffer = 0x8F37
  let copy_write_buffer_binding = 0x8F37
  let cull_face_enum = 0xB44
  let cull_face_mode = 0xB45
  let current_program = 0x8B8D
  let current_query = 0x8865
  let current_vertex_attrib = 0x8626
  let cw = 0x900
  let darken = 0x9297
  let debug_callback_function = 0x8244
  let debug_callback_user_param = 0x8245
  let debug_group_stack_depth = 0x826D
  let debug_logged_messages = 0x9145
  let debug_next_logged_message_length = 0x8243
  let debug_output = 0x92E0
  let debug_output_synchronous = 0x8242
  let debug_severity_high = 0x9146
  let debug_severity_low = 0x9148
  let debug_severity_medium = 0x9147
  let debug_severity_notification = 0x826B
  let debug_source_api = 0x8246
  let debug_source_application = 0x824A
  let debug_source_other = 0x824B
  let debug_source_shader_compiler = 0x8248
  let debug_source_third_party = 0x8249
  let debug_source_window_system = 0x8247
  let debug_type_deprecated_behavior = 0x824D
  let debug_type_error = 0x824C
  let debug_type_marker = 0x8268
  let debug_type_other = 0x8251
  let debug_type_performance = 0x8250
  let debug_type_pop_group = 0x826A
  let debug_type_portability = 0x824F
  let debug_type_push_group = 0x8269
  let debug_type_undefined_behavior = 0x824E
  let decr = 0x1E03
  let decr_wrap = 0x8508
  let delete_status = 0x8B80
  let depth = 0x1801
  let depth24_stencil8 = 0x88F0
  let depth32f_stencil8 = 0x8CAD
  let depth_attachment = 0x8D00
  let depth_bits = 0xD56
  let depth_buffer_bit = 0x100
  let depth_clear_value = 0xB73
  let depth_component = 0x1902
  let depth_component16 = 0x81A5
  let depth_component24 = 0x81A6
  let depth_component32f = 0x8CAC
  let depth_func_enum = 0xB74
  let depth_range = 0xB70
  let depth_stencil = 0x84F9
  let depth_stencil_attachment = 0x821A
  let depth_stencil_texture_mode = 0x90EA
  let depth_test = 0xB71
  let depth_writemask = 0xB72
  let difference = 0x929E
  let dispatch_indirect_buffer = 0x90EE
  let dispatch_indirect_buffer_binding = 0x90EF
  let dither = 0xBD0
  let dont_care = 0x1100
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
  let draw_indirect_buffer = 0x8F3F
  let draw_indirect_buffer_binding = 0x8F43
  let dst_alpha = 0x304
  let dst_color = 0x306
  let dynamic_copy = 0x88EA
  let dynamic_draw = 0x88E8
  let dynamic_read = 0x88E9
  let element_array_barrier_bit = 0x2
  let element_array_buffer = 0x8893
  let element_array_buffer_binding = 0x8895
  let equal = 0x202
  let exclusion = 0x92A0
  let extensions = 0x1F03
  let false_ = 0x0
  let fastest = 0x1101
  let first_vertex_convention = 0x8E4D
  let fixed = 0x140C
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
  let fractional_even = 0x8E7C
  let fractional_odd = 0x8E7B
  let fragment_interpolation_offset_bits = 0x8E5D
  let fragment_shader = 0x8B30
  let fragment_shader_bit = 0x2
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
  let framebuffer_barrier_bit = 0x400
  let framebuffer_binding = 0x8CA6
  let framebuffer_complete = 0x8CD5
  let framebuffer_default = 0x8218
  let framebuffer_default_fixed_sample_locations = 0x9314
  let framebuffer_default_height = 0x9311
  let framebuffer_default_layers = 0x9312
  let framebuffer_default_samples = 0x9313
  let framebuffer_default_width = 0x9310
  let framebuffer_incomplete_attachment = 0x8CD6
  let framebuffer_incomplete_dimensions = 0x8CD9
  let framebuffer_incomplete_layer_targets = 0x8DA8
  let framebuffer_incomplete_missing_attachment = 0x8CD7
  let framebuffer_incomplete_multisample = 0x8D56
  let framebuffer_undefined = 0x8219
  let framebuffer_unsupported = 0x8CDD
  let front = 0x404
  let front_and_back = 0x408
  let front_face_enum = 0xB46
  let func_add = 0x8006
  let func_reverse_subtract = 0x800B
  let func_subtract = 0x800A
  let generate_mipmap_hint = 0x8192
  let geometry_input_type = 0x8917
  let geometry_output_type = 0x8918
  let geometry_shader = 0x8DD9
  let geometry_shader_bit = 0x4
  let geometry_shader_invocations = 0x887F
  let geometry_vertices_out = 0x8916
  let gequal = 0x206
  let greater = 0x204
  let green = 0x1904
  let green_bits = 0xD53
  let guilty_context_reset = 0x8253
  let half_float = 0x140B
  let hardlight = 0x929B
  let high_float = 0x8DF2
  let high_int = 0x8DF5
  let hsl_color = 0x92AF
  let hsl_hue = 0x92AD
  let hsl_luminosity = 0x92B0
  let hsl_saturation = 0x92AE
  let image_2d = 0x904D
  let image_2d_array = 0x9053
  let image_3d = 0x904E
  let image_binding_access = 0x8F3E
  let image_binding_format = 0x906E
  let image_binding_layer = 0x8F3D
  let image_binding_layered = 0x8F3C
  let image_binding_level = 0x8F3B
  let image_binding_name = 0x8F3A
  let image_buffer = 0x9051
  let image_cube = 0x9050
  let image_cube_map_array = 0x9054
  let image_format_compatibility_by_class = 0x90C9
  let image_format_compatibility_by_size = 0x90C8
  let image_format_compatibility_type = 0x90C7
  let implementation_color_read_format = 0x8B9B
  let implementation_color_read_type = 0x8B9A
  let incr = 0x1E02
  let incr_wrap = 0x8507
  let info_log_length = 0x8B84
  let innocent_context_reset = 0x8254
  let int = 0x1404
  let interleaved_attribs = 0x8C8C
  let int_2_10_10_10_rev = 0x8D9F
  let int_image_2d = 0x9058
  let int_image_2d_array = 0x905E
  let int_image_3d = 0x9059
  let int_image_buffer = 0x905C
  let int_image_cube = 0x905B
  let int_image_cube_map_array = 0x905F
  let int_sampler_2d = 0x8DCA
  let int_sampler_2d_array = 0x8DCF
  let int_sampler_2d_multisample = 0x9109
  let int_sampler_2d_multisample_array = 0x910C
  let int_sampler_3d = 0x8DCB
  let int_sampler_buffer = 0x8DD0
  let int_sampler_cube = 0x8DCC
  let int_sampler_cube_map_array = 0x900E
  let int_vec2 = 0x8B53
  let int_vec3 = 0x8B54
  let int_vec4 = 0x8B55
  let invalid_enum = 0x500
  let invalid_framebuffer_operation = 0x506
  let invalid_index = 0xFFFFFFFFl
  let invalid_operation = 0x502
  let invalid_value = 0x501
  let invert = 0x150A
  let isolines = 0x8E7A
  let is_per_patch = 0x92E7
  let is_row_major = 0x9300
  let keep = 0x1E00
  let last_vertex_convention = 0x8E4E
  let layer_provoking_vertex = 0x825E
  let lequal = 0x203
  let less = 0x201
  let lighten = 0x9298
  let linear = 0x2601
  let linear_mipmap_linear = 0x2703
  let linear_mipmap_nearest = 0x2701
  let lines = 0x1
  let lines_adjacency = 0xA
  let line_loop = 0x2
  let line_strip = 0x3
  let line_strip_adjacency = 0xB
  let line_width_enum = 0xB21
  let link_status = 0x8B82
  let location = 0x930E
  let lose_context_on_reset = 0x8252
  let low_float = 0x8DF0
  let low_int = 0x8DF3
  let luminance = 0x1909
  let luminance_alpha = 0x190A
  let major_version = 0x821B
  let map_flush_explicit_bit = 0x10
  let map_invalidate_buffer_bit = 0x8
  let map_invalidate_range_bit = 0x4
  let map_read_bit = 0x1
  let map_unsynchronized_bit = 0x20
  let map_write_bit = 0x2
  let matrix_stride = 0x92FF
  let max = 0x8008
  let max_3d_texture_size = 0x8073
  let max_array_texture_layers = 0x88FF
  let max_atomic_counter_buffer_bindings = 0x92DC
  let max_atomic_counter_buffer_size = 0x92D8
  let max_color_attachments = 0x8CDF
  let max_color_texture_samples = 0x910E
  let max_combined_atomic_counters = 0x92D7
  let max_combined_atomic_counter_buffers = 0x92D1
  let max_combined_compute_uniform_components = 0x8266
  let max_combined_fragment_uniform_components = 0x8A33
  let max_combined_geometry_uniform_components = 0x8A32
  let max_combined_image_uniforms = 0x90CF
  let max_combined_shader_output_resources = 0x8F39
  let max_combined_shader_storage_blocks = 0x90DC
  let max_combined_tess_control_uniform_components = 0x8E1E
  let max_combined_tess_evaluation_uniform_components = 0x8E1F
  let max_combined_texture_image_units = 0x8B4D
  let max_combined_uniform_blocks = 0x8A2E
  let max_combined_vertex_uniform_components = 0x8A31
  let max_compute_atomic_counters = 0x8265
  let max_compute_atomic_counter_buffers = 0x8264
  let max_compute_image_uniforms = 0x91BD
  let max_compute_shader_storage_blocks = 0x90DB
  let max_compute_shared_memory_size = 0x8262
  let max_compute_texture_image_units = 0x91BC
  let max_compute_uniform_blocks = 0x91BB
  let max_compute_uniform_components = 0x8263
  let max_compute_work_group_count = 0x91BE
  let max_compute_work_group_invocations = 0x90EB
  let max_compute_work_group_size = 0x91BF
  let max_cube_map_texture_size = 0x851C
  let max_debug_group_stack_depth = 0x826C
  let max_debug_logged_messages = 0x9144
  let max_debug_message_length = 0x9143
  let max_depth_texture_samples = 0x910F
  let max_draw_buffers = 0x8824
  let max_elements_indices = 0x80E9
  let max_elements_vertices = 0x80E8
  let max_element_index = 0x8D6B
  let max_fragment_atomic_counters = 0x92D6
  let max_fragment_atomic_counter_buffers = 0x92D0
  let max_fragment_image_uniforms = 0x90CE
  let max_fragment_input_components = 0x9125
  let max_fragment_interpolation_offset = 0x8E5C
  let max_fragment_shader_storage_blocks = 0x90DA
  let max_fragment_uniform_blocks = 0x8A2D
  let max_fragment_uniform_components = 0x8B49
  let max_fragment_uniform_vectors = 0x8DFD
  let max_framebuffer_height = 0x9316
  let max_framebuffer_layers = 0x9317
  let max_framebuffer_samples = 0x9318
  let max_framebuffer_width = 0x9315
  let max_geometry_atomic_counters = 0x92D5
  let max_geometry_atomic_counter_buffers = 0x92CF
  let max_geometry_image_uniforms = 0x90CD
  let max_geometry_input_components = 0x9123
  let max_geometry_output_components = 0x9124
  let max_geometry_output_vertices = 0x8DE0
  let max_geometry_shader_invocations = 0x8E5A
  let max_geometry_shader_storage_blocks = 0x90D7
  let max_geometry_texture_image_units = 0x8C29
  let max_geometry_total_output_components = 0x8DE1
  let max_geometry_uniform_blocks = 0x8A2C
  let max_geometry_uniform_components = 0x8DDF
  let max_image_units = 0x8F38
  let max_integer_samples = 0x9110
  let max_label_length = 0x82E8
  let max_name_length = 0x92F6
  let max_num_active_variables = 0x92F7
  let max_patch_vertices = 0x8E7D
  let max_program_texel_offset = 0x8905
  let max_program_texture_gather_offset = 0x8E5F
  let max_renderbuffer_size = 0x84E8
  let max_samples = 0x8D57
  let max_sample_mask_words = 0x8E59
  let max_server_wait_timeout = 0x9111
  let max_shader_storage_block_size = 0x90DE
  let max_shader_storage_buffer_bindings = 0x90DD
  let max_tess_control_atomic_counters = 0x92D3
  let max_tess_control_atomic_counter_buffers = 0x92CD
  let max_tess_control_image_uniforms = 0x90CB
  let max_tess_control_input_components = 0x886C
  let max_tess_control_output_components = 0x8E83
  let max_tess_control_shader_storage_blocks = 0x90D8
  let max_tess_control_texture_image_units = 0x8E81
  let max_tess_control_total_output_components = 0x8E85
  let max_tess_control_uniform_blocks = 0x8E89
  let max_tess_control_uniform_components = 0x8E7F
  let max_tess_evaluation_atomic_counters = 0x92D4
  let max_tess_evaluation_atomic_counter_buffers = 0x92CE
  let max_tess_evaluation_image_uniforms = 0x90CC
  let max_tess_evaluation_input_components = 0x886D
  let max_tess_evaluation_output_components = 0x8E86
  let max_tess_evaluation_shader_storage_blocks = 0x90D9
  let max_tess_evaluation_texture_image_units = 0x8E82
  let max_tess_evaluation_uniform_blocks = 0x8E8A
  let max_tess_evaluation_uniform_components = 0x8E80
  let max_tess_gen_level = 0x8E7E
  let max_tess_patch_components = 0x8E84
  let max_texture_buffer_size = 0x8C2B
  let max_texture_image_units = 0x8872
  let max_texture_lod_bias = 0x84FD
  let max_texture_size = 0xD33
  let max_transform_feedback_interleaved_components = 0x8C8A
  let max_transform_feedback_separate_attribs = 0x8C8B
  let max_transform_feedback_separate_components = 0x8C80
  let max_uniform_block_size = 0x8A30
  let max_uniform_buffer_bindings = 0x8A2F
  let max_uniform_locations = 0x826E
  let max_varying_components = 0x8B4B
  let max_varying_vectors = 0x8DFC
  let max_vertex_atomic_counters = 0x92D2
  let max_vertex_atomic_counter_buffers = 0x92CC
  let max_vertex_attribs = 0x8869
  let max_vertex_attrib_bindings = 0x82DA
  let max_vertex_attrib_relative_offset = 0x82D9
  let max_vertex_attrib_stride = 0x82E5
  let max_vertex_image_uniforms = 0x90CA
  let max_vertex_output_components = 0x9122
  let max_vertex_shader_storage_blocks = 0x90D6
  let max_vertex_texture_image_units = 0x8B4C
  let max_vertex_uniform_blocks = 0x8A2B
  let max_vertex_uniform_components = 0x8B4A
  let max_vertex_uniform_vectors = 0x8DFB
  let max_viewport_dims = 0xD3A
  let medium_float = 0x8DF1
  let medium_int = 0x8DF4
  let min = 0x8007
  let minor_version = 0x821C
  let min_fragment_interpolation_offset = 0x8E5B
  let min_program_texel_offset = 0x8904
  let min_program_texture_gather_offset = 0x8E5E
  let min_sample_shading_value = 0x8C37
  let mirrored_repeat = 0x8370
  let multiply = 0x9294
  let multisample_line_width_granularity = 0x9382
  let multisample_line_width_range = 0x9381
  let name_length = 0x92F9
  let nearest = 0x2600
  let nearest_mipmap_linear = 0x2702
  let nearest_mipmap_nearest = 0x2700
  let never = 0x200
  let nicest = 0x1102
  let none = 0x0
  let notequal = 0x205
  let no_error = 0x0
  let no_reset_notification = 0x8261
  let num_active_variables = 0x9304
  let num_compressed_texture_formats = 0x86A2
  let num_extensions = 0x821D
  let num_program_binary_formats = 0x87FE
  let num_sample_counts = 0x9380
  let num_shader_binary_formats = 0x8DF9
  let object_type = 0x9112
  let offset = 0x92FC
  let one = 0x1
  let one_minus_constant_alpha = 0x8004
  let one_minus_constant_color = 0x8002
  let one_minus_dst_alpha = 0x305
  let one_minus_dst_color = 0x307
  let one_minus_src_alpha = 0x303
  let one_minus_src_color = 0x301
  let out_of_memory = 0x505
  let overlay = 0x9296
  let pack_alignment = 0xD05
  let pack_row_length = 0xD02
  let pack_skip_pixels = 0xD04
  let pack_skip_rows = 0xD03
  let patches = 0xE
  let patch_vertices = 0x8E72
  let pixel_buffer_barrier_bit = 0x80
  let pixel_pack_buffer = 0x88EB
  let pixel_pack_buffer_binding = 0x88ED
  let pixel_unpack_buffer = 0x88EC
  let pixel_unpack_buffer_binding = 0x88EF
  let points = 0x0
  let polygon_offset_factor = 0x8038
  let polygon_offset_fill = 0x8037
  let polygon_offset_units = 0x2A00
  let primitives_generated = 0x8C87
  let primitive_bounding_box_enum = 0x92BE
  let primitive_restart_fixed_index = 0x8D69
  let primitive_restart_for_patches_supported = 0x8221
  let program = 0x82E2
  let program_binary_formats = 0x87FF
  let program_binary_length = 0x8741
  let program_binary_retrievable_hint = 0x8257
  let program_input = 0x92E3
  let program_output = 0x92E4
  let program_pipeline = 0x82E4
  let program_pipeline_binding = 0x825A
  let program_separable = 0x8258
  let quads = 0x7
  let query = 0x82E3
  let query_result = 0x8866
  let query_result_available = 0x8867
  let r11f_g11f_b10f = 0x8C3A
  let r16f = 0x822D
  let r16i = 0x8233
  let r16ui = 0x8234
  let r32f = 0x822E
  let r32i = 0x8235
  let r32ui = 0x8236
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
  let red_bits = 0xD52
  let red_integer = 0x8D94
  let referenced_by_compute_shader = 0x930B
  let referenced_by_fragment_shader = 0x930A
  let referenced_by_geometry_shader = 0x9309
  let referenced_by_tess_control_shader = 0x9307
  let referenced_by_tess_evaluation_shader = 0x9308
  let referenced_by_vertex_shader = 0x9306
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
  let reset_notification_strategy = 0x8256
  let rg = 0x8227
  let rg16f = 0x822F
  let rg16i = 0x8239
  let rg16ui = 0x823A
  let rg32f = 0x8230
  let rg32i = 0x823B
  let rg32ui = 0x823C
  let rg8 = 0x822B
  let rg8i = 0x8237
  let rg8ui = 0x8238
  let rg8_snorm = 0x8F95
  let rgb = 0x1907
  let rgb10_a2 = 0x8059
  let rgb10_a2ui = 0x906F
  let rgb16f = 0x881B
  let rgb16i = 0x8D89
  let rgb16ui = 0x8D77
  let rgb32f = 0x8815
  let rgb32i = 0x8D83
  let rgb32ui = 0x8D71
  let rgb565 = 0x8D62
  let rgb5_a1 = 0x8057
  let rgb8 = 0x8051
  let rgb8i = 0x8D8F
  let rgb8ui = 0x8D7D
  let rgb8_snorm = 0x8F96
  let rgb9_e5 = 0x8C3D
  let rgba = 0x1908
  let rgba16f = 0x881A
  let rgba16i = 0x8D88
  let rgba16ui = 0x8D76
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
  let sampler = 0x82E6
  let sampler_2d = 0x8B5E
  let sampler_2d_array = 0x8DC1
  let sampler_2d_array_shadow = 0x8DC4
  let sampler_2d_multisample = 0x9108
  let sampler_2d_multisample_array = 0x910B
  let sampler_2d_shadow = 0x8B62
  let sampler_3d = 0x8B5F
  let sampler_binding = 0x8919
  let sampler_buffer = 0x8DC2
  let sampler_cube = 0x8B60
  let sampler_cube_map_array = 0x900C
  let sampler_cube_map_array_shadow = 0x900D
  let sampler_cube_shadow = 0x8DC5
  let samples = 0x80A9
  let sample_alpha_to_coverage = 0x809E
  let sample_buffers = 0x80A8
  let sample_coverage_enum = 0x80A0
  let sample_coverage_invert = 0x80AB
  let sample_coverage_value = 0x80AA
  let sample_mask = 0x8E51
  let sample_mask_value = 0x8E52
  let sample_position = 0x8E50
  let sample_shading = 0x8C36
  let scissor_box = 0xC10
  let scissor_test = 0xC11
  let screen = 0x9295
  let separate_attribs = 0x8C8D
  let shader = 0x82E1
  let shader_binary_formats = 0x8DF8
  let shader_compiler = 0x8DFA
  let shader_image_access_barrier_bit = 0x20
  let shader_source_length = 0x8B88
  let shader_storage_barrier_bit = 0x2000
  let shader_storage_block = 0x92E6
  let shader_storage_buffer = 0x90D2
  let shader_storage_buffer_binding = 0x90D3
  let shader_storage_buffer_offset_alignment = 0x90DF
  let shader_storage_buffer_size = 0x90D5
  let shader_storage_buffer_start = 0x90D4
  let shader_type = 0x8B4F
  let shading_language_version = 0x8B8C
  let short = 0x1402
  let signaled = 0x9119
  let signed_normalized = 0x8F9C
  let softlight = 0x929C
  let src_alpha = 0x302
  let src_alpha_saturate = 0x308
  let src_color = 0x300
  let srgb = 0x8C40
  let srgb8 = 0x8C41
  let srgb8_alpha8 = 0x8C43
  let stack_overflow = 0x503
  let stack_underflow = 0x504
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
  let stencil_bits = 0xD57
  let stencil_buffer_bit = 0x400
  let stencil_clear_value = 0xB91
  let stencil_fail = 0xB94
  let stencil_func_enum = 0xB92
  let stencil_index = 0x1901
  let stencil_index8 = 0x8D48
  let stencil_pass_depth_fail = 0xB95
  let stencil_pass_depth_pass = 0xB96
  let stencil_ref = 0xB97
  let stencil_test = 0xB90
  let stencil_value_mask = 0xB93
  let stencil_writemask = 0xB98
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
  let tess_control_output_vertices = 0x8E75
  let tess_control_shader = 0x8E88
  let tess_control_shader_bit = 0x8
  let tess_evaluation_shader = 0x8E87
  let tess_evaluation_shader_bit = 0x10
  let tess_gen_mode = 0x8E76
  let tess_gen_point_mode = 0x8E79
  let tess_gen_spacing = 0x8E77
  let tess_gen_vertex_order = 0x8E78
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
  let texture_2d_array = 0x8C1A
  let texture_2d_multisample = 0x9100
  let texture_2d_multisample_array = 0x9102
  let texture_3d = 0x806F
  let texture_alpha_size = 0x805F
  let texture_alpha_type = 0x8C13
  let texture_base_level = 0x813C
  let texture_binding_2d = 0x8069
  let texture_binding_2d_array = 0x8C1D
  let texture_binding_2d_multisample = 0x9104
  let texture_binding_2d_multisample_array = 0x9105
  let texture_binding_3d = 0x806A
  let texture_binding_buffer = 0x8C2C
  let texture_binding_cube_map = 0x8514
  let texture_binding_cube_map_array = 0x900A
  let texture_blue_size = 0x805E
  let texture_blue_type = 0x8C12
  let texture_border_color = 0x1004
  let texture_buffer = 0x8C2A
  let texture_buffer_binding = 0x8C2A
  let texture_buffer_data_store_binding = 0x8C2D
  let texture_buffer_offset = 0x919D
  let texture_buffer_offset_alignment = 0x919F
  let texture_buffer_size = 0x919E
  let texture_compare_func = 0x884D
  let texture_compare_mode = 0x884C
  let texture_compressed = 0x86A1
  let texture_cube_map = 0x8513
  let texture_cube_map_array = 0x9009
  let texture_cube_map_negative_x = 0x8516
  let texture_cube_map_negative_y = 0x8518
  let texture_cube_map_negative_z = 0x851A
  let texture_cube_map_positive_x = 0x8515
  let texture_cube_map_positive_y = 0x8517
  let texture_cube_map_positive_z = 0x8519
  let texture_depth = 0x8071
  let texture_depth_size = 0x884A
  let texture_depth_type = 0x8C16
  let texture_fetch_barrier_bit = 0x8
  let texture_fixed_sample_locations = 0x9107
  let texture_green_size = 0x805D
  let texture_green_type = 0x8C11
  let texture_height = 0x1001
  let texture_immutable_format = 0x912F
  let texture_immutable_levels = 0x82DF
  let texture_internal_format = 0x1003
  let texture_mag_filter = 0x2800
  let texture_max_level = 0x813D
  let texture_max_lod = 0x813B
  let texture_min_filter = 0x2801
  let texture_min_lod = 0x813A
  let texture_red_size = 0x805C
  let texture_red_type = 0x8C10
  let texture_samples = 0x9106
  let texture_shared_size = 0x8C3F
  let texture_stencil_size = 0x88F1
  let texture_swizzle_a = 0x8E45
  let texture_swizzle_b = 0x8E44
  let texture_swizzle_g = 0x8E43
  let texture_swizzle_r = 0x8E42
  let texture_update_barrier_bit = 0x100
  let texture_width = 0x1000
  let texture_wrap_r = 0x8072
  let texture_wrap_s = 0x2802
  let texture_wrap_t = 0x2803
  let timeout_expired = 0x911B
  let timeout_ignored = 0xFFFFFFFFFFFFFFFFL
  let top_level_array_size = 0x930C
  let top_level_array_stride = 0x930D
  let transform_feedback = 0x8E22
  let transform_feedback_active = 0x8E24
  let transform_feedback_barrier_bit = 0x800
  let transform_feedback_binding = 0x8E25
  let transform_feedback_buffer = 0x8C8E
  let transform_feedback_buffer_binding = 0x8C8F
  let transform_feedback_buffer_mode = 0x8C7F
  let transform_feedback_buffer_size = 0x8C85
  let transform_feedback_buffer_start = 0x8C84
  let transform_feedback_paused = 0x8E23
  let transform_feedback_primitives_written = 0x8C88
  let transform_feedback_varying = 0x92F4
  let transform_feedback_varyings_enum = 0x8C83
  let transform_feedback_varying_max_length = 0x8C76
  let triangles = 0x4
  let triangles_adjacency = 0xC
  let triangle_fan = 0x6
  let triangle_strip = 0x5
  let triangle_strip_adjacency = 0xD
  let true_ = 0x1
  let type_ = 0x92FA
  let undefined_vertex = 0x8260
  let uniform = 0x92E1
  let uniform_array_stride = 0x8A3C
  let uniform_barrier_bit = 0x4
  let uniform_block = 0x92E2
  let uniform_block_active_uniforms = 0x8A42
  let uniform_block_active_uniform_indices = 0x8A43
  let uniform_block_binding_enum = 0x8A3F
  let uniform_block_data_size = 0x8A40
  let uniform_block_index = 0x8A3A
  let uniform_block_name_length = 0x8A41
  let uniform_block_referenced_by_fragment_shader = 0x8A46
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
  let unknown_context_reset = 0x8255
  let unpack_alignment = 0xCF5
  let unpack_image_height = 0x806E
  let unpack_row_length = 0xCF2
  let unpack_skip_images = 0x806D
  let unpack_skip_pixels = 0xCF4
  let unpack_skip_rows = 0xCF3
  let unsignaled = 0x9118
  let unsigned_byte = 0x1401
  let unsigned_int = 0x1405
  let unsigned_int_10f_11f_11f_rev = 0x8C3B
  let unsigned_int_24_8 = 0x84FA
  let unsigned_int_2_10_10_10_rev = 0x8368
  let unsigned_int_5_9_9_9_rev = 0x8C3E
  let unsigned_int_atomic_counter = 0x92DB
  let unsigned_int_image_2d = 0x9063
  let unsigned_int_image_2d_array = 0x9069
  let unsigned_int_image_3d = 0x9064
  let unsigned_int_image_buffer = 0x9067
  let unsigned_int_image_cube = 0x9066
  let unsigned_int_image_cube_map_array = 0x906A
  let unsigned_int_sampler_2d = 0x8DD2
  let unsigned_int_sampler_2d_array = 0x8DD7
  let unsigned_int_sampler_2d_multisample = 0x910A
  let unsigned_int_sampler_2d_multisample_array = 0x910D
  let unsigned_int_sampler_3d = 0x8DD3
  let unsigned_int_sampler_buffer = 0x8DD8
  let unsigned_int_sampler_cube = 0x8DD4
  let unsigned_int_sampler_cube_map_array = 0x900F
  let unsigned_int_vec2 = 0x8DC6
  let unsigned_int_vec3 = 0x8DC7
  let unsigned_int_vec4 = 0x8DC8
  let unsigned_normalized = 0x8C17
  let unsigned_short = 0x1403
  let unsigned_short_4_4_4_4 = 0x8033
  let unsigned_short_5_5_5_1 = 0x8034
  let unsigned_short_5_6_5 = 0x8363
  let validate_status = 0x8B83
  let vendor = 0x1F00
  let version = 0x1F02
  let vertex_array = 0x8074
  let vertex_array_binding = 0x85B5
  let vertex_attrib_array_barrier_bit = 0x1
  let vertex_attrib_array_buffer_binding = 0x889F
  let vertex_attrib_array_divisor = 0x88FE
  let vertex_attrib_array_enabled = 0x8622
  let vertex_attrib_array_integer = 0x88FD
  let vertex_attrib_array_normalized = 0x886A
  let vertex_attrib_array_pointer = 0x8645
  let vertex_attrib_array_size = 0x8623
  let vertex_attrib_array_stride = 0x8624
  let vertex_attrib_array_type = 0x8625
  let vertex_attrib_binding_enum = 0x82D4
  let vertex_attrib_relative_offset = 0x82D5
  let vertex_binding_buffer = 0x8F4F
  let vertex_binding_divisor_enum = 0x82D6
  let vertex_binding_offset = 0x82D7
  let vertex_binding_stride = 0x82D8
  let vertex_shader = 0x8B31
  let vertex_shader_bit = 0x1
  let viewport_enum = 0xBA2
  let wait_failed = 0x911D
  let write_only = 0x88B9
  let zero = 0x0
end

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
