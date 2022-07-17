open Tsdl
open Tgl4
let check = function
  | Ok r -> r
  | Error (`Msg e) -> failwith e
let set attr v = check (Sdl.gl_set_attribute attr v)

let string_of_source e =
  if e = Gl.debug_source_api then "API"
  else if e = Gl.debug_source_window_system then "Window System"
  else if e = Gl.debug_source_shader_compiler then "Shader Compiler"
  else if e = Gl.debug_source_third_party then "Third Party"
  else if e = Gl.debug_source_application then "Application"
  else if e = Gl.debug_source_other then "Other"
  else "??"

let string_of_type e =
  if e = Gl.debug_type_error then "Error"
  else if e = Gl.debug_type_deprecated_behavior then "Deprecated Functionality"
  else if e = Gl.debug_type_portability then "Portability"
  else if e = Gl.debug_type_performance then "Performance"
  else if e = Gl.debug_type_other then "Other"
  else "??"

let string_of_severity e =
  if e = Gl.debug_severity_high then Sdl.log_error Sdl.Log.category_application
  else if e = Gl.debug_severity_medium then Sdl.log_warn Sdl.Log.category_application
  else if e = Gl.debug_severity_low then Sdl.log_verbose Sdl.Log.category_application
  else Sdl.log

let debug_func src typ _id severity msg =
  let log = string_of_severity severity in
  log "%s from %s: %s" (string_of_type typ) (string_of_source src) msg

let debug_func2 _ _ _ severity msg =
  let log = string_of_severity severity in
  log "%s" msg

let () =
  check (Sdl.init Sdl.Init.video);
  set Sdl.Gl.context_major_version 4;
  set Sdl.Gl.context_minor_version 0;
  set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;
  set Sdl.Gl.context_flags Sdl.Gl.context_debug_flag;
  Sdl.log_set_all_priority Sdl.Log.priority_verbose;
  let attrs = Sdl.Window.(opengl + resizable) in
  let win = check(Sdl.create_window ~w:100 ~h:100 "test" attrs) in
  let ctx = check (Sdl.gl_create_context win) in
  Gl.enable Gl.debug_output_synchronous;
  Gl.debug_message_callback debug_func;
  Gl.debug_message_control Gl.dont_care Gl.dont_care Gl.dont_care 0 None
    true;
  Gc.full_major ();
  let msg = "test" in
  Gl.debug_message_insert
    Gl.debug_source_application Gl.debug_type_other 42 Gl.debug_severity_low
    (String.length msg) msg;
  Gl.debug_message_callback debug_func2;
  let msg = "test" in
  Gl.debug_message_insert
    Gl.debug_source_application Gl.debug_type_other 42 Gl.debug_severity_low
    (String.length msg) msg;
  Gc.full_major ();
  Sdl.gl_delete_context ctx;
  Sdl.destroy_window win;
  Sdl.quit ();
