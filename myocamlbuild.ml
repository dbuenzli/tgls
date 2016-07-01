open Ocamlbuild_plugin
open Command

(* Platform detection *)

let os =
  String.trim @@
  try Sys.getenv "TGLS_HOST_OS"
  with Not_found -> run_and_read "uname -s"

let darwin = os = "Darwin"
let linux = os = "Linux"
let freebsd = os = "FreeBSD"

let rpi =
  linux &&
  try ignore (run_and_read "cat /proc/cpuinfo | grep -q 'BCM270.'"); true
  with Failure _ -> false

(* pkg-config invocation. N.B. we don't fail if we don't have the package. *)

let pkg_config flags package =
  let has_package =
    try ignore (run_and_read ("pkg-config --exists " ^ package)); true
    with Failure _ -> false
  in
  let cmd tmp =
    Command.execute ~quiet:true &
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A arg) (string_list_of_file tmp)
  in
  if has_package then with_temp_file "pkgconfig" "pkg-config" cmd else []

let use_pkg_config = linux || freebsd

(* Tags for OpenGL X.Y *)

let gl_tag ~tag ~lib ~cpkg  =
  let make_opt o arg = S [ A o; arg ] in
  let stub_l = [A (Printf.sprintf "-l%s" lib)] in
  let cflags = if use_pkg_config then pkg_config "cflags" cpkg else [] in
  let libs_l = if use_pkg_config then pkg_config "libs-only-l" cpkg else [] in
  let libs_L = if use_pkg_config then pkg_config "libs-only-L" cpkg else [] in
  let linker = if linux then [A "-Wl,-no-as-needed"] else [] in
  let mklib_framework = if darwin then [A "-framework"; A "OpenGL" ] else [] in
  let lib_framework = if darwin then [A "-framework OpenGL" ] else [] in
  let mklib_flags =
    (List.map (make_opt "-ldopt") linker) @ libs_l @ libs_L @ mklib_framework
  in
  let compile_flags = List.map (make_opt "-ccopt") cflags in
  let lib_flags = List.map (make_opt "-cclib") (libs_l @ lib_framework) in
  let link_flags = List.map (make_opt "-ccopt") (linker @ libs_L) in
  let stublib_flags = List.map (make_opt "-dllib") stub_l  in
  flag ["c"; "ocamlmklib"; tag] (S mklib_flags);
  flag ["c"; "compile"; tag] (S compile_flags);
  flag ["link"; "ocaml"; tag] (S (link_flags @ lib_flags));
  flag ["link"; "ocaml"; "library"; "byte"; tag] (S stublib_flags)

(* Tags for OpenGL ES X.Y *)

let gles_tag ~tag ~lib ~cpkg  =
  let make_opt o arg = S [ A o; arg ] in
  let stub_l = [A (Printf.sprintf "-l%s" lib)] in
  let cflags =
    if rpi then [] else
    if use_pkg_config then pkg_config "cflags" cpkg else []
  in
  let libs_l =
    if rpi then [A "-lGLESv2"] else
    if use_pkg_config then pkg_config "libs-only-l" cpkg else []
  in
  let libs_L =
    if rpi then [A "-L/opt/vc/lib"] else
    if use_pkg_config then pkg_config "libs-only-L" cpkg else []
  in
  let linker =
    if rpi || linux then [A "-Wl,-no-as-needed"] else []
  in
  let mklib_framework = if darwin then [] else [] in
  let lib_framework = if darwin then [] else [] in
  let mklib_flags =
    (List.map (make_opt "-ldopt") linker) @ libs_l @ libs_L @ mklib_framework
  in
  let compile_flags = List.map (make_opt "-ccopt") cflags in
  let lib_flags = List.map (make_opt "-cclib") (libs_l @ lib_framework) in
  let link_flags = List.map (make_opt "-ccopt") (linker @ libs_L) in
  let stublib_flags = List.map (make_opt "-dllib") stub_l  in
  flag ["c"; "ocamlmklib"; tag] (S mklib_flags);
  flag ["c"; "compile"; tag] (S compile_flags);
  flag ["link"; "ocaml"; tag] (S (link_flags @ lib_flags));
  flag ["link"; "ocaml"; "library"; "byte"; tag] (S stublib_flags)

let () =
  dispatch begin function
  | After_rules ->
      gl_tag ~tag:"use_gl3" ~lib:"tgl3" ~cpkg:"gl";
      gl_tag ~tag:"use_gl4" ~lib:"tgl3" ~cpkg:"gl";
      gles_tag ~tag:"use_gles2" ~lib:"tgles2" ~cpkg:"glesv2";
      gles_tag ~tag:"use_gles3" ~lib:"tgles3" ~cpkg:"glesv3";
  | _ -> ()
  end
