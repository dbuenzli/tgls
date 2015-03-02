let gen_bindings version bindings =
  let prefix =
    match version with
    | `Gl3 -> "tgl3"
    | `Gl4 -> "tgl4"
    | `Gles2 -> "tgles2"
    | `Gles3 -> "tgles3"
  and definitions, headers =
    match version with
    | `Gl3 | `Gl4 -> ["GL_GLEXT_PROTOTYPES"], ["GL/gl.h"; "GL/glext.h"]
    | `Gles2 -> [], ["GLES2/gl2.h"]
    | `Gles3 -> [], ["GLES3/gl3.h"]
  in

  let chan = open_out (Printf.sprintf "src/%s_stubs.c" prefix) in
  let fmt = Format.formatter_of_out_channel chan in
  List.iter (Format.fprintf fmt "#define %s@.") definitions;
  List.iter (Format.fprintf fmt "#include <%s>@.") headers;
  Cstubs.write_c fmt ~prefix:"caml_" bindings;
  close_out chan;

  let chan = open_out (Printf.sprintf "src/%s_generated.ml" prefix) in
  let fmt = Format.formatter_of_out_channel chan in
  Cstubs.write_ml fmt ~prefix:"caml_" bindings;
  close_out chan

(* https://github.com/ocamllabs/ocaml-ctypes/issues/279 *)
module Wrap(S: functor(F: Cstubs.FOREIGN with type 'a fn = 'a) -> sig end)
           (F: Cstubs.FOREIGN with type 'a fn = unit) =
struct
  include S(struct
    type 'a fn = 'a
    let foreign name typ = F.foreign name typ; fun _ -> assert false
  end)
end

let () =
  gen_bindings `Gl3 (module Wrap(Tgl3_bindings.Gl));
  gen_bindings `Gl4 (module Wrap(Tgl4_bindings.Gl));
  gen_bindings `Gles2 (module Wrap(Tgles2_bindings.Gl));
  gen_bindings `Gles3 (module Wrap(Tgles3_bindings.Gl))
