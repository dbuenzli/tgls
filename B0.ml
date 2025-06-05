open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let ctypes = B0_ocaml.libname "ctypes"
let ctypes_foreign = B0_ocaml.libname "ctypes-foreign"
let integers = B0_ocaml.libname "integers" (* dep of ctypes *)
let bigarray_compat = B0_ocaml.libname "bigarray-compat" (* dep of ctypes *)
let tsdl = B0_ocaml.libname "tsdl"
let xmlm = B0_ocaml.libname "xmlm"

(* tgls libraries *)

(* Libraries *)

let tgl ~id =
  (* FIXME this is not ready yet we need the right runes for the C
     stubs which are platform dependent, the triangle tests work likely
     because of linking against SDL. The link tests do not work with B0. *)
  let base = Fmt.str "tgl%s" id in
  let tgl_name = B0_ocaml.libname ("tgls." ^ base) in
  let srcs = [ `Dir ~/(Fmt.str "src/%s" base); `File ~/"src/tgl_stub.c" ] in
  let tgl =
    let requires = [ctypes; ctypes_foreign; integers; bigarray_compat] in
    B0_ocaml.lib tgl_name ~srcs ~requires
  in
  tgl_name, tgl

let tgl3, tgl3_lib = tgl ~id:"3"
let tgl4, tgl4_lib = tgl ~id:"4"
let tgles2, tgles2_lib = tgl ~id:"es2"
let tgles3, tgles3_lib = tgl ~id:"es3"

(* API generator *)

let apiquery =
  let srcs = [`Dir ~/"support"; `X ~/"support/dump.ml" ] in
  let doc = "Gl OCaml API generation tool" in
  B0_ocaml.exe "apiquery" ~srcs ~requires:[xmlm] ~doc

let apidump =
  let srcs = [`Dir ~/"support"; `X ~/"support/apiquery.ml" ] in
  B0_ocaml.exe "apidump" ~srcs ~requires:[xmlm]

let glxml = ~/"support/gl.xml"
let glxml_url =
  "https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/api/gl.xml"

let download_glxml =
  let doc = "Download gl.xml to support/gl.xml" in
  B0_unit.of_action "download-glxml" ~doc @@ fun env _ ~args:_ ->
  let glxml = B0_env.in_scope_dir env glxml in
  (Log.stdout @@ fun m ->
   m "@[<v>Downloading %s@,to %a@]" glxml_url Fpath.pp glxml);
  let* () = B0_action_kit.fetch_url env glxml_url glxml in
  Ok ()

let generate_libraries =
  let doc = "Generate tgls.* libraries" in
  let units = [apiquery] in
  B0_unit.of_action "generate-libraries" ~units ~doc @@ fun env _ ~args:_ ->
  let src = B0_env.in_scope_dir env ~/"src" in
  let glxml = B0_env.in_scope_dir env glxml in
  let* () = Os.File.must_exist glxml in
  let* apiquery = B0_env.unit_exe_file_cmd env apiquery in
  let gen_lib glapi lib =
    let mli = Fpath.(src / lib / lib + ".mli") in
    let ml = Fpath.(src / lib / lib + ".ml") in
    let stdout = Os.Cmd.out_file ~force:true ~make_path:false mli in
    let* () = Os.Cmd.run Cmd.(apiquery % "-mli" % "-api" % glapi) ~stdout in
    let stdout = Os.Cmd.out_file ~force:true ~make_path:false ml in
    let* () = Os.Cmd.run Cmd.(apiquery % "-ml" % "-api" % glapi) ~stdout in
    Ok ()
  in
  let* () = gen_lib "gl3.3" "tgl3" in
  let* () = gen_lib "gl4.5" "tgl4" in
  let* () = gen_lib "gles2.0" "tgles2" in
  let* () = gen_lib "gles3.2" "tgles3" in
  Ok ()

(* Tests *)

let test kind ?(requires = []) ~id lib =
  let src = ~/(Fmt.str "test/%sgl%s.ml" kind id) in
  let requires = ctypes :: ctypes_foreign :: lib :: requires in
  B0_ocaml.test src ~requires

let lib_tests ~id lib =
  test "tri" ~id lib ~requires:[tsdl],
  test "link" ~id lib

let linktgl3, tritgl3 = lib_tests ~id:"3" tgl3
let linktgl4, tritgl4 = lib_tests ~id:"4" tgl4
let linktgles2, tritgles2 = lib_tests ~id:"es2" tgles2
let linktgles3, tritgles3 = lib_tests ~id:"es3" tgles3
let dbgllifetime4 =
  B0_ocaml.test ~/"test/dbglifetime4.ml" ~requires:[tgl4; tsdl] ~run:false

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The tgls programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/tgls"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/tgls/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/tgls.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/tgls/issues"
    |> ~~ B0_meta.description_tags
      ["bindings"; "opengl"; "opengl-es"; "graphics"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "ctypes", {|>= "0.21.1"|};
        "ctypes-foreign", {|>= "0.21.1"|};
(*        "tsdl", {|with-test|}; *)
        "xmlm", {|dev|};
      ]
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.make "default" ~doc:"tgls package" ~meta ~locked:true @@
  B0_unit.list ()
