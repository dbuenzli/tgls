#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let glreg_file = "support/gl.xml"
let glreg_uri =
  "https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/api/gl.xml"

let get_glreg () =
  OS.File.exists glreg_file >>= function
  | true -> Ok ()
  | false ->
      let curl = Cmd.(v "curl" % "-f" % "-#" % "-S" % glreg_uri) in
      Log.app (fun m -> m "Downloading %s" glreg_uri);
      OS.Cmd.(run_out curl |> to_file glreg_file)

let gen_apis () =
  let ocb = Conf.tool "ocamlbuild" `Build_os in
  let ocb = Cmd.(ocb % "-classic-display" % "-no-links" % "-use-ocamlfind") in
  let apiquery = Cmd.(v "_build/support/apiquery.native") in
  let gen_api api m =
    let mli = OS.Cmd.to_file (strf "src/%s.mli" m) in
    let ml = OS.Cmd.to_file (strf "src/%s.ml" m) in
    OS.Cmd.run_out Cmd.(apiquery % "-mli" % "-api" % api) |> mli >>= fun () ->
    OS.Cmd.run_out Cmd.(apiquery % "-ml" % "-api" % api) |> ml
  in
  let gen_apis () =
    gen_api "gl3.3" "tgl3" >>= fun () ->
    gen_api "gl4.5" "tgl4" >>= fun () ->
    gen_api "gles2.0" "tgles2" >>= fun () ->
    gen_api "gles3.2" "tgles3"
  in
  (* FIXME this won't work on bytecode only pins *)
  OS.Cmd.run Cmd.(ocb % "apiquery.native")
  >>= fun () -> gen_apis ()

let main () =
  begin
    Topkg.Private.disable_main (); (* We only want the nicer OS API. *)
    get_glreg ()
    >>= fun () -> gen_apis ()
    >>= fun () -> Ok 0
  end
  |> Log.on_error_msg ~use:(fun () -> 1)

let () = exit (main ())
