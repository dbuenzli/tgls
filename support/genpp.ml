(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp = Format.fprintf
let pp_str = Format.pp_print_string
let pp_opt pp_v ppf v = match v with
| None -> () | Some v -> pp ppf "%a" pp_v v

(* mli API Doc *)

let pp_mli_api_header ppf api =
  let syn = Oapi.doc_synopsis api in
  let lsyn = Oapi.doc_synopsis_long api in
  let profile = Capi.profile api in
  let lib_module = Oapi.module_lib api in
  let bind_module = Oapi.module_bind api in
  pp ppf
"\
(** %s thin bindings.

    [%s] can program %a %s contexts.
    Consult the {{!conventions}binding conventions}.

    Open the module use it, this defines only the module [%s]
    in your scope. To use in the toplevel with [findlib],
    just [#require \"tgls.%s\"], it automatically loads the library and
    opens the [%s] module.

    {b References}
    {ul
    {- {{:%s}%s}}}

    {e %s — %s — %s } *)
@\n"
  syn lib_module (pp_opt pp_str) profile lsyn bind_module
  (String.lowercase_ascii lib_module) lib_module (Doc.home_uri api) syn
  "%%VERSION%%" syn "{{:%%PKG_HOMEPAGE%% }homepage}"

let pp_mli_api_footer ppf api =
  let lib_module = Oapi.module_lib api in
  let bind_module = Oapi.module_bind api in
  pp ppf
"\
(** {1:conventions Conventions}

    To find the name of an OCaml function corresponding to a C
    function name, map the [gl] prefix to the module name
    {!%s.%s},
    add an underscore between each minuscule and majuscule and lower
    case the result. For example [glGetError] maps to
    {!%s.%s.get_error}

    To find the name of an OCaml value corresponding to a C enumerant name,
    map the [GL_] prefix to the module name {!%s.%s}
    and lower case the rest. For example [GL_COLOR_BUFFER_BIT] maps to
    {!%s.%s.color_buffer_bit}.

    The following exceptions occur:
    {ul
    {- A few enumerant names do clash with functions name. In that case we
       postfix the enumerant name with [_enum]. For example we have
       {!%s.%s.viewport} and {!%s.%s.viewport_enum}.}
    {- If applying the above procedures results in an identifier that
       doesn't start with a letter, prefix the identifier with a ['_'].}
    {- If applying the above procedures results in an identifier that
       is an OCaml keyword, suffix the identifier with a ['_'].}} *)"
lib_module bind_module lib_module bind_module lib_module bind_module
lib_module bind_module lib_module bind_module lib_module bind_module

(* License *)

let pp_license_header ppf () =
  let exe = Filename.basename Sys.executable_name in
  let invocation = exe :: (List.tl (Array.to_list Sys.argv)) in
  let invocation = String.concat " " invocation in
  pp ppf
"\
(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated with:
   %s *)
@\n" invocation
