(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** OpenGL registry decoder.

    [Glreg] decodes the data of the
    {{:http://www.opengl.org/registry}OpenGL registry} from its
    XML representation.

    Release %%VERSION%% – %%MAINTAINER%% *)

(** {1 Registry representation} *)

type typ =
  { t_name : string;
    t_api : string option;
    t_requires : string option;
    t_def : string; }
(** The type representing the type tag. The [def] string is obtained
    from the contents of the tag by concatenating all {e data} sections
    and removing any ["typedef"] and [";"] suffix. *)

type group =
  { g_name : string;
    g_enums : string list; }
(** The type representing the [group] tag. *)

type enum =
  { e_name : string;
    e_p_namespace : string;
    e_p_type : string option;
    e_p_group : string option;
    e_p_vendor : string option;
    e_value : string;
    e_api : string option;
    e_type : string option;
    e_alias : string option; }
(** The type representing an enum tag. The [e_p]* fields come from
    the parent's [enums] tag. *)

type param_type =
  { p_group : string option;
    p_type : string;
    p_len : string option;
    p_nullable : bool; (** This doesn't exist in the registry.
                           See {!Fixreg}. *)}
(** The type for representing return types and parameter type of commands. *)

type command =
  { c_name : string;
    c_p_namespace : string;
    c_ret : param_type; (** group * return type *)
    c_params : (string * param_type) list;
    c_alias : string option;
    c_vec_equiv : string option; }
(** The type representing a command tag. The [c_p]* fields come
    from the parent [commands]'s tag. *)

type i_element =
  { i_name : string;
    i_type : [ `Enum | `Command | `Type ];
    i_api : string option;
    i_profile : string option; }
(** The type for interface elements as described in require and remove
    tags. [i_api] comes from the nearest ancestor ([feature], [require] or
    [remove] tag). *)

type feature =
  { f_api : string;
    f_number : int * int;
    f_require : i_element list;
    f_remove : i_element list; }
(** The type for representing a [feature] tag. *)

type extension =
  { x_name : string;
    x_supported : string option;
    x_require : i_element list;
    x_remove : i_element list; }
(** The type for repesenting an [extension] tag. *)

type t =
  { types : (string, typ list) Hashtbl.t;
    groups : (string, group) Hashtbl.t;
    enums : (string, enum list) Hashtbl.t;
    commands : (string, command list) Hashtbl.t;
    features : (string, feature list) Hashtbl.t;
    extensions : (string, extension) Hashtbl.t; }
(** The type for an OpenGL registry.
  {ul
  {- [types] the contents of types tag represented as a map from
     type names to their definition(s).}
  {- [groups] the contents of groups tag represented as a map from
     group names to their definition.}
  {- [enums] the contents of enums tag represented as a map from
     {e enum} name to their definition(s).}
  {- [commands] the contents of commands tag represented as a map from
     {e command} name to their definition(s).}
  {- [feature] the contents of feature tags represented as a map from
     api name to their definition.}
  {- [extensions] the contents of extension tags represented as a map
     from extension name to their definition.}} *)

(** {1:decoder Decoder} *)

type src = [ `Channel of in_channel | `String of string ]
(** The type for input sources. *)

type decoder
(** The type for the OpenGL XML registry decoder *)

val decoder : [< src ] -> decoder
(** [decoder src] is a decoder that inputs from [src]. *)

val decode : decoder -> [ `Error of string | `Ok of t ]
(** [decode d] decodes an OpenGL XML registry from [d] or returns an
    error. *)

val decoded_range : decoder -> (int * int) * (int * int)
(** [decoded_range d] is the range of characters spanning the [`Error]
    decoded by [d]. A pair of line and column numbers respectively
    one and zero based. *)

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
