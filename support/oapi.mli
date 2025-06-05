(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** OCaml APIs.

    An {!t} value represents an OCaml OpenGL API for a C API. *)

(** {1:api APIs} *)

type t = Capi.t

val doc_synopsis : t -> string
(** [doc_synopsis t] is a name to represent the major version of [api]
    and all its preceding versions. *)

val doc_synopsis_long : t -> string
(** [doc_synopis_long t] is like {!doc_synopsis} but longer. *)

val identifier : string -> string
(** [identifier s] is either [s] or [s] transformed so that it is
    an OCaml identifier. *)

(** {1:modules Modules} *)

val module_lib : t -> string
(** [module_lib api] is the toplevel OCaml module name for [api]. *)

val module_bind : t -> string
(** [module_bind api] is the OCaml module name containing the
    binding for [api]. *)

(** {1:types Types} *)

type ctypes =
  [ `Builtin of string
  | `View of string * string * string * string
  | `Builtin_wrap_in of string * (Format.formatter -> string -> unit)
  | `Def of string * string ]
(** The type for a ctypes definition for a type.
    {ul
    {- [`Builtin ctype] is the builtin ctypes C type [ctype].}
    {- [`View (id, read, write, ctype)] is a ctypes view named
       [id] for [ctype]; reads with [read], writes with [write].}
    {- [`Builtin_wrap_in (ctype, pp_wrap)] is the builtin type [ctype],
       [pp_wrap] is a function that given a value name transforms
       the value to a value of the type represented by [ctype].}
    {- [`Def (id,def)] is a ctype [id] whose definition is [def].}} *)

type typ =
  { type_name : string;
    type_def : [ `Alias of string | `Abstract of string | `Builtin ];
    type_ctypes : ctypes;
    type_doc : string option; }
(** The type for OCaml types and their definitions. *)

val type_def : t -> Capi.typ -> [ `Ok of typ | `Unknown of string ]
(** [type_def api t] is the OCaml type definition for the C type [t]
    in [api] *)

val types : t -> typ list
(** [types api] are the OCaml types mentioned in [api]. *)

(** {1:funs Functions} *)

type arg =
  { arg_name : string;
    arg_type : typ }
(** The type for OCaml arguments. A sample argument name and its
    type. *)

type fun_def =
  [ `Derived of arg list * typ
  | `Manual of string * string
  | `Unknown
  | `Unbound of string ]
(** The type for OCaml function definitions.
    {ul
    {- [`Derived]. The binding is automatically derived from the C prototope
       of the function. For a few function names the derivation is tweaked
       manually}
    {- [`Manual (mli, ml)]. The binding is made manually, [mli] is
       the function's signature and [ml] its implementation.}
    {- [`Unknown]. The function is unknown.}
    {- [`Unbound reason]. The function is unbound for [reason].}} *)

type func =
  { fun_name : string; (** Function name *)
    fun_c : Capi.func; (** C definition *)
    fun_def : fun_def; (** Binding definition *)
    fun_doc : string option; (** Additional documentation. *) }
(** The type for an OCaml function specification. *)

val funs : t -> func list
(** [funs api] are the OCaml functions of [api]. *)

(** {1:enums Enums} *)

type enum =
  { enum_name : string;
    enum_c_name : string;
    enum_value : Capi.enum_value }
(** The type for OCaml's representation of C enums. *)

val enums : t -> enum list
(** [enums api] is the enums of [api]. *)
