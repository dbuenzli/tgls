(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** C APIs.

    A {!t} value represents an C OpenGL API profile or a C OpenGL extension
    (roughly a [feature] or [extension] tag in the OpenGL registry). *)

(** {1:apiid C API identifiers} *)

type version = int * int
(** The type for version numbers. *)

type id = [ `Gl of version | `Gles of version | `Ext of string ]
(** The type for API identifiers. *)

val id_of_string : string -> id
(** [id_of_string s] is an API identifier extracted from [s]. *)

(** {1:apis C APIs} *)

type t
(** The type for a C API. *)

val create : Glreg.t -> id -> string -> [ `Ok of t | `Error of string ]
(** [create registry id profile] is the C API [id] with profile
    [profile] as defined in [registry] ([profile] is ignored if
    unapplicable). *)

val id : t -> id
(** [id api] is the identifier of [api]. *)

val profile : t -> string option
(** [id api] is the profile of [api]. *)

(** {1:types C types} *)

type base_type =
  [ `GLbitfield | `GLboolean | `GLbyte | `GLchar | `GLclampx | `GLdouble
  | `GLenum | `GLfixed | `GLfloat | `GLint | `GLint64 | `GLintptr | `GLshort
  | `GLsizei | `GLsizeiptr | `GLsync | `GLubyte | `GLuint | `GLuint64
  | `GLushort | `GLDEBUGPROC | `Void | `Void_or_index ]
(** The type for C base types as found in OpenGL APIs. *)

val base_type_to_string : base_type -> string
(** [base_type_to_string t] is a string representation for [t]. *)

val base_type_def : t -> base_type -> [ `Def of string | `Builtin ]
(** [base_type_def api t] is the type definition for [t]. Either
    [`Def] if [t] is typedef'd or [`Builtin] if [t] is a C base type. *)

type typ =
  [ `Base of base_type
  | `Ptr of typ
  | `Const of typ
  | `Nullable of typ ]
(** The type for C types as found in OpenGL APIs. *)

val type_to_string : typ -> string
(** [type_to_string t] is a string representation for [t]. *)

val types : t -> typ list
(** [types api] is the set of C types mentioned in the signatures of
    [api]. *)

(** {1:funs C functions} *)

type arg_len = [ `Arg of string | `Size of int | `Csize of string
               | `Other of string ]
(** The type for argument length specifications as found in the registry. *)

type arg =
  { arg_name : string; (** variable name example *)
    arg_type : typ;
    arg_group : string option; (** loosely defined enum group. *)
    arg_len : arg_len option (** loosely defined length of the argument *) }
(** The type for C function arguments. *)

type func = string * (arg list * typ)
(** The type for C functions, a name and an argument list tupled with
    a return type. *)

val funs : t -> func list
(** [funs api] are the C functions of [api]. *)

(** {1:enums C enumerations} *)

type enum_value =
  [ `GLenum of int | `GLenum_max | `GLuint64 of int64 | `GLuint of int32]
(** The type for C enumeration values. *)

type enum = string * enum_value
(** The type for C enumerations, a name and and a value. *)

val enums : t -> enum list
(** [enums api] are the C enums of [api]. *)
