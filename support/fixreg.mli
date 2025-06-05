(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Missing registry data.

    This module provides information that is missing in the OpenGL XML
    registry. *)

(** {1:null NULL arguments or return values} *)

val is_arg_nullable : string -> string -> bool
(** [is_arg_nullable f arg] is [true] if the argument named [arg] of the C
    function [f] can be [NULL]. *)

val is_ret_nullable : string -> bool
(** [is_ret_nullable f] is [true] if the function named [f] may return
    [NULL]. *)

(** {1:null Void pointer or index arguments} *)

val is_arg_voidp_or_index : string -> string -> bool
(** [is_voidp_or_index f arg] is [true] if the argument named [arg] of the
    C function [f] can be either a pointer or an integer index. *)
