(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
