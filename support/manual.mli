(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Manual bindings. *)

(** {1 Manual bindings} *)

type binding = string * string
(** The type for bindings. An [mli] signature and an [ml] implementation. *)

val get : Capi.t -> string -> binding option
(** [get api f] is a manual binding, if any, for the C function [f] in
    the API [api]. *)
