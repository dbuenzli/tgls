(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** URIs to documentation. *)

(** {1 URIs for APIs and their functions} *)

val home_uri : Capi.t -> string
(** [home_uri api] is a home page URI for the API [api]. *)

val man_uri : Capi.t -> string -> string option
(** [man_uri api f] is an URI to a manual page for the C function [f]
    in the API [api]. *)
