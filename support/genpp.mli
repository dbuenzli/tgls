(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Binding generation helpers. *)

(** {1 Documentation generalities for APIs} *)

val pp_mli_api_header : Format.formatter -> Capi.t -> unit
(** [pp_mli_api_header ppf mli] prints an [.mli] file header for [api]
    on [ppf]. *)

val pp_mli_api_footer : Format.formatter -> Capi.t -> unit
(** [pp_mli_api_footer ppf mli] prints an [.mli] file footer for [api]
    on [ppf]. *)

(** {1 License} *)

val pp_license_header : Format.formatter -> unit -> unit
(** [pp_license_header ppf ()] prints a license header on [ppf]. *)
