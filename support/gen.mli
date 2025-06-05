(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Binding generation *)

(** {1 API binding generation} *)

val pp_api_mli : log:Format.formatter ->  Format.formatter -> Capi.t -> unit
(** [pp_api_mli log ppf api] pretty prints a binding signature for [api] on
    [ppf]. Warnings and errors are logged on [log]. *)

val pp_api_ml : log:Format.formatter -> Format.formatter -> Capi.t -> unit
(** [pp_api_ml log ppf api] pretty prints a binding implementation for [api]
    on [ppf]. Warnings and errors are logged on [log]. *)
