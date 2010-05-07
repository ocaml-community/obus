(*
 * nm_ppp.mli
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** PPP *)

include OBus_proxy.Private

val need_secrets : t -> (string * string) Lwt.t
val set_ip4_config : t -> config : (string * OBus_value.V.single) list -> unit Lwt.t
val set_state : t -> state : int -> unit Lwt.t
