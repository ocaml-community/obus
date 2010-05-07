(*
 * nm_vpn_plugin.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** VPN plugin interface *)

include OBus_proxy.Private

val connect : t -> connection : (string * (string * OBus_value.V.single) list) list -> unit Lwt.t
val need_secrets : t -> settings : (string * (string * OBus_value.V.single) list) list -> string Lwt.t
val disconnect : t -> unit Lwt.t
val set_ip4_config : t -> config : (string * OBus_value.V.single) list -> unit Lwt.t
val set_failure : t -> reason : string -> unit Lwt.t

val state_changed : t -> int OBus_signal.t
val ip4_config : t -> (string * OBus_value.V.single) list OBus_signal.t
val login_banner : t -> string OBus_signal.t
val failure : t -> int OBus_signal.t

val state : t -> int OBus_property.r
