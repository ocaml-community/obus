(*
 * nm_vpn_connection.mli
 * ---------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** VPN connections *)

include OBus_proxy.Private

val vpn_state_changed : t -> (int * int) OBus_signal.t
val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t

val vpn_state : t -> int OBus_property.r
val banner : t -> string OBus_property.r

type properties = {
  vpn_state : int;
  banner : string;
}

val properties : t -> properties OBus_property.r
