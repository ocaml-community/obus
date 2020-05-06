(*
 * nm_dhcp4_config.mli
 * -------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** DHCP4 configuration *)

include OBus_proxy.Private

val options : t -> (string * OBus_value.V.single) list OBus_property.r

val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t
