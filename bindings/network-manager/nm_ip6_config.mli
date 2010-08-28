(*
 * nm_ip6_config.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Ip6 configuration *)

include OBus_proxy.Private

val addresses : t -> (string * int) list OBus_property.r
val nameservers : t -> string list OBus_property.r
val domains : t -> string list OBus_property.r
val routes : t -> (string * int * string * int) list OBus_property.r

val properties : t -> OBus_property.group
