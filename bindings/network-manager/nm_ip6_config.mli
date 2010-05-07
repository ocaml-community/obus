(*
 * nm_ip6_config.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

include OBus_proxy.Private

val addresses : t -> (string * int) list OBus_property.r
val nameservers : t -> string list OBus_property.r
val domains : t -> string list OBus_property.r
val routes : t -> (string * int * string * int) list OBus_property.r

type properties = {
  addresses : (string * int) list;
  nameservers : string list;
  domains : string list;
  routes : (string * int * string * int) list;
}

val properties : t -> properties OBus_property.r
