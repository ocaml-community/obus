(*
 * uDisks_expander.ml
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_pervasives

include OBus_proxy.Private

let op_interface = OBus_proxy.make_interface ~changed:"Changed" "toto.titi"

OP_signal Changed : unit

OP_property_r NativePath : string
OP_property_r Vendor : string
OP_property_r Model : string
OP_property_r Revision : string
OP_property_r NumPorts : uint
OP_property_r UpstreamPorts : t list
OP_property_r Adapter : UDisks_adapter.t
