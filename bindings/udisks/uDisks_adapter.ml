(*
 * uDisks_adapter.ml
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_pervasives

include OBus_proxy.Private

let op_interface = OBus_proxy.make_interface ~notify:(OBus_property.notify_global "Changed") "org.freedesktop.UDisks.Adapter"

OP_signal Changed : unit

OP_property_r Fabric : string
OP_property_r NumPorts : uint
OP_property_r Driver : string
OP_property_r Model : string
OP_property_r Vendor : string
OP_property_r NativePath : string
