(*
 * uDisks_port.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_pervasives

include OBus_proxy.Private

let op_interface = OBus_proxy.make_interface ~changed:"Changed" "org.freedesktop.UDisks.Port"

OP_signal Changed : unit

OP_property_r ConnectorType : string
OP_property_r Number : int
OP_property_r Parent : OBus_proxy.t
OP_property_r Adapter : OBus_proxy.t
OP_property_r NativePath : string
