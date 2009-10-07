(*
 * oBus_interface.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

module type Name = sig
  val name : OBus_name.interface
end

module Make(Name : Name) =
struct
  let op_interface = Name.name
  let op_method_call member typ proxy = OBus_proxy.method_call proxy ~interface:op_interface ~member typ
  let op_signal member typ proxy = OBus_signal.connect proxy ~interface:op_interface ~member typ
  let op_property_reader member typ proxy = OBus_property.get proxy ~interface:op_interface ~member typ
  let op_property_writer member typ proxy value = OBus_property.set proxy ~interface:op_interface ~member typ value
end
