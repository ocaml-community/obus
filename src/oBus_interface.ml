(*
 * oBus_interface.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
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

module type Custom = sig
  type proxy
  val get : proxy -> OBus_proxy.t
end

module MakeCustom(Custom : Custom)(Name : Name) =
struct
  let op_interface = Name.name
  let op_method_call member typ proxy = OBus_proxy.method_call (Custom.get proxy) ~interface:op_interface ~member typ
  let op_signal member typ proxy = OBus_signal.connect (Custom.get proxy) ~interface:op_interface ~member typ
  let op_property_reader member typ proxy = OBus_property.get (Custom.get proxy) ~interface:op_interface ~member typ
  let op_property_writer member typ proxy value = OBus_property.set (Custom.get proxy) ~interface:op_interface ~member typ value
end
