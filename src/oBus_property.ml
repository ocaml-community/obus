(*
 * oBus_property.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type.Perv

let method_call member typ proxy = OBus_proxy.method_call proxy ~interface:"org.freedesktop.DBus.Properties" ~member typ

OBUS_method Get as _dyn_get : string -> string -> variant
OBUS_method Set as _dyn_set : string -> string -> variant -> unit
OBUS_method GetAll as _dyn_get_all : string -> (string, variant) dict

OBUS_method Get as dyn_get_with_context : string -> string -> variant * OBus_connection.context

let dyn_get proxy ~interface ~member = _dyn_get proxy interface member
let dyn_set proxy ~interface ~member value = _dyn_set proxy interface member value
let dyn_get_all proxy ~interface = _dyn_get_all proxy interface

let get proxy ~interface ~member typ =
  lwt value, (connection, message) = dyn_get_with_context proxy interface member in
  return (OBus_type.cast_single typ ~context:(OBus_connection.Context(connection, message)) value)

let set proxy ~interface ~member typ x =
  dyn_set proxy ~interface ~member (OBus_type.make_single typ x)
