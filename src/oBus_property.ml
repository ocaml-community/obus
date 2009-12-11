(*
 * oBus_property.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_type.Pervasives

let op_method_call member typ proxy = OBus_proxy.method_call proxy ~interface:"org.freedesktop.DBus.Properties" ~member typ

OP_method Get as _dyn_get : string -> string -> variant
OP_method Set as _dyn_set : string -> string -> variant -> unit
OP_method GetAll as _dyn_get_all : string -> (string, variant) dict

OP_method Get as dyn_get_with_context : string -> string -> variant * OBus_connection.context

let dyn_get proxy ~interface ~member = _dyn_get proxy interface member
let dyn_set proxy ~interface ~member value = _dyn_set proxy interface member value
let dyn_get_all proxy ~interface = _dyn_get_all proxy interface

let get proxy ~interface ~member typ =
  lwt value, (connection, message) = dyn_get_with_context proxy interface member in
  return (OBus_type.cast_single typ ~context:(OBus_connection.Context(connection, message)) value)

let set proxy ~interface ~member typ x =
  dyn_set proxy ~interface ~member (OBus_type.make_single typ x)
