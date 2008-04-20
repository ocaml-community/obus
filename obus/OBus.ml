(*
 * OBus.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module LowLevel = LowLevel_internal
module Types = Types_internal
module Values = Values_internal

type bus = LowLevel.bus

exception Connection_failed

let session () =
  LowLevel.open_bus (Address.session ())

let system () =
  LowLevel.open_bus (Address.system ())

let connect_to_bus str =
  LowLevel.open_bus (Address.of_string str)
