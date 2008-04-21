(*
 * OBus.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module LowLevel = LowLevel_internal

module Bus =
struct
  type t = LowLevel.bus
  exception Connection_failed = LowLevel.Connection_failed
  let session () = LowLevel.open_bus (Address.session ())
  let system () = LowLevel.open_bus (Address.system ())
  let connect str = LowLevel.open_bus (Address.of_string str)
end

module Types = Types_internal
module Values = Values_internal

module Signal =
struct
  type 'a t = int
  type id = int
  let register bus signal f = failwith "not implemented"
  let unregister bus id = failwith "not implemented"
  let clear bus signal = failwith "not implemented"
end
