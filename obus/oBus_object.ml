(*
 * oBus_object.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_internals

class t = object(self)
  method path = ["ocaml_object_id"; string_of_int (Oo.id self)]
  method handle_call (connection : connection) (mc : OBus_message.method_call) = false
end

let expose connection obj =
  with_running connection & fun running ->
    running.exported_objects <- Object_map.add (obj#path) (obj :> t) running.exported_objects

let remove connection obj =
  let path = obj#path in
  with_running connection & fun running -> match Object_map.lookup path running.exported_objects with
    | Some obj' when (obj :> t) = obj' ->
        running.exported_objects <- Object_map.remove path running.exported_objects
    | _ -> ()

let remove_by_path connection path =
  with_running connection & fun running ->
    running.exported_objects <- Object_map.remove path running.exported_objects
