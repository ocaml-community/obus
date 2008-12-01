(*
 * oBus_peer.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type

type t = {
  connection : OBus_connection.t;
  name : OBus_name.bus option;
}

let tt = wrap_sequence_ctx tunit
  (fun context () -> match context with
     | OBus_connection.Context(connection, msg) -> { connection = connection; name = OBus_message.sender msg }
     | _ -> raise Cast_failure)
  (fun _ -> ())

let make c n = { connection = c; name = Some n }
let anonymous c = { connection = c; name = None }
let connection p = p.connection
let name p = p.name

let call member typ peer = OBus_connection.method_call peer.connection
  ?destination:peer.name
  ~path:[]
  ~interface:"org.freedesktop.DBus.Peer"
  ~member
  typ

OBUS_method Ping : t
OBUS_method GetMachineId : OBus_uuid.t
