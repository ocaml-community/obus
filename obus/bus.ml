(*
 * bus.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Values

type name = string

type t = {
  name : name;
  connection : Connection.t;
}

let from_connection connection =
  {
    name = begin
      let (_, body) =
        Connection.send_message_sync connection
          (Message.method_call []
             "org.freedesktop.DBus"
             "/org/freedesktop/DBus"
             "org.freedesktop.DBus" "Hello"
             []) in
        match body with
          | [String name] -> name
          | _ -> raise Wire.Reading.Unexpected_signature
    end;
    connection = connection;
  }

let connect addresses =
  from_connection (Connection.of_addresses addresses ~shared:false)

let session () = connect (Address.session ())
let system () = connect (Address.system ())

let dispatch bus = Connection.dispatch bus.connection

let name { name = x } = x
let connection { connection = x } = x
let make_proxy bus interface destination path =
  Proxy.make (connection bus) interface ~sender:(name bus) ~destination:destination path
