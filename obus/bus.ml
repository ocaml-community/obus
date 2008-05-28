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
(*  message_bus : DBus.t Proxy.t;*)
}

let from_connection connection =
(*  let message_bus = Proxy.make connection DBus.interface "org.freedesktop.DBus" "/org/freedesktop/DBus" in*)
    {
      name = begin
        let (_, body) =
          Connection.send_message_sync connection
            (Message.method_call []
               "org.freedesktop.DBus"
               "/org/freedesktop/DBus"
               "org.freedesktop.DBus" "Hello"
               []) in
        let (name, _) =
          (get_values (cons string nil) body) in
          name
      end;
      connection = connection;
(*      message_bus = message_bus;*)
    }

let connect addresses =
  from_connection (Connection.of_addresses addresses ~shared:false)

let session () = connect (Address.session ())
let system () = connect (Address.system ())

let dispatch bus = Connection.dispatch bus.connection

let name { name = x } = x
let connection { connection = x } = x
let make_proxy bus interface name path =
  Proxy.make bus.connection interface (Some name) path
