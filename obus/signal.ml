(*
 * signal.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Values

type ('a, 'b) handler = 'a Proxy.t option -> 'b -> bool
type ('a, 'b) set = 'a Interface.t * (Connection.t -> ('a, 'b) handler -> bool Connection.reader)

let register connection (_, make_reader) handler =
  Connection.raw_add_filter connection (make_reader connection handler)

let bus_register bus (interface, make_reader) handler =
  let connection = Bus.connection bus in
    Connection.raw_add_filter connection (make_reader connection handler);
    ignore
      (Connection.send_message_sync connection
         (Message.method_call []
            "org.freedesktop.DBus"
            "/org/freedesktop/DBus"
            "org.freedesktop.DBus" "AddMatch"
            [make_value string
               (Printf.sprintf "type='signal',interface='%s'" (Interface.name interface))]))

let make_set interface make_reader = (interface, make_reader)
