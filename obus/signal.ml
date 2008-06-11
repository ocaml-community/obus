(*
 * signal.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Values
open Message
open Connection

type ('a, 'b) handler = 'a Proxy.t -> 'b -> unit
type ('a, 'b) set = 'a Interface.t * (Connection.t -> ('a, 'b) handler -> (signal_type intern_recv -> (unit -> unit) option))

let register connection (interface, make_reader) handler =
  intern_add_signal_handler connection (Interface.name interface)
    (make_reader connection handler)

let bus_register bus (interface, make_reader) handler =
  intern_add_signal_handler bus (Interface.name interface)
    (make_reader bus handler);
  ignore
    (send_message_sync bus
       (method_call
          ~destination:"org.freedesktop.DBus"
          ~path:"/org/freedesktop/DBus"
          ~interface:"org.freedesktop.DBus"
          ~member:"AddMatch"
          ~body:[string
                   (Printf.sprintf "type='signal',interface='%s'" (Interface.name interface))]
          ()))

let make_set interface get_reader =
  (interface,
   fun connection handler message ->
     let `Signal(path, _, member) = message.typ in
       match get_reader (member, message.signature) with
         | Some(reader) ->
             let (byte_order, buffer, ptr) = message.body in
             let signal = reader byte_order buffer ptr
             and proxy = Proxy.make connection interface ?destination:message.sender path in
               Some(fun () -> handler proxy signal)
         | None -> None)
