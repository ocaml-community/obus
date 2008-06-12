(*
 * bus.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open ThreadImplem
open Values

type t = Connection.t

let name connection =
  Connection.intern_get_name connection
    (fun () ->
       match Connection.send_message_sync connection
         (Message.method_call
            ~destination:"org.freedesktop.DBus"
            ~path:"/org/freedesktop/DBus"
            ~interface:"org.freedesktop.DBus"
            ~member:"Hello"
            ~body:[] ())
       with
         | { Message.body = [String name] } -> name
         | { Message.signature = signature } ->
             failwith
               (Printf.sprintf
                  "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                  "Hello" "org.freedesktop.DBus" "s" (signature_of_dtypes signature)))

let register_connection connection = ignore (name connection)

let connect addresses =
  let bus = Connection.of_addresses addresses ~shared:true in
    register_connection bus;
    bus

let session () = connect (Address.session ())
let system () = connect (Address.system ())

let make_proxy bus interface destination path =
  Proxy.make bus interface ~destination:destination path
