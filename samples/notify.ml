(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

(* This sample illustrate a method call using the functions from
   [OBus_connection] *)

let notify connection title msg =
  (perform
     (header, id) <-- OBus_connection.send_message_with_reply connection

       (* The header is here constructed by hand *)
       (OBus_header.method_call
          ~destination:"org.freedesktop.Notifications"
          ~path:"/org/freedesktop/Notifications"
          ~interface:"org.freedesktop.Notifications"
          ~member:"Notify" ())

       (* This is a type which is expanded by the [pa_obus] syntax
          extension into a expression *)
       [: string -> uint -> string -> string -> string -> string list -> (string, variant) assoc -> int -> uint ]

       (* Here are the argument of the method call *)
       (Filename.basename Sys.argv.(0)) (* app_name *)
       0 (* id *)
       "info" (* icon *)
       title (* summary *)
       msg (* body *)
       [] (* actions *)
       [] (* hints *)
       5000; (* timeout *)
     return id)

let main =
  (perform
     bus <-- OBus_bus.session ();
     notify bus "Hello, world!" "ocaml is fun!";
     return ())

let _ = Lwt_unix.run main
