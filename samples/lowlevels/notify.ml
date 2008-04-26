(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Now something more fun: sending notification *)

open OBus
open Message
open Values

let notify connection title msg =
  let (_, body) =
    Connection.send_message_sync connection
      (method_call []
         "org.freedesktop.Notifications"
         "/org/freedesktop/Notifications"
         "org.freedesktop.Notifications"
         "Notify"
         (make_values
            (<+ string uint32 string string string
               (array string)
               (dict string variant)
               int32 +>)
            (<- (Filename.basename Sys.argv.(0)) (* app_name *)
               0l (* id *)
               "info" (* icon *)
               title (* summary *)
               msg (* body *)
               [] (* actions *)
               [] (* hints *)
               5000l (* timeout *) ->)))
  in
  let <- return_id -> = get_values <+ uint32 +> body in
    return_id

let _ =
  let bus = Bus.session () in
  let connection = Bus.connection bus in
    ignore (notify connection "Hello, world!" "ocaml is fun!")
