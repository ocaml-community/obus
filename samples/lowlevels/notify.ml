(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This sample illustrate use of the lowlevel interface of OBus *)

open OBus
open Values

let notify connection title msg =
  (* We have to construct a message by hand *)
  let message =
    Message.method_call
      ~destination:"org.freedesktop.Notifications"
      ~path:"/org/freedesktop/Notifications"
      ~interface:"org.freedesktop.Notifications"
      ~member:"Notify"
      ~body:[string (Filename.basename Sys.argv.(0)); (* app_name *)
             uint32 0l; (* id *)
             string "info"; (* icon *)
             string title; (* summary *)
             string msg; (* body *)
             array `string []; (* actions *)
             dict `string `variant []; (* hints *)
             int32 5000l] (* timeout *)
      () in

  (* Now we send the message and block until the reply come *)
  let reply =
    Connection.send_message_sync connection message in

    (* Of course we must match the body to see if this is what we
       expect... *)
    match Message.body reply with
      | [Uint32 return_id] -> return_id
      | _ ->
          Printf.eprintf "unexpected signature: %s\n%!" (Message.signature reply);
          exit 1

let _ =
  let bus = Bus.session () in
    ignore (notify bus "Hello, world!" "ocaml is fun!")
