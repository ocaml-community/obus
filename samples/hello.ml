(*
 * hello.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Just open a connection with the message bus and print the assigned
   unique name *)

open Lwt

let main =
  (perform
     bus <-- Lazy.force OBus_bus.session;
     let _ = Printf.printf "My unique connection name is: %s\n" (match OBus_connection.name bus with
                                                                   | Some x -> x
                                                                   | None -> "") in
     return ())

let _ = Lwt_unix.run main
