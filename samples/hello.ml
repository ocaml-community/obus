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
     bus <-- OBus_bus.session ();
     return (Printf.printf "My unique connection name is: %s\n" (OBus_bus.name bus)))

let _ =
  Lwt_unix.run main
