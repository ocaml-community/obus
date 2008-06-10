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

open OBus

let _ =
  let bus = Bus.session () in
    Printf.printf "My unique connection name is: %s\n" (Bus.name bus)
