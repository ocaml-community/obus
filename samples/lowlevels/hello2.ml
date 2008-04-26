(*
 * hello2.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This is same example as hello.ml, but using the Bus module *)

open OBus

let _ =
  let bus = Bus.session () in
    (* We do not have to send a Hello, the Bus.session already do that
       for us :) *)
    Printf.printf "My unique connection name is: %s\n" (Bus.name bus)

