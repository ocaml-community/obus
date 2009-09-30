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

let () = Lwt_main.run (
  lwt bus = Lazy.force OBus_bus.session in
  lwt () = Lwt_io.printlf "My unique connection name is: %s"
    (match OBus_connection.name bus with
       | Some x -> x
       | None -> "") in
  return ()
)
