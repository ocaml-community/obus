(*
 * hello.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Just open a connection with the message bus and print the assigned
   unique name *)

let () = Lwt_main.run begin
  let%lwt bus = OBus_bus.session () in
  Lwt_io.printlf "My unique connection name is: %s" (OBus_connection.name bus)
end
