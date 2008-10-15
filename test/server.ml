(*
 * server.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type

let _ =
  Lwt_unix.run
    (perform
       server <-- OBus_server.make (fun connection ->
                                      print_endline "new connection";
                                      ignore (OBus_connection.add_filter connection
                                                (Format.printf "@[<hv 2>message received:@\n%a@]@." OBus_message.print)));
       let addresses = OBus_server.addresses server in
       let _ = Printf.eprintf "server addresses: %S\n%!" (OBus_address.to_string addresses) in
       connection <-- OBus_connection.of_addresses addresses;
       OBus_connection.emit_signal connection
         ~path:["plop"]
         ~interface:"truc.bidule"
         ~member:"Coucou"
         (<< string -> unit >>) "Hello world!";
       Lwt_unix.sleep 2.0)
