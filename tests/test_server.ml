(*
 * test_server.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_pervasives

let () = Lwt_main.run (
  lwt server =
    OBus_server.make
      (fun server connection ->
         ignore begin
           lwt () = Lwt_io.printl "new connection" in
           ignore (Lwt_sequence.add_r
                     (fun message ->
                        Format.printf "@[<hv 2>message received:@\n%a@]@." OBus_message.print message;
                        Some message)
                     (OBus_connection.incoming_filters connection));
           OBus_connection.set_up connection;
           return ()
         end)
  in
  lwt () = Lwt_io.eprintlf "server addresses: %S" (OBus_address.to_string (OBus_server.addresses server)) in
  lwt connection = OBus_connection.of_addresses (OBus_server.addresses server) in
  lwt () = OBus_connection.emit_signal connection
    ~path:["plop"]
    ~interface:"truc.bidule"
    ~member:"Coucou"
    <:obus_type< string >>
    "Hello world!" in
  Lwt_unix.sleep 2.0
)
