(*
 * server.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_type.Pervasives

let () = Lwt_main.run (
  lwt server = OBus_server.make () in
  Lwt_event.always_notify_p
    (fun connection ->
       lwt () = Lwt_io.printl "new connection" in
       ignore (Lwt_sequence.add_r
                 (fun message ->
                    Format.printf "@[<hv 2>message received:@\n%a@]@." OBus_message.print message;
                    Some message)
                 (OBus_connection.incoming_filters connection));
       OBus_connection.set_up connection;
       return ())
    server#event;
  lwt () = Lwt_io.eprintlf "server addresses: %S" (OBus_address.to_string server#addresses) in
  lwt connection = OBus_connection.of_addresses server#addresses in
  lwt () = OBus_connection.emit_signal connection
    ~path:["plop"]
    ~interface:"truc.bidule"
    ~member:"Coucou"
    <:obus_type< string >>
    "Hello world!" in
  Lwt_unix.sleep 2.0
)
