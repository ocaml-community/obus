(*
 * pong.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Very simple service with one object have a ping method *)

open Lwt
open OBus_type
open OBus_value
open OBus_introspect


let obj = object
  method path = [ "plip" ]

  method handle_call connection msg =
    let `Method_call(path, interface, member) = OBus_message.typ msg in
    match interface, member, OBus_message.body msg with
      | Some "org.plop.foo", "ping", [Basic(String m)] ->
          Lwt.ignore_result (OBus_connection.dsend_reply connection msg [vbasic(String("pong in reply to: " ^ m))]);
          true
      | _ -> false
end

let _ = Lwt_unix.run
  (perform
     bus <-- Lazy.force OBus_bus.session;

     (* Request a name *)
     OBus_bus.request_name bus "org.plop" [];

     (* Expose the object on the connection *)
     let _ = OBus_object.expose bus obj in

     (* Wait forever *)
     wait ())
