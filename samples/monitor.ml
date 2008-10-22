(*
 * monitor.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This sample illustrate the use of threads in DBus + use of
   filters. Filters are part of the lowlevel api. *)

open Lwt
open OBus_bus
open OBus_message
open OBus_value

let filter what_bus message =
  Format.printf "@[<hv 2>message intercepted on %s bus:@\n%a@]@."
    what_bus OBus_message.print message;
  (* Drop the message so we do not respond to method call *)
  None

let add_filter what_bus lbus =
  (perform
     bus <-- Lazy.force lbus;
     let _ =
       ignore (OBus_connection.add_incoming_filter bus (filter what_bus));

       List.iter (fun typ -> ignore_result (OBus_bus.add_match bus (OBus_bus.match_rule ~typ ())))
         [ `method_call; `method_return; `error; `signal ]
     in
     return ())

let _ =
  ignore_result (add_filter "session" OBus_bus.session);
  ignore_result (add_filter "system" OBus_bus.system);

  Printf.printf "type Ctrl+C to stop\n%!";
  Lwt_unix.run (wait ())
