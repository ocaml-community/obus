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

open Printf
open Lwt
open OBus_bus
open OBus_header

let filter what_bus header body =
  let opt = function
    | Some s -> sprintf "Some %S" s
    | None -> "None"
  in
  printf "message intercepted on %s bus:
  no_reply_expected = %B
  no_auto_start = %B
  serial = %ld
  message_type = %s
  destination = %s
  sender = %s
  signature = %S
  body_type = %s
  body = %s

%!" what_bus header.flags.no_reply_expected header.flags.no_auto_start header.serial
    (match header.typ with
       | `Method_call(path, interface, member) ->
           sprintf "method_call
  path = %S
  interface = %s
  member = %S" path (opt interface) member
       | `Method_return reply_serial ->
           sprintf "method_return
  reply_serial = %ld" reply_serial
       | `Error(reply_serial, error_name) ->
           sprintf "error
  reply_serial = %ld
  error_name = %S" reply_serial error_name
       | `Signal(path, interface, member) ->
           sprintf "signal
  path = %S
  interface = %S
  member = %S" path interface member)
    (opt header.destination)
    (opt header.sender)
    (OBus_types.string_of_signature (OBus_value.type_of_sequence body))
    (OBus_types.string_of_sequence  (OBus_value.type_of_sequence body))
    (OBus_value.string_of_sequence body)

let add_filter what_bus get_bus =
  (perform
     bus <-- get_bus ();
     let _ =
       ignore (OBus_connection.add_filter bus (filter what_bus));

       (* Filtering method calls seems to make the bus to disconnect us *)
       List.iter (fun typ -> ignore_result (OBus_bus.add_match bus (OBus_bus.match_rule ~typ ())))
         [ `method_return; `error; `signal ]
     in
     return ())

let _ =
  ignore_result (add_filter "session" OBus_bus.session);
  ignore_result (add_filter "system" OBus_bus.system);

  printf "type Ctrl+C to stop\n%!";
  Lwt_unix.run (wait ())
