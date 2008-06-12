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
open OBus
open Rules
open Message

let filter what_bus message =
  let opt = function
    | Some s -> sprintf "Some %S" s
    | None -> "None"
  in
    printf "message intercepted on %s bus:
  flags = { no_reply_expected = %B ; no_auto_start = %B }
  serial = %ld
  message_type = %s
  destination = %s
  sender = %s
  signature = %s
  body = %s

%!" what_bus message.flags.no_reply_expected message.flags.no_auto_start message.serial
      (match message.typ with
         | `Method_call(path, interface, member) ->
             sprintf "method_call { path = %S ; interface = %s ; member = %S }"
               path (opt interface) member
         | `Method_return reply_serial ->
             sprintf "method_return { reply_serial = %ld }" reply_serial
         | `Error(reply_serial, error_name) ->
             sprintf "error { reply_serial = %ld ; error_name = %S }" reply_serial error_name
         | `Signal(path, interface, member) ->
             sprintf "signal { path = %S ; interface = %S ; member = %S }"
               path interface member)
      (opt message.destination)
      (opt message.sender)
      (Values.string_of_dtypes (signature message))
      (Values.string_of_values (body message))

let match_all bus =
  (* Filtering method calls seems to make the bus to disconnect us *)
  List.iter (fun typ -> DBus.add_match bus [ Type typ ])
    [ Method_return; Error; Signal ]

let _ =
  let session = Bus.session () in
  let system = Bus.system () in

    ignore (Connection.add_filter session (filter "session"));
    ignore (Connection.add_filter system (filter "system"));

    (* filter everything *)
    match_all session;
    match_all system;

    (* Nothing more to do a thread already handles messages on each
       connections! *)
    Thread.delay 0.1;
    printf "type Ctrl+C to stop\n%!";
    Thread.delay 10000.0
