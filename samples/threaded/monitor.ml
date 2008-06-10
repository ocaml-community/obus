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
open Header
open Rules

let filter what_bus header body =
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
  signature = %S
  body = %s

%!" what_bus header.flags.no_reply_expected header.flags.no_auto_start header.serial
      (match header.message_type with
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
      (opt header.destination)
      (opt header.sender)
      header.signature
      (Values.string_of_values body)

(* Note that we can not filter all method calls... The bus will
   disconnection us if we try to do it. So we have to add a match for
   each names *)

let add_name bus name =
  if name <> Bus.name bus && name.[0] = ':' then
    DBus.add_match bus [ Type Method_call; Sender name ]

(* Handle new names *)
let handle_new_names bus proxy = function
  | DBus.Name_owner_changed(name, "", _) -> add_name bus name
  | _ -> ()

let match_all bus =
  List.iter (fun typ -> DBus.add_match bus [ Type typ ])
    [ Method_return; Error; Signal ];
  (* Initially we list names and add a match for each name *)
  DBus.list_names_async bus (List.iter (add_name bus));
  Signal.register bus DBus.signals (handle_new_names bus)

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
