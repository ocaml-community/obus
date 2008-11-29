(*
 * bus_functions.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This sample illustrate use if some of the functions offered by the
   message bus *)

open Printf
open Lwt

let service = "org.freedesktop.Notifications"
let name = "org.ocamlcore.forge.obus"

let main =
  (perform
     bus <-- Lazy.force OBus_bus.session;

     id <-- OBus_bus.get_id bus;
     let _ = printf "the message bus id is: %S\n" (OBus_uuid.to_string id) in

     names <-- OBus_bus.list_names bus;
     let _ =
       printf "names on the session bus:\n";
       List.iter (Printf.printf "  %s\n") names
     in

     names <-- OBus_bus.list_activatable_names bus;
     let _ =
       printf "these names are activatable:\n";
       List.iter (Printf.printf "  %s\n") names
     in

     let _ = printf "trying to start service %S: %!" service in
     result <-- OBus_bus.start_service_by_name bus service;
     let _ = print_endline
       (match result with
          | `success -> "success"
          | `already_running -> "already running")
     in

     let _ = printf "trying to acquire the name %S: %!" name in
     result <-- OBus_bus.request_name bus ~flags:[ `replace_existing; `do_not_queue ] name;
     let _ = print_endline
       (match result with
          | `primary_owner -> "success"
          | `in_queue -> "in queue"
          | `exists -> "the name already exists"
          | `already_owner -> "i already own the name")
     in
     return ())

let _ = Lwt_unix.run main
