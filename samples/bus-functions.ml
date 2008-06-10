(*
 * bus-functions.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This sample illustrate use if some of the functions offered by the
   message bus *)

open Printf
open OBus

let service = "org.freedesktop.Notifications"
let name = "org.ocamlcore.forge.obus"

let _ =
  let bus = Bus.session () in
    printf "the message bus id is: %S\n" (DBus.get_id bus);

    printf "names on the session bus:\n";
    List.iter (printf "  %s\n") (DBus.list_names bus);

    printf "these names are activable:\n";
    List.iter (printf "  %s\n") (DBus.list_activatable_names bus);

    printf "trying to start service %S: %!" service;
    print_endline
      (match DBus.start_service_by_name bus service [] with
         | `Success -> "success"
         | `Already_running -> "already running");

    printf "trying to acquire the name %S: %!" name;
    print_endline
      (match DBus.request_name bus name [ `Replace_existing; `Do_not_queue ] with
         | `Primary_owner -> "success"
         | `In_queue -> "in queue"
         | `Exists -> "the name already exists"
         | `Already_owner -> "i already own the name")

