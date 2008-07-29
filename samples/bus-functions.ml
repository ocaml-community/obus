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
open Lwt

let service = "org.freedesktop.Notifications"
let name = "org.ocamlcore.forge.obus"

let ($) a b = a b

let main =
  (perform
     bus <-- OBus_bus.session ();
     id <-- OBus_bus.get_id bus;
     return $ printf "the message bus id is: %S\n" id;

     return $ printf "names on the session bus:\n";
     names <-- OBus_bus.list_names bus;
     return $ List.iter (Printf.printf "  %s\n") names;

     return $ printf "these names are activable:\n";
     names <-- OBus_bus.list_activable_names bus;
     return $ List.iter (Printf.printf "  %s\n") names;

     return $ printf "trying to start service %S: %!" service;
     result <-- OBus_bus.start_service_by_name bus service [];
     return $ print_endline
       (match result with
          | `success -> "success"
          | `already_running -> "already running");

     return $ printf "trying to acquire the name %S: %!" name;
     result <-- OBus_bus.request_name bus name [ `replace_existing; `do_not_queue ];
     return $ print_endline
       (match result with
          | `primary_owner -> "success"
          | `in_queue -> "in queue"
          | `exists -> "the name already exists"
          | `already_owner -> "i already own the name"))

let _ = Lwt_unix.run main

