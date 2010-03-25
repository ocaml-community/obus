(*
 * bus_functions.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* This sample illustrate use if some of the functions offered by the
   message bus *)

open Lwt
open Lwt_io

let service = "org.freedesktop.Notifications"
let name = "org.ocamlcore.forge.obus"

module Name_set = Set.Make(String)

lwt () =
  lwt bus = OBus_bus.session () in

  lwt id = OBus_bus.get_id bus in
  lwt () = printlf "the message bus id is: %S" (OBus_uuid.to_string id) in

  lwt names = OBus_bus.list_names bus in
  lwt () = printlf "names on the session bus:" in
  lwt () = Lwt_list.iter_p (printlf "  %s") names in

  lwt names = OBus_bus.list_activatable_names bus in
  lwt () = printlf "these names are activatable:" in
  lwt () = Lwt_list.iter_p (printlf "  %s") names in

  lwt () = printf "trying to start service %S: " service in
  lwt result = OBus_bus.start_service_by_name bus service in
  lwt () = printl
    (match result with
       | `Success -> "success"
       | `Already_running -> "already running")
  in

  lwt () = printf "trying to acquire the name %S: " name in
  lwt result = OBus_bus.request_name bus ~replace_existing:true ~do_not_queue:true name in
  lwt () = printl
    (match result with
       | `Primary_owner -> "success"
       | `In_queue -> "in queue"
       | `Exists -> "the name already exists"
       | `Already_owner -> "i already own the name")
  in

  printlf "my names are: %s" (String.concat ", " (Name_set.elements (React.S.value (OBus_bus.acquired_names bus))))
