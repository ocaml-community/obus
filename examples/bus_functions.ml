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
open Lwt_react
open Lwt_io

let service = "org.freedesktop.Notifications"
let name = "org.ocamlcore.forge.obus"

module String_set = Set.Make(String)

let () = Lwt_main.run begin
  let%lwt bus = OBus_bus.session () in

  let%lwt id = OBus_bus.get_id bus in
  let%lwt () = printlf "the message bus id is: %S" (OBus_uuid.to_string id) in

  let%lwt names = OBus_bus.list_names bus in
  let%lwt () = printlf "names on the session bus:" in
  let%lwt () = Lwt_list.iter_p (printlf "  %s") names in

  let%lwt names = OBus_bus.list_activatable_names bus in
  let%lwt () = printlf "these names are activatable:" in
  let%lwt () = Lwt_list.iter_p (printlf "  %s") names in

  let%lwt () = printf "trying to start service %S: " service in
  let%lwt result = OBus_bus.start_service_by_name bus service in
  let%lwt () = printl
    (match result with
       | `Success -> "success"
       | `Already_running -> "already running")
  in

  let%lwt () = printf "trying to acquire the name %S: " name in
  let%lwt result = OBus_bus.request_name bus ~replace_existing:true ~do_not_queue:true name in
  let%lwt () = printl
    (match result with
       | `Primary_owner -> "success"
       | `In_queue -> "in queue"
       | `Exists -> "the name already exists"
       | `Already_owner -> "i already own the name")
  in

  printlf "my names are: %s" (String.concat ", " (String_set.elements (S.value (OBus_bus.names bus))))
end
