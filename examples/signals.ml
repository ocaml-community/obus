(*
 * signals.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* This sample illustrate the use of signals *)

open Lwt
open Lwt_io

(* Add an handler on keyboard event which print the multimedia key
   pressed *)
let handle_multimedia_keys device =
  Lwt_event.always_notify_p
    (fun (action, key) ->
       lwt () = printlf "from Hal: action %S on key %S!" action key in
       lwt () = printlf "          the signal come from the device %S" (OBus_path.to_string (Hal_device.udi device)) in
       return ())
    (Hal_device.condition device)#event

lwt () =
  lwt session = Lazy.force OBus_bus.session in

  (* +---------------------------------------------------------------+
     | Signals from message bus                                      |
     +---------------------------------------------------------------+ *)

  Lwt_event.always_notify_p
    (fun (name, old_owner, new_owner) ->
       let opt = function
         | Some s -> s
         | None -> ""
       in
       printlf "from D-Bus: the owner of the name %S changed: %S -> %S"
         name (opt old_owner) (opt new_owner))
    signal#event;

  Lwt_event.always_notify_p
    (printlf "from D-Bus: i lost the name %S!")
    (OBus_bus.name_lost session)#event;

  Lwt_event.always_notify_p
    (printf "from D-Bus: i got the name '%S!")
    (OBus_bus.name_acquired session)#event;

  (* +---------------------------------------------------------------+
     | Some Hal signals                                              |
     +---------------------------------------------------------------+ *)

  lwt manager = Lazy.force Hal_manager.manager in

  Lwt_event.always_notify_p
    (fun device ->
       lwt () = printlf "from Hal: device added: %S" (OBus_path.to_string (Hal_device.udi device)) in

       (* Handle the adding of keyboards *)
       Hal_device.query_capability device "input.keyboard" >>= function
         | true -> handle_multimedia_keys device; return ()
         | false -> return ())
    (Hal_manager.device_added manager)#event;

  (* Find all keyboards and handle events on them *)
  lwt keyboards = Hal_manager.find_device_by_capability manager "input.keyboard" in
  lwt () = printlf "keyboard founds: %d" (List.length keyboards) in
  lwt () = Lwt_list.iter_p (fun dev -> printlf "  %s" (OBus_path.to_string (Hal_device.udi dev))) keyboards in

  List.iter handle_multimedia_keys keyboards;

  lwt () = printf "type Ctrl+C to stop\n%!" in
  fst (wait ())
