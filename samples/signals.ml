(*
 * signals.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This sample illustrate the use of signals *)

open Lwt
open Printf

(* Add an handler on keyboard event which print the multimedia key
   pressed *)
let handle_multimedia_keys device =
  let e0 = OBus_signal.connect (Hal_device.condition device)
    (fun (action, key) ->
       printf "from Hal: action %S on key %S!\n" action key;
       printf "          the signal come from the device %S\n%!" (OBus_path.to_string (device :> OBus_path.t));
       return ())
  >>= fun _ -> return ()

let main : unit Lwt.t =
  lwt session = Lazy.force OBus_bus.session in

  (*** Signals from Message bus ***)

  lwt e1 = OBus_bus.name_owner_changed session >|= fun s -> React.E.map s#signal
    (fun (name, old_owner, new_owner) ->
       let opt = function
         | Some s -> s
         | None -> ""
       in
       printf "from DBus: the owner of the name %S changed: \"%s\" -> \"%s\"\n%!"
         name (opt old_owner) (opt new_owner);
       return ()) in

  let e2 = React.E.map (OBus_bus.name_lost session)#signal
    (fun name ->
       printf "from DBus: i lost the name %S!\n%!" name;
       return ()) in

  let e3 = React.E.map (OBus_bus.name_acquired session)#signal
    (fun name ->
       printf "from DBus: i got the name '%S!\n%!" name;
       return ()) in

  (*** Some hal signals ***)

  let e4 = React.E.map Hal_manager.device_added#signal
    (fun device ->
       printf "from Hal: device added: %S\n%!" (OBus_path.to_string (device :> OBus_path.t));

       (* Handle the adding of keyboards *)
       Hal_device.query_capability device "input.keyboard"
       >>= function
         | true -> handle_multimedia_keys device
         | false -> return ()) in

  (* Find all keyboards and handle events on them *)
  lwt keyboards = Hal_manager.find_device_by_capability "input.keyboard" in
  printf "keyboard founds: %d\n" (List.length keyboards);
  List.iter (fun udi -> printf "  %s\n" (OBus_path.to_string (Hal_device.path udi))) keyboards;

  lwt () = Lwt_util.iter handle_multimedia_keys keyboards in

  printf "type Ctrl+C to stop\n%!";
  wait ()

let _ = Lwt_unix.run main
