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
  OBus_signal.connect device Hal.Device.condition
    (fun (action, key) ->
       printf "from Hal: action %S on key %S!\n" action key;
       printf "          the signal come from the device %S\n%!" (OBus_path.to_string (Hal.Device.udi device)))
  >>= fun _ -> return ()

let main : unit Lwt.t =
  perform
    session <-- Lazy.force OBus_bus.session;

    (*** Signals from Message bus ***)

    OBus_signal.connect session OBus_bus.name_owner_changed
      (fun (name, old_owner, new_owner) ->
         let opt = function
           | Some s -> s
           | None -> ""
         in
         printf "from DBus: the owner of the name %S changed: \"%s\" -> \"%s\"\n%!"
           name (opt old_owner) (opt new_owner));

    OBus_signal.connect session OBus_bus.name_lost
      (printf "from DBus: i lost the name %S!\n%!");

    OBus_signal.connect session OBus_bus.name_acquired
      (printf "from DBus: i got the name '%S!\n%!");

    (*** Some hal signals ***)

    hal <-- Lazy.force Hal.manager;

    OBus_signal.connect hal Hal.Manager.device_added
      (fun device ->
         printf "from Hal: device added: %S\n%!" (OBus_path.to_string (OBus_proxy.path device));

         (* Handle the adding of keyboards *)
         ignore_result
           (Hal.Device.query_capability device "input.keyboard"
            >>= function
              | true -> handle_multimedia_keys device
              | false -> return ()));

    (* Find all keyboards and handle events on them *)
    keyboards <-- Hal.Manager.find_device_by_capability hal "input.keyboard";
    let _ =
      printf "keyboard founds: %d\n" (List.length keyboards);
      List.iter (fun p -> printf "  %s\n" (OBus_path.to_string (OBus_proxy.path p))) keyboards
    in
    Lwt_util.iter handle_multimedia_keys keyboards;

    let _ = printf "type Ctrl+C to stop\n%!" in
    wait ()

let _ = Lwt_unix.run main
