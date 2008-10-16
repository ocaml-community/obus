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
let handle_multimedia_keys udi =
  Hal_device.on_condition udi
    (fun (action, key) ->
       printf "from Hal: action %S on key %S!\n" action key;
       printf "          the signal come from the device %S\n%!" (OBus_path.to_string udi))
  >>= fun _ -> return ()

let main : unit Lwt.t =
  perform
    session <-- Lazy.force OBus_bus.session;

    (*** Signals from Message bus ***)

    OBus_bus.on_name_owner_changed session
      (fun (name, old_owner, new_owner) ->
         printf "from DBus: the owner of the name %S changed: %S -> %S\n%!" name old_owner new_owner);

    OBus_bus.on_name_lost session
      (printf "from DBus: i lost the name %S!\n%!");

    OBus_bus.on_name_acquired session
      (printf "from DBus: i got the name '%S!\n%!");

    (*** Some hal signals ***)

    Hal_manager.on_device_added
      (fun udi ->
         printf "from Hal: device added: %S\n%!" (OBus_path.to_string udi);

         (* Handle the adding of keyboards *)
         ignore_result
           (Hal_device.query_capability udi "input.keyboard"
            >>= function
              | true -> handle_multimedia_keys udi
              | false -> return ()));

    (* Find all keyboards handle events on them *)
    keyboards <-- Hal_manager.find_device_by_capability "input.keyboard";
    let _ =
      printf "keyboard founds: %d\n" (List.length keyboards);
      List.iter (fun p -> printf "  %s\n" (OBus_path.to_string p)) keyboards
    in
    Lwt_util.iter handle_multimedia_keys keyboards;

    let _ = printf "type Ctrl+C to stop\n%!" in
    wait ()

let _ = Lwt_unix.run main
