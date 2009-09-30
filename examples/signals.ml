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
open Lwt_io

let events = ref []

(* Add an handler on keyboard event which print the multimedia key
   pressed *)
let handle_multimedia_keys device =
  lwt signal = Hal_device.condition device in
  events := React.E.map
    (fun (action, key) ->
       ignore
         (lwt () = printlf "from Hal: action %S on key %S!" action key in
          lwt () = printlf "          the signal come from the device %S" (OBus_path.to_string (device :> OBus_path.t)) in
          return ())) signal#event :: !events;
  return ()

let () = Lwt_main.run (
  lwt session = Lazy.force OBus_bus.session in

  (*** Signals from Message bus ***)

  lwt e1 = OBus_bus.name_owner_changed session >|= fun s -> React.E.map
    (fun (name, old_owner, new_owner) ->
       let opt = function
         | Some s -> s
         | None -> ""
       in
       ignore (printlf "from DBus: the owner of the name %S changed: %S -> %S"
                 name (opt old_owner) (opt new_owner))) s#event in

  lwt e2 = OBus_bus.name_lost session >|= fun s -> React.E.map
    (fun name -> ignore (printlf "from DBus: i lost the name %S!" name))
    s#event in

  lwt e3 = OBus_bus.name_acquired session >|= fun s -> React.E.map
    (fun name -> ignore (printf "from DBus: i got the name '%S!" name))
    s#event in

  (*** Some hal signals ***)

  lwt e4 = Hal_manager.device_added () >|= fun s -> React.E.map
    (fun device ->
       ignore begin
         lwt () = printlf "from Hal: device added: %S" (OBus_path.to_string device) in

         (* Handle the adding of keyboards *)
         Hal_device.query_capability device "input.keyboard" >>= function
           | true -> handle_multimedia_keys device
           | false -> return ()
       end) s#event in

  (* Find all keyboards and handle events on them *)
  lwt keyboards = Hal_manager.find_device_by_capability "input.keyboard" in
  lwt () = printlf "keyboard founds: %d" (List.length keyboards) in
  lwt () = Lwt_util.iter (fun udi -> printlf "  %s" (OBus_path.to_string udi)) keyboards in

  lwt () = Lwt_util.iter handle_multimedia_keys keyboards in

  lwt () = printf "type Ctrl+C to stop\n%!" in
  fst (wait ())
)
