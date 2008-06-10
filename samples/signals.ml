(*
 * signals.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This sample illustrate signal handling + main loop without
   threads *)

open OBus
open Rules
open Printf

let bus_handler proxy = function
  | DBus.Name_owner_changed(name, old_owner, new_owner) ->
      printf "from DBus: the owner of the name %S changed: %S -> %S\n%!" name old_owner new_owner
  | DBus.Name_lost(name) ->
      printf "from DBus: i lost the name %S!\n%!" name
  | DBus.Name_acquired(name) ->
      printf "from DBus: i got the name '%S!\n%!" name

let hal_handler proxy = function
  | Hal.Device.Condition("ButtonPressed", button) ->
      printf "from Hal: you pressed the button %S!\n%!" button;
      printf "          the signal come from the object %S\n%!" (Proxy.path proxy)
  | _ ->
      ()

let get_fd bus = Transport.fd (Connection.transport bus)

let _ =
  let session = Bus.session () in
  let system = Bus.system () in
    (* Bus register will add the signals handler + ask the bus to
       route us signals from this interface *)
    Signal.bus_register session DBus.signals bus_handler;

    (* We can also add manually the match rules on the message
       bus. That way we can put finer rules *)
    DBus.add_match system
      [ Type Signal;
        Sender "org.freedesktop.Hal";
        Interface (Interface.name Hal.Device.interface);
        Member "Condition";
        Arg(0, "ButtonPressed") ];

    (* Now we just add the handler *)
    Signal.register system Hal.Device.signals hal_handler;

    printf "You can now do other things with the bus or press one of your multimedia key to see something.\n\n%!";

    (* We retreive the file descriptor of the connections for our main
       loop *)
    let fd_map =
      [ get_fd session, session;
        get_fd system, system ] in

      while true do
        (* Now we just do a [Unix.select] on all file descriptors and
           do one dispatch operation on the concerned connection *)
        let (fds, _, _) = Unix.select (List.map fst fd_map) [] [] (-1.0) in
          List.iter (fun fd -> Connection.dispatch (List.assoc fd fd_map)) fds
      done
