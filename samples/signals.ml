(*
 * signals.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus
open Printf

let bus_handler proxy = function
  | DBus.Name_owner_changed(name, old, _new) ->
      printf "the owner of the name '%s' changed: '%s' -> '%s'\n%!" name old _new
  | DBus.Name_lost(name) ->
      printf "i lost the name '%s'!\n%!" name
  | DBus.Name_acquired(name) ->
      printf "Youhou! i got the name '%s'!\n%!" name

let hal_handler proxy = function
  | Hal.Device.Condition("ButtonPressed", button) ->
      printf "You pressed '%s'!\n%!" button;
  | _ ->
      ()

let get_fd bus = Transport.fd (Connection.transport bus)

let _ =
  let session = Bus.session () in
  let system = Bus.system () in
    Signal.bus_register session DBus.signals bus_handler;
    Signal.bus_register system Hal.Device.signals hal_handler;
    let fd_map =
      [ get_fd session, session;
        get_fd system, system ] in
      while true do
        let (fds, _, _) = Unix.select (List.map fst fd_map) [] [] (-1.0) in
          List.iter
            (fun fd -> Connection.dispatch (List.assoc fd fd_map))
            fds
      done
