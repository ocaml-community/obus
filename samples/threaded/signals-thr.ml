(*
 * signals-thr.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus
open Printf

let bus_handler proxy = function
  | DBus.Name_owner_changed(name, old, _new) ->
      printf "the owner of the name '%s' changed: '%s' -> '%s'\n%!" name old _new;
      true
  | DBus.Name_lost(name) ->
      printf "i lost the name '%s'!\n%!" name;
      true
  | DBus.Name_acquired(name) ->
      printf "Youhou! i got the name '%s'!\n%!" name;
      true

let hal_handler proxy = function
  | Hal.Device.Condition("ButtonPressed", button) ->
      printf "You pressed '%s'!\n%!" button;
      true
  | _ ->
      false

let get_fd bus = Transport.fd (Connection.transport (Bus.connection bus))

let _ =
  let session = Bus.session () in
  let system = Bus.system () in
    Signal.bus_register session DBus.signals bus_handler;
    Signal.bus_register system Hal.Device.signals hal_handler;
    for i = 1 to 10 do
      Thread.delay 3.0;
      ignore (DBus.request_name session "org.truc.bidule" []);
      Thread.delay 3.0;
      ignore (DBus.release_name session "org.truc.bidule")
    done;
    Thread.delay 300.0
