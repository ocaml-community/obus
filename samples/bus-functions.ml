(*
 * bus-functions.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus

let _ =
  let bus = Bus.session () in
    Printf.printf "bus id: %s\n" (DBus.get_id bus);
    let names = DBus.list_names bus in
      print_endline "names on the session bus:";
      List.iter print_endline names;
      let s = "org.freedesktop.Notifications" in
        Printf.printf "starting service %s: " s;
        match DBus.start_service_by_name bus s [] with
          | `Success -> print_endline "success"
          | `Already_running -> print_endline "already running"
