(*
 * async.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This samples illustrate the use of asynchronous calls *)

open Printf
open OBus

let _ =
  let bus = Bus.session () in
    DBus.get_id_async bus
      (fun name -> eprintf "bus id: %S\n%!" name);

    let cookie = DBus.list_activatable_names_cookie bus in

      (* This will always fail *)
      DBus.get_name_owner_async bus "foo.bar"
        ~on_error:(fun exn -> eprintf "error received: %s\n%!" (Printexc.to_string exn))
        (fun owner -> ());

      let names = Cookie.get cookie in
        eprintf "%d names are activables\n%!" (List.length names);

        (* Wait a bit for the reply *)
        Thread.delay 1.0
