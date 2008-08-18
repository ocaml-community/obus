(*
 * list-services.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* List services with their owner *)

open Lwt

let list n lbus =
  perform
    let _ = Printf.printf "service name mapping on %s bus:\n" n in
    bus <-- Lazy.force lbus;

    (* Get the list of all names on the session bus *)
    names <-- OBus_bus.list_names bus;

    Lwt_util.iter
      (fun name ->
         OBus_bus.get_name_owner bus name >>= fun owner ->
           Printf.printf "  %s -> %s\n" owner name;
           return ())

      (* Select only names which are not connection unique names *)
      (List.filter (fun s -> s.[0] <> ':') names)

let _ = Lwt_unix.run
  (perform
     list "session" OBus_bus.session;
     list "system" OBus_bus.system)
