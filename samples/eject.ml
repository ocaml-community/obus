(*
 * eject.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Simple sample which eject all cdroms using Hal *)

open Lwt
open Printf
open Hal

let main =
  (perform
     manager <-- Lazy.force manager;
     cdroms <-- Manager.find_device_by_capability manager "storage.cdrom";
     let _ = printf "cdrom(s) found: %d\n" (List.length cdroms) in
     Lwt_util.iter begin function cdrom ->
       Printf.printf "eject on device %s\n" (OBus_path.to_string (Device.udi cdrom));
       Device.Storage.eject cdrom [] >>= fun _ -> return ();
     end cdroms)

let _ = Lwt_unix.run main
