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

let main =
  (perform
     cdroms <-- Hal_manager.find_device_by_capability "storage.cdrom";
     let _ = printf "cdrom(s) found: %d\n" (List.length cdroms) in
     Lwt_util.iter begin function cdrom ->
       Printf.printf "eject on device %s\n" (OBus_path.to_string (Hal_device.path cdrom));
       Hal_device.Storage.eject cdrom [] >>= fun _ -> return ();
     end cdroms)

let _ = Lwt_unix.run main
