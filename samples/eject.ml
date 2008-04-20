(*
 * eject.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus
open DBus
open Hal

let _ =
  let bus = OBus.system () in
  let dbus = DBus.proxy bus "org.freedesktop.DBus" "/org/freedesktop/DBus" in
    Printf.printf "connection name : %s\n" (DBus.hello dbus ());
    let manager = Hal.Manager.proxy bus "org.freedesktop.Hal" "/org/freedesktop/Hal/Manager" in
    let cdroms = Hal.Manager.find_device_by_capability manager "storage.cdrom" in
      Printf.printf "cdrom(s) found: %d\n" (List.length cdroms);
      List.iter begin function cdrom_path ->
        let cdrom = Hal.Device.Volume.proxy bus "org.freedesktop.Hal" cdrom_path in
          Printf.printf "eject on device %s\n" cdrom_path;
          ignore (Hal.Device.Volume.eject cdrom [])
      end cdroms
