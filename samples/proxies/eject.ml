(*
 * eject.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus

let _ =
  let bus = Bus.system () in
  let manager = Bus.make_proxy bus Hal.Manager.interface "org.freedesktop.Hal" "/org/freedesktop/Hal/Manager" in
  let cdroms = Hal.Manager.find_device_by_capability manager "storage.cdrom" in
    Printf.printf "cdrom(s) found: %d\n" (List.length cdroms);
    List.iter begin function cdrom_path ->
      let cdrom = Bus.make_proxy bus Hal.Device.Volume.interface "org.freedesktop.Hal" cdrom_path in
        Printf.printf "eject on device %s\n" cdrom_path;
        ignore (Hal.Device.Volume.eject cdrom [])
    end cdroms
