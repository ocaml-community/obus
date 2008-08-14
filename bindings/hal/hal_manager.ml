(*
 * hal_manager.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type
open Lwt

include OBus_client.Make_constant
  (struct
     let name = "org.freedesktop.Hal.Manager"
     let path = "/org/freedesktop/Hal/Manager"
     let service = Some "org.freedesktop.Hal"
     let bus = OBus_bus.system
   end)

let get_all_devices = call "GetAllDevices" << unit -> Hal_device.udi list >>
let get_all_devices_with_properties = call "GetAllDevicesWithProperties" << unit -> (Hal_device.udi * (string, Hal_device.property) assoc) structure list >>
let device_exists = call "DeviceExists" << object_path -> bool >>
let find_device_string_match = call "FindDeviceStringMatch" << string -> string -> Hal_device.udi list >>
let find_device_by_capability = call "FindDeviceByCapability" << string -> Hal_device.udi list >>
let new_device = call "NewDevice" << unit -> string >>
let remove = call "Remove" << string -> unit >>
let commit_to_gdl = call "CommitToGdl" << string -> string -> unit >>
let acquire_global_interface_lock = call "AcquireGlobalInterfaceLock" << string -> bool -> unit >>
let release_global_interface_lock = call "ReleaseGlobalInterfaceLock" << string -> unit >>
let singleton_addon_is_ready = call "SingletonAddonIsReady" << string -> unit >>
