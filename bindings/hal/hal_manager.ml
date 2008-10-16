(*
 * hal_manager.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type
open OBus_value
open Lwt

include OBus_client.Make_constant
  (struct
     let name = "org.freedesktop.Hal.Manager"
     let path = ["org"; "freedesktop"; "Hal"; "Manager"]
     let service = Some "org.freedesktop.Hal"
     let bus = OBus_bus.system
   end)

let get_all_devices = call "GetAllDevices" << unit -> Hal_device.udi list >>
let get_all_devices_with_properties = call "GetAllDevicesWithProperties" << unit -> [Hal_device.udi * {string, Hal_device.property} list] list >>
let device_exists = call "DeviceExists" << object_path -> bool >>
let find_device_string_match = call "FindDeviceStringMatch" << string -> string -> Hal_device.udi list >>

let tbroken_udi = wrap_basic tstring OBus_path.of_string OBus_path.to_string

(* Signature from introsection seems to be wrong for this method. So
   we temporary use this ugly hack: *)
let find_device_by_capability capability =
  dcall "FindDeviceByCapability" [vbasic (String capability)] >>= fun v ->
    match opt_cast_sequence <:obus_type< path list >> v with
      | Some x -> return x
      | None ->
          match opt_cast_sequence <:obus_type< broken_udi list >> v with
            | Some x -> return x
            | None ->
                fail
                  (Failure (Printf.sprintf
                              "unexpected signature for reply of method \"FindDeviceByCapability\"\
                               on interface \"org.freedesktop.Hal.Manager\", expected: \"ao\", got: %S"
                              (string_of_signature (type_of_sequence v))))

let new_device = call "NewDevice" << unit -> string >>
let remove = call "Remove" << string -> unit >>
let commit_to_gdl = call "CommitToGdl" << string -> string -> unit >>
let acquire_global_interface_lock = call "AcquireGlobalInterfaceLock" << string -> bool -> unit >>
let release_global_interface_lock = call "ReleaseGlobalInterfaceLock" << string -> unit >>
let singleton_addon_is_ready = call "SingletonAddonIsReady" << string -> unit >>

let on_device_added = on_signal "DeviceAdded" <:obus_type< broken_udi >>
let on_device_removed = on_signal "DeviceRemoved" <:obus_type< broken_udi >>
let on_new_capability = on_signal "NewCapability" <:obus_type< broken_udi * string >>
let on_global_interface_lock_acquired = on_signal "GlobalInterfaceLockAcquired" <:obus_type< string * string * int >>
let on_global_interface_lock_released = on_signal "GlobalInterfaceLockReleased" <:obus_type< string * string * int >>
