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

OBUS_method GetAllDevices : unit -> Hal_device.udi list
OBUS_method GetAllDevicesWithProperties : unit -> [Hal_device.udi * {string, Hal_device.property} list] list
OBUS_method DeviceExists : object_path -> bool
OBUS_method FindDeviceStringMatch : string -> string -> Hal_device.udi list

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

OBUS_method NewDevice : unit -> string
OBUS_method Remove : string -> unit
OBUS_method CommitToGdl : string -> string -> unit
OBUS_method AcquireGlobalInterfaceLock : string -> bool -> unit
OBUS_method ReleaseGlobalInterfaceLock : string -> unit
OBUS_method SingletonAddonIsReady : string -> unit

OBUS_signal DeviceAdded : broken_udi
OBUS_signal DeviceRemoved : broken_udi
OBUS_signal NewCapability : broken_udi * string
OBUS_signal GlobalInterfaceLockAcquired : string * string * int
OBUS_signal GlobalInterfaceLockReleased : string * string * int
