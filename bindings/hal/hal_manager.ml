(*
 * hal_manager.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_value

let get_manager =
  lazy(perform
         bus <-- Lazy.force OBus_bus.system;
         return (OBus_proxy.make (OBus_peer.make bus "org.freedesktop.Hal")
                   [ "org"; "freedesktop"; "Hal"; "Manager" ]))

include OBus_interface.Make_single
    (struct
       let proxy = get_manager
     end)
    (struct
       let name = "org.freedesktop.Hal.Manager"
     end)

OBUS_method GetAllDevices : unit -> Hal_device.udi list
OBUS_method GetAllDevicesWithProperties : unit -> [Hal_device.udi * {string, Hal_device.property} list] list
OBUS_method DeviceExists : Hal_device.udi -> bool
OBUS_method FindDeviceStringMatch : string -> string -> Hal_device.udi list

let tbroken_udi = OBus_type.wrap_basic tstring
  (fun x -> Hal_device.make (OBus_path.of_string x))
  (fun x -> OBus_path.to_string (x :> OBus_path.t))

(* Signature from introsection seems to be wrong for this method. So
   we temporary use this ugly hack: *)
let find_device_by_capability capability =
  Lazy.force get_manager >>= fun proxy ->
    OBus_proxy.dcall proxy ~interface ~member:"FindDeviceByCapability" [vbasic (String capability)] >>= fun v ->
      match OBus_type.opt_cast_sequence <:obus_type< Hal_device.udi list >> v with
        | Some x -> return x
        | None ->
            match OBus_type.opt_cast_sequence <:obus_type< broken_udi list >> v with
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
