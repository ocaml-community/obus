(*
 * hal_manager.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_value
open OBus_type.Pervasives

type t = OBus_proxy.t with obus

let manager =
  lazy(lwt bus = Lazy.force OBus_bus.system in
       return (OBus_proxy.make
                 (OBus_peer.make bus "org.freedesktop.Hal")
                 [ "org"; "freedesktop"; "Hal"; "Manager" ]))

include OBus_interface.Make(struct let name = "org.freedesktop.Hal.Manager" end)

(* Hal seems to returns string instead of object path... *)
let obus_broken_device = OBus_type.map_with_context obus_string
  (fun context path -> match context with
     | OBus_connection.Context(connection, message) ->
         { OBus_proxy.peer = { OBus_peer.connection = connection;
                               OBus_peer.name = OBus_message.sender message };
           OBus_proxy.path = OBus_path.of_string path }
     | _ ->
         raise OBus_type.Cast_failure)
  (fun proxy -> OBus_path.to_string (OBus_proxy.path proxy))

OP_method GetAllDevices : broken_device list
OP_method GetAllDevicesWithProperties : (broken_device * (string, Hal_device.property) dict) structure list
OP_method DeviceExists : broken_device -> bool
OP_method FindDeviceStringMatch : string -> string -> broken_device list
OP_method FindDeviceByCapability : string -> broken_device list
OP_method NewDevice : string
OP_method Remove : string -> unit
OP_method CommitToGdl : string -> string -> unit
OP_method AcquireGlobalInterfaceLock : string -> bool -> unit
OP_method ReleaseGlobalInterfaceLock : string -> unit
OP_method SingletonAddonIsReady : string -> unit

OP_signal DeviceAdded : broken_device
OP_signal DeviceRemoved : broken_device
OP_signal NewCapability : broken_device * string
OP_signal GlobalInterfaceLockAcquired : string * string * int
OP_signal GlobalInterfaceLockReleased : string * string * int
