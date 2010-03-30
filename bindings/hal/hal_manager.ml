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
open OBus_pervasives

type t = OBus_proxy.t with obus

let manager () =
  lwt bus = OBus_bus.system () in
  return (OBus_proxy.make
            (OBus_peer.make bus "org.freedesktop.Hal")
            [ "org"; "freedesktop"; "Hal"; "Manager" ])

let op_interface = OBus_proxy.make_interface "org.freedesktop.Hal.Manager"

OP_method GetAllDevices : OBus_proxy.broken list
OP_method GetAllDevicesWithProperties : (OBus_proxy.broken * (string, Hal_device.property) dict) structure list
OP_method DeviceExists : OBus_proxy.broken -> bool
OP_method FindDeviceStringMatch : string -> string -> OBus_proxy.broken list
OP_method FindDeviceByCapability : string -> OBus_proxy.broken list
OP_method NewDevice : string
OP_method Remove : string -> unit
OP_method CommitToGdl : string -> string -> unit
OP_method AcquireGlobalInterfaceLock : string -> bool -> unit
OP_method ReleaseGlobalInterfaceLock : string -> unit
OP_method SingletonAddonIsReady : string -> unit

OP_signal DeviceAdded : OBus_proxy.broken
OP_signal DeviceRemoved : OBus_proxy.broken
OP_signal NewCapability : OBus_proxy.broken * string
OP_signal GlobalInterfaceLockAcquired : string * string * int
OP_signal GlobalInterfaceLockReleased : string * string * int
