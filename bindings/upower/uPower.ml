(*
 * uPower.ml
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_pervasives

include OBus_peer.Private

exception General_error of string
 with obus("org.freedesktop.UPower.GeneralError")

let daemon () =
  lwt bus = OBus_bus.system () in
  return (OBus_peer.make bus "org.freedesktop.UPower")

module Proxy = OBus_proxy.Make(struct
                                 type proxy = t
                                 let cast peer = OBus_proxy.make peer ["org"; "freedesktop"; "UPower"]
                                 let make = OBus_proxy.peer
                               end)

let op_interface = Proxy.make_interface ~changed:"Changed" "org.freedesktop.UPower"

OP_method HibernateAllowed : bool
OP_method Hibernate : unit
OP_method SuspendAllowed : bool
OP_method Suspend : unit
OP_method AboutToSleep : unit
OP_method EnumerateDevices : UPower_device.t list

OP_signal Resuming : unit
OP_signal Sleeping : unit
OP_signal Changed : unit
OP_signal DeviceChanged : UPower_device.broken
OP_signal DeviceRemoved : UPower_device.broken
OP_signal DeviceAdded : UPower_device.broken

OP_property_rw LidIsPresent : bool
OP_property_rw LidIsClosed : bool
OP_property_rw OnLowBattery : bool
OP_property_rw OnBattery : bool
OP_property_r CanHibernate : bool
OP_property_r CanSuspend : bool
OP_property_r DaemonVersion : string
