(*
 * uPower_device.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_pervasives

include OBus_proxy.Private

let op_interface = OBus_proxy.make_interface "org.freedesktop.UPower.Device"

OP_method GetStatistics : string -> (float * float) structure list
OP_method GetHistory : string -> uint -> uint -> (uint * float * uint) structure list
OP_method Refresh : unit
OP_signal Changed : unit
OP_property_rw RecallUrl : string
OP_property_rw RecallVendor : string
OP_property_rw RecallNotice : bool
OP_property_rw Technology : uint
OP_property_rw Capacity : float
OP_property_rw IsRechargeable : bool
OP_property_rw State : uint
OP_property_rw IsPresent : bool
OP_property_rw Percentage : float
OP_property_rw TimeToFull : int64
OP_property_rw TimeToEmpty : int64
OP_property_rw Voltage : float
OP_property_rw EnergyRate : float
OP_property_rw EnergyFullDesign : float
OP_property_rw EnergyFull : float
OP_property_rw EnergyEmpty : float
OP_property_rw Energy : float
OP_property_rw Online : bool
OP_property_rw HasStatistics : bool
OP_property_rw HasHistory : bool
OP_property_rw PowerSupply : bool
OP_property_rw Type as typ : uint
OP_property_rw UpdateTime : uint64
OP_property_rw Serial : string
OP_property_rw Model : string
OP_property_rw Vendor : string
OP_property_rw NativePath : string
