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

exception General_error of string
 with obus("org.freedesktop.UPower.Device.GeneralError")

type typ =
    [ `Unknown
    | `Line_power
    | `Battery
    | `Ups
    | `Monitor
    | `Mouse
    | `Keyboard
    | `Pda
    | `Phone ]

let obus_typ = OBus_type.mapping obus_uint [
  `Unknown, 0;
  `Line_power, 1;
  `Battery, 2;
  `Ups, 3;
  `Monitor, 4;
  `Mouse, 5;
  `Keyboard, 6;
  `Pda, 7;
  `Phone, 8;
]

type state =
    [ `Unknown
    | `Charging
    | `Discharging
    | `Empty
    | `Fully_charged
    | `Pending_charge
    | `Pending_discharge ]

let obus_state = OBus_type.mapping obus_uint [
  `Unknown, 0;
  `Charging, 1;
  `Discharging, 2;
  `Empty, 3;
  `Fully_charged, 4;
  `Pending_charge, 5;
  `Pending_discharge, 6;
]

type technology =
    [ `Unknown
    | `Lithium_ion
    | `Lithium_polymer
    | `Lithium_iron_phosphate
    | `Lead_acid
    | `Nickel_cadmium
    | `Nickel_metal_hydride ]

let obus_technology = OBus_type.mapping obus_uint [
  `Unknown, 0;
  `Lithium_ion, 1;
  `Lithium_polymer, 2;
  `Lithium_iron_phosphate, 3;
  `Lead_acid, 4;
  `Nickel_cadmium, 5;
  `Nickel_metal_hydride, 6;
]

let op_interface = OBus_proxy.make_interface ~notify:(OBus_property.notify_global "Changed") "org.freedesktop.UPower.Device"

OP_method GetStatistics : string -> (float * float) structure list
OP_method GetHistory : string -> uint -> uint -> (uint * float * uint) structure list
OP_method Refresh : unit

OP_signal Changed : unit

OP_property_rw RecallUrl : string
OP_property_rw RecallVendor : string
OP_property_rw RecallNotice : bool
OP_property_rw Technology : technology
OP_property_rw Capacity : float
OP_property_rw IsRechargeable : bool
OP_property_rw State : state
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
OP_property_rw Type as typ : typ
OP_property_rw UpdateTime : uint64
OP_property_rw Serial : string
OP_property_rw Model : string
OP_property_rw Vendor : string
OP_property_rw NativePath : string
