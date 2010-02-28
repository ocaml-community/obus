(*
 * hal_device.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_value
open OBus_pervasives

type t = OBus_proxy.t
  with obus

type udi = path
  with obus

let udi = OBus_proxy.path

let computer =
  lazy(lwt bus = Lazy.force OBus_bus.system in
       return (OBus_proxy.make
                 (OBus_peer.make bus "org.freedesktop.Hal")
                 ["org"; "freedesktop"; "Hal"; "devices"; "computer"]))


type property =
  | Pstring of string
  | Pstrlist of string list
  | Pint of int32
  | Puint64 of int64
  | Pbool of bool
  | Pdouble of float

let obus_property = OBus_type.map obus_variant
  (function
     | Basic(String s) -> Pstring s
     | Array(Tbasic Tstring, _) as l -> Pstrlist(OBus_type.cast_single (obus_list obus_string) l)
     | Basic(Int32 x) -> Pint x
     | Basic(Uint64 x) -> Puint64 x
     | Basic(Boolean x) -> Pbool x
     | Basic(Double x) -> Pdouble x
     | v -> failwith ("invalid device property: " ^ OBus_value.string_of_single v))
  (function
     | Pstring s -> basic (String s)
     | Pstrlist l -> OBus_type.make_single (obus_list obus_string) l
     | Pint x -> basic (Int32 x)
     | Puint64 x -> basic (Uint64 x)
     | Pbool x -> basic (Boolean x)
     | Pdouble x -> basic (Double x))

OP_interface "org.freedesktop.Hal.Device"

OP_method GetAllProperties : (string, property) dict
OP_method SetMultipleProperties : (string, property) dict -> unit
OP_method GetProperty : string -> property
OP_method GetPropertyString : string -> string
OP_method GetPropertyStringList : string -> string list
OP_method GetPropertyInteger : string -> int
OP_method GetPropertyBoolean : string -> bool
OP_method GetPropertyDouble : string -> float
OP_method SetProperty : string -> property -> unit
OP_method SetPropertyString : string -> string -> unit
OP_method SetPropertyStringList : string -> string list -> unit
OP_method SetPropertyInteger : string -> int -> unit
OP_method SetPropertyBoolean : string -> bool -> unit
OP_method SetPropertyDouble : string -> float -> unit
OP_method RemoveProperty : string -> unit
OP_method GetPropertyType : string -> int
OP_method PropertyExists : string -> bool
OP_method AddCapability : string -> unit
OP_method QueryCapability : string -> bool
OP_method Lock : string -> bool
OP_method Unlock : bool
OP_method AcquireInterfaceLock : string -> bool -> unit
OP_method ReleaseInterfaceLock : string -> unit
OP_method IsCallerLockedOut : string -> string -> bool
OP_method IsCallerPrivileged : string -> string list -> string -> string
OP_method IsLockedByOthers : string -> bool
OP_method StringListAppend : string -> string -> unit
OP_method StringListPrepend : string -> string -> unit
OP_method StringListRemove : string -> string -> unit
OP_method EmitCondition : string -> string -> bool
OP_method Rescan : bool
OP_method Reprobe : bool
OP_method ClaimInterface : string -> string -> bool
OP_method AddonIsReady : bool

OP_signal PropertyModified : int * (string * bool * bool) structure list
OP_signal Condition : string * string
OP_signal InterfaceLockAcquired : string * string * int
OP_signal InterfaceLockReleased : string * string * int

module Volume = struct
  OP_interface "org.freedesktop.Hal.Device.Volume"
  OP_method Mount : string -> string -> string list -> int
  OP_method Unmount : string list -> int
  OP_method Eject : string list -> int
end
module Storage = struct
  OP_interface "org.freedesktop.Hal.Device.Storage"
  OP_method Eject : string list -> int
  OP_method CloseTray : string list -> int
end
module Storage_removable = struct
  OP_interface "org.freedesktop.Hal.Device.Storage.Removable"
  OP_method CheckForMedia : bool
end
module Wake_on_lan = struct
  OP_interface "org.freedesktop.Hal.Device.WakeOnLan"
  OP_method GetSupported : int
  OP_method GetEnabled : int
  OP_method SetEnabled : bool -> int
end
module System_power_management = struct
  OP_interface "org.freedesktop.Hal.Device.SystemPowerManagement"
  OP_method Suspend : int -> int
  OP_method SuspendHybrid : int -> int
  OP_method Hibernate : int
  OP_method Shutdown : int
  OP_method Reboot : int
  OP_method SetPowerSave : bool -> int
end
module Cpufreq = struct
  OP_interface "org.freedesktop.Hal.Device.CPUFreq"
  OP_method SetCPUFreqGovernor : string -> unit
  OP_method SetCPUFreqPerformance : int -> unit
  OP_method SetCPUFreqConsiderNice : bool -> unit
  OP_method GetCPUFreqGovernor : string
  OP_method GetCPUFreqPerformance : int
  OP_method GetCPUFreqConsiderNice : bool
  OP_method GetCPUFreqAvailableGovernors : string list
end
module Laptop_panel = struct
  OP_interface "org.freedesktop.Hal.Device.LaptopPanel"
  OP_method SetBrightness : int -> int
  OP_method GetBrightness : int
end
module Dock_station = struct
  OP_interface "org.freedesktop.Hal.Device.DockStation"
  OP_method Undock : int
end
module Kill_switch = struct
  OP_interface "org.freedesktop.Hal.Device.KillSwitch"
  OP_method SetPower : bool -> int
  OP_method GetPower : int
end
