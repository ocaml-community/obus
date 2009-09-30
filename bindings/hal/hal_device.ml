(*
 * hal_device.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_value
open OBus_type.Perv

let get_peer =
  lazy(lwt bus = Lazy.force OBus_bus.system in
       return (OBus_peer.make bus "org.freedesktop.Hal"))

type udi = path
  with obus

external make : OBus_path.t -> udi = "%identity"
external path : udi -> OBus_path.t = "%identity"

module Make_interface =
  OBus_interface.Make_custom(struct
                               type t = udi
                               let make_proxy udi =
                                 (lwt peer = Lazy.force get_peer in
                                  return (OBus_proxy.make peer udi))
                             end)

module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device" end)

let computer = ["org"; "freedesktop"; "Hal"; "devices"; "computer"]

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

OBUS_method GetAllProperties : (string, property) dict
OBUS_method SetMultipleProperties : (string, property) dict -> unit
OBUS_method GetProperty : string -> property
OBUS_method GetPropertyString : string -> string
OBUS_method GetPropertyStringList : string -> string list
OBUS_method GetPropertyInteger : string -> int
OBUS_method GetPropertyBoolean : string -> bool
OBUS_method GetPropertyDouble : string -> float
OBUS_method SetProperty : string -> property -> unit
OBUS_method SetPropertyString : string -> string -> unit
OBUS_method SetPropertyStringList : string -> string list -> unit
OBUS_method SetPropertyInteger : string -> int -> unit
OBUS_method SetPropertyBoolean : string -> bool -> unit
OBUS_method SetPropertyDouble : string -> float -> unit
OBUS_method RemoveProperty : string -> unit
OBUS_method GetPropertyType : string -> int
OBUS_method PropertyExists : string -> bool
OBUS_method AddCapability : string -> unit
OBUS_method QueryCapability : string -> bool
OBUS_method Lock : string -> bool
OBUS_method Unlock : bool
OBUS_method AcquireInterfaceLock : string -> bool -> unit
OBUS_method ReleaseInterfaceLock : string -> unit
OBUS_method IsCallerLockedOut : string -> string -> bool
OBUS_method IsCallerPrivileged : string -> string list -> string -> string
OBUS_method IsLockedByOthers : string -> bool
OBUS_method StringListAppend : string -> string -> unit
OBUS_method StringListPrepend : string -> string -> unit
OBUS_method StringListRemove : string -> string -> unit
OBUS_method EmitCondition : string -> string -> bool
OBUS_method Rescan : bool
OBUS_method Reprobe : bool
OBUS_method ClaimInterface : string -> string -> bool
OBUS_method AddonIsReady : bool

OBUS_signal PropertyModified : int * (string * bool * bool) structure list
OBUS_signal Condition : string * string
OBUS_signal InterfaceLockAcquired : string * string * int
OBUS_signal InterfaceLockReleased : string * string * int

module Volume = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.Volume" end)
  OBUS_method Mount : string -> string -> string list -> int
  OBUS_method Unmount : string list -> int
  OBUS_method Eject : string list -> int
end
module Storage = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.Storage" end)
  OBUS_method Eject : string list -> int
  OBUS_method CloseTray : string list -> int
end
module Storage_removable = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.Storage.Removable" end)
  OBUS_method CheckForMedia : bool
end
module Wake_on_lan = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.WakeOnLan" end)
  OBUS_method GetSupported : int
  OBUS_method GetEnabled : int
  OBUS_method SetEnabled : bool -> int
end
module System_power_management = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.SystemPowerManagement" end)
  OBUS_method Suspend : int -> int
  OBUS_method SuspendHybrid : int -> int
  OBUS_method Hibernate : int
  OBUS_method Shutdown : int
  OBUS_method Reboot : int
  OBUS_method SetPowerSave : bool -> int
end
module Cpufreq = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.CPUFreq" end)
  OBUS_method SetCPUFreqGovernor : string -> unit
  OBUS_method SetCPUFreqPerformance : int -> unit
  OBUS_method SetCPUFreqConsiderNice : bool -> unit
  OBUS_method GetCPUFreqGovernor : string
  OBUS_method GetCPUFreqPerformance : int
  OBUS_method GetCPUFreqConsiderNice : bool
  OBUS_method GetCPUFreqAvailableGovernors : string list
end
module Laptop_panel = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.LaptopPanel" end)
  OBUS_method SetBrightness : int -> int
  OBUS_method GetBrightness : int
end
module Dock_station = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.DockStation" end)
  OBUS_method Undock : int
end
module Kill_switch = struct
  module OBUS_INTERFACE = Make_interface(struct let name = "org.freedesktop.Hal.Device.KillSwitch" end)
  OBUS_method SetPower : bool -> int
  OBUS_method GetPower : int
end
