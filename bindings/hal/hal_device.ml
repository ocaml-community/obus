(*
 * hal_device.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type
open OBus_value

module Make_interf(Name : sig val name : string end) =
  OBus_client.Make_constant_bus
    (struct
       let name = Name.name
       let service = Some "org.freedesktop.Hal"
       let bus = OBus_bus.system
     end)

include Make_interf(struct let name = "org.freedesktop.Hal.Device" end)

OBUS_type udi = path
let computer = "/org/freedesktop/devices/computer"

type property =
  | Pstring of string
  | Pstrlist of string list
  | Pint of int32
  | Puint64 of int64
  | Pbool of bool
  | Pdouble of float

let tproperty = wrap_single tvariant
  (function
     | Basic (String s) -> Pstring s
     | Array(Tsingle (Tbasic Tstring), _) as l -> Pstrlist(cast_single (tlist tstring) l)
     | Basic (Int32 x) -> Pint x
     | Basic (Uint64 x) -> Puint64 x
     | Basic (Boolean x) -> Pbool x
     | Basic (Double x) -> Pdouble x
     | v -> failwith ("invalid device property: " ^ OBus_value.string_of_single v))
  (function
     | Pstring s -> vbasic (String s)
     | Pstrlist l -> make_single (tlist tstring) l
     | Pint x -> vbasic (Int32 x)
     | Puint64 x -> vbasic (Uint64 x)
     | Pbool x -> vbasic (Boolean x)
     | Pdouble x -> vbasic (Double x))

let get_all_properties = call "GetAllProperties" << {string, property} list >>
let set_multiple_properties = call "SetMultipleProperties" << {string, property} list -> unit >>
let get_property = call "GetProperty" << string -> property >>
let get_property_string = call "GetPropertyString" << string -> string >>
let get_property_string_list = call "GetPropertyStringList" << string -> string list >>
let get_property_integer = call "GetPropertyInteger" << string -> int >>
let get_property_boolean = call "GetPropertyBoolean" << string -> bool >>
let get_property_double = call "GetPropertyDouble" << string -> float >>
let set_property = call "SetProperty" << string -> property -> unit >>
let set_property_string = call "SetPropertyString" << string -> string -> unit >>
let set_property_string_list = call "SetPropertyStringList" << string -> string list -> unit >>
let set_property_integer = call "SetPropertyInteger" << string -> int -> unit >>
let set_property_boolean = call "SetPropertyBoolean" << string -> bool -> unit >>
let set_property_double = call "SetPropertyDouble" << string -> float -> unit >>
let remove_property = call "RemoveProperty" << string -> unit >>
let get_property_type = call "GetPropertyType" << string -> int >>
let property_exists = call "PropertyExists" << string -> bool >>
let add_capability = call "AddCapability" << string -> unit >>
let query_capability = call "QueryCapability" << string -> bool >>
let lock = call "Lock" << string -> bool >>
let unlock = call "Unlock" << bool >>
let acquire_interface_lock = call "AcquireInterfaceLock" << string -> bool -> unit >>
let release_interface_lock = call "ReleaseInterfaceLock" << string -> unit >>
let is_caller_locked_out = call "IsCallerLockedOut" << string -> string -> bool >>
let is_caller_privileged = call "IsCallerPrivileged" << string -> string list -> string -> string >>
let is_locked_by_others = call "IsLockedByOthers" << string -> bool >>
let string_list_append = call "StringListAppend" << string -> string -> unit >>
let string_list_prepend = call "StringListPrepend" << string -> string -> unit >>
let string_list_remove = call "StringListRemove" << string -> string -> unit >>
let emit_condition = call "EmitCondition" << string -> string -> bool >>
let rescan = call "Rescan" << bool >>
let reprobe = call "Reprobe" << bool >>
let claim_interface = call "ClaimInterface" << string -> string -> bool >>
let addon_is_ready = call "AddonIsReady" << bool >>

let on_property_modified = on_signal "PropertyModified" << int -> [string * bool * bool] list -> unit >>
let on_condition = on_signal "Condition" << string -> string -> unit >>
let on_interface_lock_acquired = on_signal "InterfaceLockAcquired" << string -> string -> int -> unit >>
let on_interface_lock_released = on_signal "InterfaceLockReleased" << string -> string -> int -> unit >>

module Volume = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.Volume" end)
  let mount = call "Mount" << string -> string -> string list -> int >>
  let unmount = call "Unmount" << string list -> int >>
  let eject = call "Eject" << string list -> int >>
end
module Storage = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.Storage" end)
  let eject = call "Eject" << string list -> int >>
  let close_tray = call "CloseTray" << string list -> int >>
end
module Storage_removable = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.Storage.Removable" end)
  let check_for_media = call "CheckForMedia" << bool >>
end
module Wake_on_lan = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.WakeOnLan" end)
  let get_supported = call "GetSupported" << int >>
  let get_enabled = call "GetEnabled" << int >>
  let set_enabled = call "SetEnabled" << bool -> int >>
end
module System_power_management = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.SystemPowerManagement" end)
  let suspend = call "Suspend" << int -> int >>
  let suspend_hybrid = call "SuspendHybrid" << int -> int >>
  let hibernate = call "Hibernate" << int >>
  let shutdown = call "Shutdown" << int >>
  let reboot = call "Reboot" << int >>
  let set_power_save = call "SetPowerSave" << bool -> int >>
end
module Cpufreq = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.CPUFreq" end)
  let set_cpufreq_governor = call "SetCPUFreqGovernor" << string -> unit >>
  let set_cpufreq_performance = call "SetCPUFreqPerformance" << int -> unit >>
  let set_cpufreq_consider_nice = call "SetCPUFreqConsiderNice" << bool -> unit >>
  let get_cpufreq_governor = call "GetCPUFreqGovernor" << string >>
  let get_cpufreq_performance = call "GetCPUFreqPerformance" << int >>
  let get_cpufreq_consider_nice = call "GetCPUFreqConsiderNice" << bool >>
  let get_cpufreq_available_governors = call "GetCPUFreqAvailableGovernors" << string list >>
end
module Laptop_panel = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.LaptopPanel" end)
  let set_brightness = call "SetBrightness" << int -> int >>
  let get_brightness = call "GetBrightness" << int >>
end
module Dock_station = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.DockStation" end)
  let undock = call "Undock" << int >>
end
module Kill_switch = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.KillSwitch" end)
  let set_power = call "SetPower" << bool -> int >>
  let get_power = call "GetPower" << int >>
end
