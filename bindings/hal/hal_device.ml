(*
 * hal_device.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module Make_interf(Name : sig val name : string end) =
  OBus_client.Make_uniq
    (struct
       let name = Name.name
       let service = Some "org.freedesktop.Hal"
       let connection = OBus_bus.system
     end)

include Make_interf(struct let name = "org.freedesktop.Hal.Device" end)

type udi = OBus_path.t
let computer = "/org/freedesktop/devices/computer"

type property =
  | Pstring of string
  | Pstrlist of string list
  | Pint of int32
  | Puint64 of int64
  | Pbool of bool
  | Pdouble of float

open OBus_types
open OBus_value

let ob_property = OBus_comb.wrap ob_variant
  (function
     | Basic (String s) -> Pstring s
     | Array(Tbasic Tstring, _) as l -> Pstrlist(cast_single (tarray tstring) l)
     | Basic (Int32 x) -> Pint x
     | Basic (Uint64 x) -> Puint64 x
     | Basic (Boolean x) -> Pbool x
     | Basic (Double x) -> Pdouble x
     | v -> failwith ("invalid device property: " ^ OBus_value.string_of_single v))
  (function
     | Pstring s -> vbasic (String s)
     | Pstrlist l -> make_single (tarray tstring) l
     | Pint x -> vbasic (Int32 x)
     | Puint64 x -> vbasic (Uint64 x)
     | Pbool x -> vbasic (Boolean x)
     | Pdouble x -> vbasic (Double x))
let ob_udi = ob_path

let get_all_properties p = call p "GetAllProperties" [: (string, property) assoc ]
let set_multiple_properties p = call p "SetMultipleProperties" [: (string, property) assoc -> unit ]
let get_property p = call p "GetProperty" [: string -> property ]
let get_property_string p = call p "GetPropertyString" [: string -> string ]
let get_property_string_list p = call p "GetPropertyStringList" [: string -> string list ]
let get_property_integer p = call p "GetPropertyInteger" [: string -> int ]
let get_property_boolean p = call p "GetPropertyBoolean" [: string -> bool ]
let get_property_double p = call p "GetPropertyDouble" [: string -> float ]
let set_property p = call p "SetProperty" [: string -> property -> unit ]
let set_property_string p = call p "SetPropertyString" [: string -> string -> unit ]
let set_property_string_list p = call p "SetPropertyStringList" [: string -> string list -> unit ]
let set_property_integer p = call p "SetPropertyInteger" [: string -> int -> unit ]
let set_property_boolean p = call p "SetPropertyBoolean" [: string -> bool -> unit ]
let set_property_double p = call p "SetPropertyDouble" [: string -> float -> unit ]
let remove_property p = call p "RemoveProperty" [: string -> unit ]
let get_property_type p = call p "GetPropertyType" [: string -> int ]
let property_exists p = call p "PropertyExists" [: string -> bool ]
let add_capability p = call p "AddCapability" [: string -> unit ]
let query_capability p = call p "QueryCapability" [: string -> bool ]
let lock p = call p "Lock" [: string -> bool ]
let unlock p = call p "Unlock" [: bool ]
let acquire_interface_lock p = call p "AcquireInterfaceLock" [: string -> bool -> unit ]
let release_interface_lock p = call p "ReleaseInterfaceLock" [: string -> unit ]
let is_caller_locked_out p = call p "IsCallerLockedOut" [: string -> string -> bool ]
let is_caller_privileged p = call p "IsCallerPrivileged" [: string -> string list -> string -> string ]
let is_locked_by_others p = call p "IsLockedByOthers" [: string -> bool ]
let string_list_append p = call p "StringListAppend" [: string -> string -> unit ]
let string_list_prepend p = call p "StringListPrepend" [: string -> string -> unit ]
let string_list_remove p = call p "StringListRemove" [: string -> string -> unit ]
let emit_condition p = call p "EmitCondition" [: string -> string -> bool ]
let rescan p = call p "Rescan" [: bool ]
let reprobe p = call p "Reprobe" [: bool ]
let claim_interface p = call p "ClaimInterface" [: string -> string -> bool ]
let addon_is_ready p = call p "AddonIsReady" [: bool ]

module Volume = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.Volume" end)
  let mount p = call p "Mount" [: string -> string -> string list -> int ]
  let unmount p = call p "Unmount" [: string list -> int ]
  let eject p = call p "Eject" [: string list -> int ]
end
module Storage = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.Storage" end)
  let eject p = call p "Eject" [: string list -> int ]
  let close_tray p = call p "CloseTray" [: string list -> int ]
end
module Storage_removable = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.Storage.Removable" end)
  let check_for_media p = call p "CheckForMedia" [: bool ]
end
module Wake_on_lan = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.WakeOnLan" end)
  let get_supported p = call p "GetSupported" [: int ]
  let get_enabled p = call p "GetEnabled" [: int ]
  let set_enabled p = call p "SetEnabled" [: bool -> int ]
end
module System_power_management = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.SystemPowerManagement" end)
  let suspend p = call p "Suspend" [: int -> int ]
  let suspend_hybrid p = call p "SuspendHybrid" [: int -> int ]
  let hibernate p = call p "Hibernate" [: int ]
  let shutdown p = call p "Shutdown" [: int ]
  let reboot p = call p "Reboot" [: int ]
  let set_power_save p = call p "SetPowerSave" [: bool -> int ]
end
module Cpufreq = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.CPUFreq" end)
  let set_cpufreq_governor p = call p "SetCPUFreqGovernor" [: string -> unit ]
  let set_cpufreq_performance p = call p "SetCPUFreqPerformance" [: int -> unit ]
  let set_cpufreq_consider_nice p = call p "SetCPUFreqConsiderNice" [: bool -> unit ]
  let get_cpufreq_governor p = call p "GetCPUFreqGovernor" [: string ]
  let get_cpufreq_performance p = call p "GetCPUFreqPerformance" [: int ]
  let get_cpufreq_consider_nice p = call p "GetCPUFreqConsiderNice" [: bool ]
  let get_cpufreq_available_governors p = call p "GetCPUFreqAvailableGovernors" [: string list ]
end
module Laptop_panel = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.LaptopPanel" end)
  let set_brightness p = call p "SetBrightness" [: int -> int ]
  let get_brightness p = call p "GetBrightness" [: int ]
end
module Dock_station = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.DockStation" end)
  let undock p = call p "Undock" [: int ]
end
module Kill_switch = struct
  include Make_interf(struct let name = "org.freedesktop.Hal.Device.KillSwitch" end)
  let set_power p = call p "SetPower" [: bool -> int ]
  let get_power p = call p "GetPower" [: int ]
end
