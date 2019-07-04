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
open Hal_interfaces

include OBus_proxy.Private

type udi = OBus_path.t

let udi = OBus_proxy.path

let computer () =
  let%lwt bus = OBus_bus.system () in
  return (OBus_proxy.make
            (OBus_peer.make bus "org.freedesktop.Hal")
            ["org"; "freedesktop"; "Hal"; "devices"; "computer"])


type property =
  | Pstring of string
  | Pstrlist of string list
  | Pint of int32
  | Puint64 of int64
  | Pbool of bool
  | Pdouble of float

let property_of_variant = function
  | V.Basic(V.String s) -> Pstring s
  | V.Array(T.Basic T.String, _) as l -> Pstrlist(C.cast_single (C.array C.basic_string) l)
  | V.Basic(V.Int32 x) -> Pint x
  | V.Basic(V.Uint64 x) -> Puint64 x
  | V.Basic(V.Boolean x) -> Pbool x
  | V.Basic(V.Double x) -> Pdouble x
  | v -> Printf.ksprintf failwith "Hal_device.property_of_variant: invalid device property: %s" (V.string_of_single v)

let variant_of_property = function
  | Pstring s -> V.basic_string s
  | Pstrlist l -> C.make_single (C.array C.basic_string) l
  | Pint x -> V.basic_int32 x
  | Puint64 x -> V.basic_uint64 x
  | Pbool x -> V.basic_boolean x
  | Pdouble x -> V.basic_double x

open Org_freedesktop_Hal_Device

let get_all_properties proxy =
  let%lwt l = OBus_method.call m_GetAllProperties proxy () in
  return (List.map (fun (name, value) -> (name, property_of_variant value)) l)

let set_multiple_properties proxy properties =
  OBus_method.call m_SetMultipleProperties proxy
    (List.map (fun (name, property) -> (name, variant_of_property property)) properties)

let get_property proxy key =
  OBus_method.call m_GetProperty proxy key >|= property_of_variant

let get_property_string proxy key =
  OBus_method.call m_GetPropertyString proxy key

let get_property_string_list proxy key =
  OBus_method.call m_GetPropertyStringList proxy key

let get_property_integer proxy key =
  let%lwt value = OBus_method.call m_GetPropertyInteger proxy key in
  let value = Int32.to_int value in
  return value

let get_property_boolean proxy key =
  OBus_method.call m_GetPropertyBoolean proxy key

let get_property_double proxy key =
  OBus_method.call m_GetPropertyDouble proxy key

let set_property proxy key value =
  OBus_method.call m_SetProperty proxy (key, variant_of_property value)

let set_property_string proxy key value =
  OBus_method.call m_SetPropertyString proxy (key, value)

let set_property_string_list proxy key value =
  OBus_method.call m_SetPropertyStringList proxy (key, value)

let set_property_integer proxy key value =
  let value = Int32.of_int value in
  OBus_method.call m_SetPropertyInteger proxy (key, value)

let set_property_boolean proxy key value =
  OBus_method.call m_SetPropertyBoolean proxy (key, value)

let set_property_double proxy key value =
  OBus_method.call m_SetPropertyDouble proxy (key, value)

let remove_property proxy key =
  OBus_method.call m_RemoveProperty proxy key

let get_property_type proxy key =
  let%lwt typ = OBus_method.call m_GetPropertyType proxy key in
  let typ = Int32.to_int typ in
  return typ

let property_exists proxy key =
  OBus_method.call m_PropertyExists proxy key

let add_capability proxy capability =
  OBus_method.call m_AddCapability proxy capability

let query_capability proxy capability =
  OBus_method.call m_QueryCapability proxy capability

let lock proxy reason =
  OBus_method.call m_Lock proxy reason

let unlock proxy =
  OBus_method.call m_Unlock proxy ()

let acquire_interface_lock proxy interface_name exclusive =
  OBus_method.call m_AcquireInterfaceLock proxy (interface_name, exclusive)

let release_interface_lock proxy interface_name =
  OBus_method.call m_ReleaseInterfaceLock proxy interface_name

let is_caller_locked_out proxy interface_name caller_sysbus_name =
  OBus_method.call m_IsCallerLockedOut proxy (interface_name, caller_sysbus_name)

let is_caller_privileged proxy action caller_sysbus_name =
  OBus_method.call m_IsCallerPrivileged proxy (action, caller_sysbus_name)

let is_locked_by_others proxy interface_name =
  OBus_method.call m_IsLockedByOthers proxy interface_name

let string_list_append proxy key value =
  OBus_method.call m_StringListAppend proxy (key, value)

let string_list_prepend proxy key value =
  OBus_method.call m_StringListPrepend proxy (key, value)

let string_list_remove proxy key value =
  OBus_method.call m_StringListRemove proxy (key, value)

let emit_condition proxy condition_name condition_details =
  OBus_method.call m_EmitCondition proxy (condition_name, condition_details)

let rescan proxy =
  OBus_method.call m_Rescan proxy ()

let reprobe proxy =
  OBus_method.call m_Reprobe proxy ()

let claim_interface proxy interface_name introspection_xml =
  OBus_method.call m_ClaimInterface proxy (interface_name, introspection_xml)

let addon_is_ready proxy =
  OBus_method.call m_AddonIsReady proxy ()

let property_modified proxy =
  OBus_signal.map
    (fun (num_updates, updates) ->
       let num_updates = Int32.to_int num_updates in
       (num_updates, updates))
    (OBus_signal.make s_PropertyModified proxy)

let condition proxy =
  OBus_signal.make s_Condition proxy

let interface_lock_acquired proxy =
  OBus_signal.map
    (fun (interface_name, lock_holder, num_locks) ->
       let num_locks = Int32.to_int num_locks in
       (interface_name, lock_holder, num_locks))
    (OBus_signal.make s_InterfaceLockAcquired proxy)

let interface_lock_released proxy =
  OBus_signal.map
    (fun (interface_name, lock_holder, num_locks) ->
       let num_locks = Int32.to_int num_locks in
       (interface_name, lock_holder, num_locks))
    (OBus_signal.make s_InterfaceLockReleased proxy)

module Volume = struct
  open Org_freedesktop_Hal_Device_Volume

  let mount proxy mount_point fstype extra_options =
    let%lwt return_code = OBus_method.call m_Mount proxy (mount_point, fstype, extra_options) in
    let return_code = Int32.to_int return_code in
    return return_code

  let unmount proxy extra_options =
    let%lwt return_code = OBus_method.call m_Unmount proxy extra_options in
    let return_code = Int32.to_int return_code in
    return return_code

  let eject proxy extra_options =
    let%lwt return_code = OBus_method.call m_Eject proxy extra_options in
    let return_code = Int32.to_int return_code in
    return return_code
end

module Storage = struct
  open Org_freedesktop_Hal_Device_Storage

  let eject proxy extra_options =
    let%lwt return_code = OBus_method.call m_Eject proxy extra_options in
    let return_code = Int32.to_int return_code in
    return return_code

  let close_tray proxy extra_options =
    let%lwt return_code = OBus_method.call m_CloseTray proxy extra_options in
    let return_code = Int32.to_int return_code in
    return return_code
end

module Storage_removable = struct
  open Org_freedesktop_Hal_Device_Storage_Removable

  let check_for_media proxy =
    OBus_method.call m_CheckForMedia proxy ()
end

module Wake_on_lan = struct
  open Org_freedesktop_Hal_Device_WakeOnLan

  let get_supported proxy =
    let%lwt return_code = OBus_method.call m_GetSupported proxy () in
    let return_code = Int32.to_int return_code in
    return return_code

  let get_enabled proxy =
    let%lwt return_code = OBus_method.call m_GetEnabled proxy () in
    let return_code = Int32.to_int return_code in
    return return_code

  let set_enabled proxy enable =
    let%lwt return_code = OBus_method.call m_SetEnabled proxy enable in
    let return_code = Int32.to_int return_code in
    return return_code
end

module System_power_management = struct
  open Org_freedesktop_Hal_Device_SystemPowerManagement

  let suspend proxy num_seconds_to_sleep =
    let num_seconds_to_sleep = Int32.of_int num_seconds_to_sleep in
    let%lwt return_code = OBus_method.call m_Suspend proxy num_seconds_to_sleep in
    let return_code = Int32.to_int return_code in
    return return_code

  let suspend_hybrid proxy num_seconds_to_sleep =
    let num_seconds_to_sleep = Int32.of_int num_seconds_to_sleep in
    let%lwt return_code = OBus_method.call m_SuspendHybrid proxy num_seconds_to_sleep in
    let return_code = Int32.to_int return_code in
    return return_code

  let hibernate proxy =
    let%lwt return_code = OBus_method.call m_Hibernate proxy () in
    let return_code = Int32.to_int return_code in
    return return_code

  let shutdown proxy =
    let%lwt return_code = OBus_method.call m_Shutdown proxy () in
    let return_code = Int32.to_int return_code in
    return return_code

  let reboot proxy =
    let%lwt return_code = OBus_method.call m_Reboot proxy () in
    let return_code = Int32.to_int return_code in
    return return_code

  let set_power_save proxy enable_power_save =
    let%lwt return_code = OBus_method.call m_SetPowerSave proxy enable_power_save in
    let return_code = Int32.to_int return_code in
    return return_code
end

module Cpufreq = struct
  open Org_freedesktop_Hal_Device_CPUFreq

  let set_cpufreq_governor proxy governor_string =
    OBus_method.call m_SetCPUFreqGovernor proxy governor_string

  let set_cpufreq_performance proxy value =
    let value = Int32.of_int value in
    OBus_method.call m_SetCPUFreqPerformance proxy value

  let set_cpufreq_consider_nice proxy value =
    OBus_method.call m_SetCPUFreqConsiderNice proxy value

  let get_cpufreq_governor proxy =
    OBus_method.call m_GetCPUFreqGovernor proxy ()

  let get_cpufreq_performance proxy =
    let%lwt return_code = OBus_method.call m_GetCPUFreqPerformance proxy () in
    let return_code = Int32.to_int return_code in
    return return_code

  let get_cpufreq_consider_nice proxy =
    OBus_method.call m_GetCPUFreqConsiderNice proxy ()

  let get_cpufreq_available_governors proxy =
    OBus_method.call m_GetCPUFreqAvailableGovernors proxy ()
end

module Laptop_panel = struct
  open Org_freedesktop_Hal_Device_LaptopPanel

  let set_brightness proxy brightness_value =
    let brightness_value = Int32.of_int brightness_value in
    let%lwt return_code = OBus_method.call m_SetBrightness proxy brightness_value in
    let return_code = Int32.to_int return_code in
    return return_code

  let get_brightness proxy =
    let%lwt brightness_value = OBus_method.call m_GetBrightness proxy () in
    let brightness_value = Int32.to_int brightness_value in
    return brightness_value
end

module Kill_switch = struct
  open Org_freedesktop_Hal_Device_KillSwitch

  let set_power proxy value =
    let%lwt return_code = OBus_method.call m_SetPower proxy value in
    let return_code = Int32.to_int return_code in
    return return_code

  let get_power proxy =
    let%lwt value = OBus_method.call m_GetPower proxy () in
    let value = Int32.to_int value in
    return value
end
