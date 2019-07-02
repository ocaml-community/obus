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

include OBus_proxy.Private


let manager () =
  let%lwt bus = OBus_bus.system () in
  return (OBus_proxy.make
            (OBus_peer.make bus "org.freedesktop.Hal")
            [ "org"; "freedesktop"; "Hal"; "Manager" ])

open Hal_interfaces.Org_freedesktop_Hal_Manager

let make_device context udi =
  Hal_device.of_proxy
    (OBus_proxy.make (OBus_context.sender context)
       (OBus_path.of_string udi))

let get_all_devices proxy =
  let%lwt context, l = OBus_method.call_with_context m_GetAllDevices proxy () in
  return (List.map (make_device context) l)

let get_all_devices_with_properties proxy =
  let%lwt context, l = OBus_method.call_with_context m_GetAllDevicesWithProperties proxy () in
  return (List.map
            (fun (udi, properties) ->
               (make_device context udi,
                List.map (fun (name, value) -> (name, Hal_device.property_of_variant value)) properties))
            l)

let device_exists proxy udi =
  OBus_method.call m_DeviceExists proxy (OBus_path.to_string udi)

let find_device_string_match proxy key value =
  let%lwt context, l = OBus_method.call_with_context m_FindDeviceStringMatch proxy (key, value) in
  return (List.map (make_device context) l)

let find_device_by_capability proxy capability =
  let%lwt context, l = OBus_method.call_with_context m_FindDeviceByCapability proxy capability in
  return (List.map (make_device context) l)

let new_device proxy =
  let%lwt context, udi = OBus_method.call_with_context m_NewDevice proxy () in
  return (make_device context udi)

let remove proxy dev =
  OBus_method.call m_Remove proxy (OBus_path.to_string (Hal_device.udi dev))

let commit_to_gdl proxy temporary_udi global_udi =
  OBus_method.call m_CommitToGdl proxy (temporary_udi, global_udi)

let acquire_global_interface_lock proxy interface_name exclusive =
  OBus_method.call m_AcquireGlobalInterfaceLock proxy (interface_name, exclusive)

let release_global_interface_lock proxy interface_name =
  OBus_method.call m_ReleaseGlobalInterfaceLock proxy interface_name

let singleton_addon_is_ready proxy command_line =
  OBus_method.call m_SingletonAddonIsReady proxy command_line

let device_added proxy =
  OBus_signal.map_with_context
    make_device
    (OBus_signal.make s_DeviceAdded proxy)

let device_removed proxy =
  OBus_signal.map_with_context
    make_device
    (OBus_signal.make s_DeviceRemoved proxy)

let new_capability proxy =
  OBus_signal.map_with_context
    (fun context (udi, cap) -> (make_device context udi, cap))
    (OBus_signal.make s_NewCapability proxy)

let global_interface_lock_acquired proxy =
  OBus_signal.map
    (fun (interface_name, lock_holder, num_locks) ->
       let num_locks = Int32.to_int num_locks in
       (interface_name, lock_holder, num_locks))
    (OBus_signal.make s_GlobalInterfaceLockAcquired proxy)

let global_interface_lock_released proxy =
  OBus_signal.map
    (fun (interface_name, lock_holder, num_locks) ->
       let num_locks = Int32.to_int num_locks in
       (interface_name, lock_holder, num_locks))
    (OBus_signal.make s_GlobalInterfaceLockReleased proxy)
