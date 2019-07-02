(*
 * nm_manager.ml
 * -------------
 * Copyright : (c) 2010, Pierre Chambart <chambart@crans.org>
 *                 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

let section = Lwt_log.Section.make "network-manager"

include OBus_peer.Private

let daemon () =
  let%lwt bus = OBus_bus.system () in
  Lwt.return (OBus_peer.make bus "org.freedesktop.NetworkManager")

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type state =
  [ `Unknown
  | `Asleep
  | `Connecting
  | `Connected
  | `Disconnected ]

let state_of_int32 = function
  | 0l -> `Unknown
  | 1l -> `Asleep
  | 2l -> `Connecting
  | 3l -> `Connected
  | 4l -> `Disconnected
  | i -> ignore (Lwt_log.warning_f ~section "Nm_manager.state_of_int32: unknown state: %ld" i); `Unknown

(* +-----------------------------------------------------------------+
   | D-Bus definitions                                               |
   +-----------------------------------------------------------------+ *)

let proxy daemon = OBus_proxy.make daemon ["org"; "freedesktop"; "NetworkManager"]

open Nm_interfaces.Org_freedesktop_NetworkManager

let get_devices daemon =
  let%lwt (context, devices) = OBus_method.call_with_context m_GetDevices (proxy daemon) () in
  return (
    List.map
      (fun path ->
         Nm_device.of_proxy
           (OBus_proxy.make (OBus_context.sender context) path))
      devices
  )

let activate_connection daemon ~service_name ~connection ~device ~specific_object =
  let connection = OBus_proxy.path (Nm_settings.Connection.to_proxy connection) in
  let device = OBus_proxy.path (Nm_device.to_proxy device) in
  let specific_object = OBus_proxy.path specific_object in
  let%lwt (context, active_connection) =
    OBus_method.call_with_context
      m_ActivateConnection
      (proxy daemon)
      (service_name, connection, device, specific_object)
  in
  return (
    Nm_connection.of_proxy
      (OBus_proxy.make (OBus_context.sender context) active_connection)
  )

let deactivate_connection daemon ~active_connection =
  let active_connection = OBus_proxy.path (Nm_connection.to_proxy active_connection) in
  OBus_method.call m_DeactivateConnection (proxy daemon) active_connection

let sleep daemon ~sleep =
  OBus_method.call m_Sleep (proxy daemon) sleep

let wireless_enabled daemon =
  OBus_property.make ~monitor:Nm_monitor.monitor p_WirelessEnabled (proxy daemon)

let wireless_hardware_enabled daemon =
  OBus_property.make ~monitor:Nm_monitor.monitor p_WirelessHardwareEnabled (proxy daemon)

let wwan_enabled daemon =
  OBus_property.make ~monitor:Nm_monitor.monitor p_WwanEnabled (proxy daemon)

let wwan_hardware_enabled daemon =
  OBus_property.make ~monitor:Nm_monitor.monitor p_WwanHardwareEnabled (proxy daemon)

let active_connections daemon =
  OBus_property.map_r_with_context
    (fun context paths ->
       List.map
         (fun path ->
            Nm_connection.of_proxy
              (OBus_proxy.make (OBus_context.sender context) path))
         paths)
    (OBus_property.make ~monitor:Nm_monitor.monitor p_ActiveConnections (proxy daemon))

let state daemon =
  OBus_property.map_r
    state_of_int32
    (OBus_property.make ~monitor:Nm_monitor.monitor p_State (proxy daemon))

let state_changed daemon =
  OBus_signal.map
    state_of_int32
    (OBus_signal.make s_StateChanged (proxy daemon))

let properties_changed daemon =
  OBus_signal.make s_PropertiesChanged (proxy daemon)

let device_added daemon =
  OBus_signal.map_with_context
    (fun context state ->
       Nm_device.of_proxy (OBus_proxy.make (OBus_context.sender context) state))
    (OBus_signal.make s_DeviceAdded (proxy daemon))

let device_removed daemon =
  OBus_signal.map_with_context
    (fun context state ->
       Nm_device.of_proxy (OBus_proxy.make (OBus_context.sender context) state))
    (OBus_signal.make s_DeviceRemoved (proxy daemon))

let properties daemon =
  OBus_property.group ~monitor:Nm_monitor.monitor (proxy daemon) interface
