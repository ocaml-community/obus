(*
 * uPower.ml
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

include OBus_peer.Private

let general_error = "org.freedesktop.UPower.GeneralError"

let daemon () =
  let%lwt bus = OBus_bus.system () in
  return (OBus_peer.make bus "org.freedesktop.UPower")

open UPower_interfaces.Org_freedesktop_UPower

let proxy daemon = OBus_proxy.make daemon ["org"; "freedesktop"; "UPower"]

let enumerate_devices daemon =
  let%lwt (context, devices) = OBus_method.call_with_context m_EnumerateDevices (proxy daemon) () in
  return
    (List.map
       (fun path ->
          UPower_device.of_proxy
            (OBus_proxy.make (OBus_context.sender context) path))
       devices)

let device_added daemon =
  OBus_signal.map_with_context
    (fun context device ->
       UPower_device.of_proxy (OBus_proxy.make (OBus_context.sender context) (OBus_path.of_string device)))
    (OBus_signal.make s_DeviceAdded (proxy daemon))

let device_removed daemon =
  OBus_signal.map_with_context
    (fun context device ->
       UPower_device.of_proxy (OBus_proxy.make (OBus_context.sender context) (OBus_path.of_string device)))
    (OBus_signal.make s_DeviceRemoved (proxy daemon))

let device_changed daemon =
  OBus_signal.map_with_context
    (fun context device ->
       UPower_device.of_proxy (OBus_proxy.make (OBus_context.sender context) (OBus_path.of_string device)))
    (OBus_signal.make s_DeviceChanged (proxy daemon))

let changed daemon =
  OBus_signal.make s_Changed (proxy daemon)

let sleeping daemon =
  OBus_signal.make s_Sleeping (proxy daemon)

let resuming daemon =
  OBus_signal.make s_Resuming (proxy daemon)

let about_to_sleep daemon =
  OBus_method.call m_AboutToSleep (proxy daemon) ()

let suspend daemon =
  OBus_method.call m_Suspend (proxy daemon) ()

let suspend_allowed daemon =
  OBus_method.call m_SuspendAllowed (proxy daemon) ()

let hibernate daemon =
  OBus_method.call m_Hibernate (proxy daemon) ()

let hibernate_allowed daemon =
  OBus_method.call m_HibernateAllowed (proxy daemon) ()

let daemon_version daemon =
  OBus_property.make ~monitor:UPower_monitor.monitor p_DaemonVersion (proxy daemon)

let can_suspend daemon =
  OBus_property.make ~monitor:UPower_monitor.monitor p_CanSuspend (proxy daemon)

let can_hibernate daemon =
  OBus_property.make ~monitor:UPower_monitor.monitor p_CanHibernate (proxy daemon)

let on_battery daemon =
  OBus_property.make ~monitor:UPower_monitor.monitor p_OnBattery (proxy daemon)

let on_low_battery daemon =
  OBus_property.make ~monitor:UPower_monitor.monitor p_OnLowBattery (proxy daemon)

let lid_is_closed daemon =
  OBus_property.make ~monitor:UPower_monitor.monitor p_LidIsClosed (proxy daemon)

let lid_is_present daemon =
  OBus_property.make ~monitor:UPower_monitor.monitor p_LidIsPresent (proxy daemon)

let properties daemon =
  OBus_property.group ~monitor:UPower_monitor.monitor (proxy daemon) interface
