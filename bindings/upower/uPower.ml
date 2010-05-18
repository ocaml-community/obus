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

exception General_error
let () = OBus_error.register ~name:"org.freedesktop.UPower.GeneralError" ~exn:General_error

let daemon () =
  lwt bus = OBus_bus.system () in
  return (OBus_peer.make bus "org.freedesktop.UPower")

open UPower_interfaces.Org_freedesktop_UPower

let proxy daemon = OBus_proxy.make daemon ["org"; "freedesktop"; "UPower"]

let enumerate_devices daemon =
  lwt (context, devices) = OBus_method.call_with_context m_EnumerateDevices (proxy daemon) () in
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
    (OBus_signal.connect s_DeviceAdded (proxy daemon))

let device_removed daemon =
  OBus_signal.map_with_context
    (fun context device ->
       UPower_device.of_proxy (OBus_proxy.make (OBus_context.sender context) (OBus_path.of_string device)))
    (OBus_signal.connect s_DeviceRemoved (proxy daemon))

let device_changed daemon =
  OBus_signal.map_with_context
    (fun context device ->
       UPower_device.of_proxy (OBus_proxy.make (OBus_context.sender context) (OBus_path.of_string device)))
    (OBus_signal.connect s_DeviceChanged (proxy daemon))

let changed daemon =
  OBus_signal.connect s_Changed (proxy daemon)

let sleeping daemon =
  OBus_signal.connect s_Sleeping (proxy daemon)

let resuming daemon =
  OBus_signal.connect s_Resuming (proxy daemon)

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
  OBus_property.make p_DaemonVersion (proxy daemon)

let can_suspend daemon =
  OBus_property.make p_CanSuspend (proxy daemon)

let can_hibernate daemon =
  OBus_property.make p_CanHibernate (proxy daemon)

let on_battery daemon =
  OBus_property.make p_OnBattery (proxy daemon)

let on_low_battery daemon =
  OBus_property.make p_OnLowBattery (proxy daemon)

let lid_is_closed daemon =
  OBus_property.make p_LidIsClosed (proxy daemon)

let lid_is_present daemon =
  OBus_property.make p_LidIsPresent (proxy daemon)

type properties = {
  lid_is_present : bool;
  lid_is_closed : bool;
  on_low_battery : bool;
  on_battery : bool;
  can_hibernate : bool;
  can_suspend : bool;
  daemon_version : string;
}

let properties daemon =
  OBus_property.map_r_with_context
    (fun context properties ->
       let find f = OBus_property.find (f daemon) context properties in
       {
         lid_is_present = find lid_is_present;
         lid_is_closed = find lid_is_closed;
         on_low_battery = find on_low_battery;
         on_battery = find on_battery;
         can_hibernate = find can_hibernate;
         can_suspend = find can_suspend;
         daemon_version = find daemon_version;
       })
    (OBus_property.make_group (proxy daemon) interface)
