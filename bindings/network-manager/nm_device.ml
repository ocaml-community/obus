(*
 * nm_device.ml
 * ------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

open Lwt

let section = Lwt_log.Section.make "network-manager(device)"

include OBus_proxy.Private

type state =
    [ `Unknown
    | `Unmanaged
    | `Unavailable
    | `Disconnected
    | `Prepare
    | `Config
    | `Need_auth
    | `Ip_config
    | `Activated
    | `Failed ]

type state_reason =
    [ `Unknown
    | `None
    | `Now_managed
    | `Now_unmanaged
    | `Config_failed
    | `Config_unavailable
    | `Config_expired
    | `No_secrets
    | `Supplicant_disconnect
    | `Supplicant_config_failed
    | `Supplicant_failed
    | `Supplicant_timeout
    | `Ppp_start_failed
    | `Ppp_disconnect
    | `Ppp_failed
    | `Dhcp_start_failed
    | `Dhcp_error
    | `Dhcp_failed
    | `Shared_start_failed
    | `Shared_failed
    | `Autoip_start_failed
    | `Autoip_error
    | `Autoip_failed
    | `Modem_busy
    | `Modem_no_dial_tone
    | `Modem_no_carrier
    | `Modem_dial_timeout
    | `Modem_dial_failed
    | `Modem_init_failed
    | `Gsm_apn_failed
    | `Gsm_registration_not_searching
    | `Gsm_registration_denied
    | `Gsm_registration_timeout
    | `Gsm_registration_failed
    | `Gsm_pin_check_failed
    | `Firmware_missing
    | `Removed
    | `Sleeping
    | `Connection_removed
    | `User_requested
    | `Carrier
    | `Connection_assumed
    | `Supplicant_available ]

type typ =
  [ `Unknown
  | `Ethernet
  | `Wifi
  | `Gsm
  | `Cdma ]

type capability =
  [ `Nm_supported
  | `Carrier_detect ]

let state_of_int32 = function
  | 0l -> `Unknown
  | 1l -> `Unmanaged
  | 2l -> `Unavailable
  | 3l -> `Disconnected
  | 4l -> `Prepare
  | 5l -> `Config
  | 6l -> `Need_auth
  | 7l -> `Ip_config
  | 8l -> `Activated
  | 9l -> `Failed
  | st ->
      ignore (Lwt_log.warning_f ~section "Nm_device.state_of_int32: unknown device_state: %ld" st);
      `Unknown

let state_reason_of_int32 = function
  | 0l -> `Unknown
  | 1l -> `None
  | 2l -> `Now_managed
  | 3l -> `Now_unmanaged
  | 4l -> `Config_failed
  | 5l -> `Config_unavailable
  | 6l -> `Config_expired
  | 7l -> `No_secrets
  | 8l -> `Supplicant_disconnect
  | 9l -> `Supplicant_config_failed
  | 10l -> `Supplicant_failed
  | 11l -> `Supplicant_timeout
  | 12l -> `Ppp_start_failed
  | 13l -> `Ppp_disconnect
  | 14l -> `Ppp_failed
  | 15l -> `Dhcp_start_failed
  | 16l -> `Dhcp_error
  | 17l -> `Dhcp_failed
  | 18l -> `Shared_start_failed
  | 19l -> `Shared_failed
  | 20l -> `Autoip_start_failed
  | 21l -> `Autoip_error
  | 22l -> `Autoip_failed
  | 23l -> `Modem_busy
  | 24l -> `Modem_no_dial_tone
  | 25l -> `Modem_no_carrier
  | 26l -> `Modem_dial_timeout
  | 27l -> `Modem_dial_failed
  | 28l -> `Modem_init_failed
  | 29l -> `Gsm_apn_failed
  | 30l -> `Gsm_registration_not_searching
  | 31l -> `Gsm_registration_denied
  | 32l -> `Gsm_registration_timeout
  | 33l -> `Gsm_registration_failed
  | 34l -> `Gsm_pin_check_failed
  | 35l -> `Firmware_missing
  | 36l -> `Removed
  | 37l -> `Sleeping
  | 38l -> `Connection_removed
  | 39l -> `User_requested
  | 40l -> `Carrier
  | 41l -> `Connection_assumed
  | 42l -> `Supplicant_available
  | n ->
      ignore (Lwt_log.warning_f ~section "Nm_device.state_reason_of_int32: unknown state_reason: %ld" n);
      `Unknown

open Nm_interfaces.Org_freedesktop_NetworkManager_Device

let udi proxy =
  OBus_property.make ~monitor:Nm_monitor.monitor p_Udi proxy

let interface proxy =
  OBus_property.make ~monitor:Nm_monitor.monitor p_Interface proxy

let driver proxy =
  OBus_property.make ~monitor:Nm_monitor.monitor p_Driver proxy

let capabilities proxy =
  OBus_property.map_r
    (fun n ->
       let n = Int32.to_int n in
       let l = [] in
       let l = if n land 0x1 <> 0 then `Nm_supported :: l else l in
       let l = if n land 0x2 <> 0 then `Carrier_detect :: l else l in
       l)
    (OBus_property.make ~monitor:Nm_monitor.monitor p_Capabilities proxy)

let ip4_address proxy =
  OBus_property.make ~monitor:Nm_monitor.monitor p_Ip4Address proxy

let state proxy =
  OBus_property.map_r
    state_of_int32
    (OBus_property.make ~monitor:Nm_monitor.monitor p_State proxy)

let ip4_config proxy =
  OBus_property.map_r_with_context
    (fun context path ->
       Nm_ip4_config.of_proxy
         (OBus_proxy.make (OBus_context.sender context) path))
    (OBus_property.make ~monitor:Nm_monitor.monitor p_Ip4Config proxy)

let dhcp4_config proxy =
  OBus_property.map_r_with_context
    (fun context path ->
       Nm_dhcp4_config.of_proxy
         (OBus_proxy.make (OBus_context.sender context) path))
    (OBus_property.make ~monitor:Nm_monitor.monitor p_Dhcp4Config proxy)

let ip6_config proxy =
  OBus_property.map_r_with_context
    (fun context path ->
       Nm_ip6_config.of_proxy
         (OBus_proxy.make (OBus_context.sender context) path))
    (OBus_property.make ~monitor:Nm_monitor.monitor p_Ip6Config proxy)

let managed proxy =
  OBus_property.make ~monitor:Nm_monitor.monitor p_Managed proxy

let device_type proxy =
  OBus_property.map_r
    (function
       | 0l -> `Unknown
       | 1l -> `Ethernet
       | 2l -> `Wifi
       | 3l -> `Gsm
       | 4l -> `Cdma
       | n ->
           ignore (Lwt_log.warning_f ~section "device_type_of_int: unknown type: %ld" n);
           `Unknown)
    (OBus_property.make ~monitor:Nm_monitor.monitor p_DeviceType proxy)

let disconnect proxy =
  OBus_method.call m_Disconnect proxy ()

let state_changed proxy =
  OBus_signal.map
    (fun (new_state, old_state, reason) ->
       (state_of_int32 new_state,
        state_of_int32 old_state,
        state_reason_of_int32 reason))
    (OBus_signal.make s_StateChanged proxy)

let properties proxy =
  OBus_property.group ~monitor:Nm_monitor.monitor proxy Nm_interfaces.Org_freedesktop_NetworkManager_Device.interface

module Bluetooth =
struct
  open Nm_interfaces.Org_freedesktop_NetworkManager_Device_Bluetooth

  let hw_address proxy =
    OBus_property.make ~monitor:Nm_monitor.monitor p_HwAddress proxy

  let name proxy =
    OBus_property.make ~monitor:Nm_monitor.monitor p_Name proxy

  let bt_capabilities proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make ~monitor:Nm_monitor.monitor p_BtCapabilities proxy)

  let properties_changed proxy =
    OBus_signal.make s_PropertiesChanged proxy

  let properties proxy =
    OBus_property.group ~monitor:Nm_monitor.monitor proxy interface
end

module Cdma =
struct
  open Nm_interfaces.Org_freedesktop_NetworkManager_Device_Cdma
  let properties_changed proxy =
    OBus_signal.make s_PropertiesChanged proxy
end

module Gsm =
struct
  open Nm_interfaces.Org_freedesktop_NetworkManager_Device_Gsm
  let properties_changed proxy =
    OBus_signal.make s_PropertiesChanged proxy
end

module Olpc_mesh =
struct
  open Nm_interfaces.Org_freedesktop_NetworkManager_Device_OlpcMesh

  let hw_address proxy =
    OBus_property.make ~monitor:Nm_monitor.monitor p_HwAddress proxy

  let companion proxy =
    OBus_property.map_r_with_context
      (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
      (OBus_property.make ~monitor:Nm_monitor.monitor p_Companion proxy)

  let active_channel proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make ~monitor:Nm_monitor.monitor p_ActiveChannel proxy)

  let properties_changed proxy =
    OBus_signal.make s_PropertiesChanged proxy

  let properties proxy =
    OBus_property.group ~monitor:Nm_monitor.monitor proxy interface
end

module Serial =
struct
  open Nm_interfaces.Org_freedesktop_NetworkManager_Device_Serial

  let ppp_stats proxy =
    OBus_signal.map
      (fun (in_bytes, out_bytes) ->
         let in_bytes = Int32.to_int in_bytes in
         let out_bytes = Int32.to_int out_bytes in
         (in_bytes, out_bytes))
      (OBus_signal.make s_PppStats proxy)
end

module Wired =
struct
  open Nm_interfaces.Org_freedesktop_NetworkManager_Device_Wired

  let hw_address proxy =
    OBus_property.make ~monitor:Nm_monitor.monitor p_HwAddress proxy

  let speed proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make ~monitor:Nm_monitor.monitor p_Speed proxy)

  let carrier proxy =
    OBus_property.make ~monitor:Nm_monitor.monitor p_Carrier proxy

  let properties_changed proxy =
    OBus_signal.make s_PropertiesChanged proxy

  let properties proxy =
    OBus_property.group ~monitor:Nm_monitor.monitor proxy interface
end

module Wireless =
struct
  open Nm_interfaces.Org_freedesktop_NetworkManager_Device_Wireless

  type wireless_capability =
      [ `Cipher_wep40
      | `Cipher_wep104
      | `Cipher_tkip
      | `Cipher_ccmp
      | `Wpa
      | `Rsn ]

  type wifi_mode =
      [ `Unknown
      | `Adhoc
      | `Infra ]

  let get_access_points proxy =
    let%lwt (context, access_points) = OBus_method.call_with_context m_GetAccessPoints proxy () in
    return (
      List.map
        (fun path ->
           Nm_access_point.of_proxy
             (OBus_proxy.make (OBus_context.sender context) path))
        access_points
    )

  let hw_address proxy =
    OBus_property.make ~monitor:Nm_monitor.monitor p_HwAddress proxy

  let mode proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make ~monitor:Nm_monitor.monitor p_Mode proxy)

  let bitrate proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make ~monitor:Nm_monitor.monitor p_Bitrate proxy)

  let active_access_point proxy =
    OBus_property.map_r_with_context
      (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
      (OBus_property.make ~monitor:Nm_monitor.monitor p_ActiveAccessPoint proxy)

  let wireless_capabilities proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make ~monitor:Nm_monitor.monitor p_WirelessCapabilities proxy)

  let properties_changed proxy =
    OBus_signal.make s_PropertiesChanged proxy

  let access_point_added proxy =
    OBus_signal.map_with_context
      (fun context access_point ->
         Nm_access_point.of_proxy
           (OBus_proxy.make (OBus_context.sender context) access_point))
      (OBus_signal.make s_AccessPointAdded proxy)

  let access_point_removed proxy =
    OBus_signal.map_with_context
      (fun context access_point ->
         Nm_access_point.of_proxy
           (OBus_proxy.make (OBus_context.sender context) access_point))
      (OBus_signal.make s_AccessPointRemoved proxy)

  let properties proxy =
    OBus_property.group ~monitor:Nm_monitor.monitor proxy interface
end
