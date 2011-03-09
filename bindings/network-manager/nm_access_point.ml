(*
 * nm_access_point.ml
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

open Lwt

include OBus_proxy.Private

open Nm_interfaces.Org_freedesktop_NetworkManager_AccessPoint

type ap_flag =
    [ `Privacy ]

let flags proxy =
  OBus_property.map_r
    (fun n -> if (Int32.to_int n) land 0x01 <> 0 then [`Privacy] else [])
    (OBus_property.make ~monitor:Nm_monitor.monitor p_Flags proxy)

type ap_security_flag =
    [ `Pair_wep40
    | `Pair_wep104
    | `Pair_tkip
    | `Pair_ccmp
    | `Group_wep40
    | `Group_wep104
    | `Group_tkip
    | `Group_ccmp
    | `Key_mgmt_psk
    | `Key_mgmt_802_1x ]

let ap_security_flags_of_int32 n =
  let n = Int32.to_int n in
  let add l bit_mask flag =
    if n land bit_mask <> 0 then
      flag :: l
    else
      l
  in
  let l = [] in
  let l = add l 0x001 `Pair_wep40 in
  let l = add l 0x002 `Pair_wep104 in
  let l = add l 0x004 `Pair_tkip in
  let l = add l 0x008 `Pair_ccmp in
  let l = add l 0x010 `Group_wep40 in
  let l = add l 0x020 `Group_wep104 in
  let l = add l 0x040 `Group_tkip in
  let l = add l 0x080 `Group_ccmp in
  let l = add l 0x100 `Key_mgmt_psk in
  let l = add l 0x200 `Key_mgmt_802_1x in
  l

let wpa_flags proxy =
  OBus_property.map_r
    ap_security_flags_of_int32
    (OBus_property.make ~monitor:Nm_monitor.monitor p_WpaFlags proxy)

let rsn_flags proxy =
  OBus_property.map_r
    ap_security_flags_of_int32
    (OBus_property.make ~monitor:Nm_monitor.monitor p_RsnFlags proxy)

let ssid proxy =
  OBus_property.make ~monitor:Nm_monitor.monitor p_Ssid proxy

let frequency proxy =
  OBus_property.map_r
    Int32.to_int
    (OBus_property.make ~monitor:Nm_monitor.monitor p_Frequency proxy)

let hw_address proxy =
  OBus_property.make ~monitor:Nm_monitor.monitor p_HwAddress proxy

let mode proxy =
  OBus_property.map_r
    Int32.to_int
    (OBus_property.make ~monitor:Nm_monitor.monitor p_Mode proxy)

let max_bitrate proxy =
  OBus_property.map_r
    Int32.to_int
    (OBus_property.make ~monitor:Nm_monitor.monitor p_MaxBitrate proxy)

let strength proxy =
  OBus_property.map_r
    int_of_char
    (OBus_property.make ~monitor:Nm_monitor.monitor p_Strength proxy)

let properties_changed proxy =
  OBus_signal.make s_PropertiesChanged proxy

let properties proxy =
  OBus_property.group ~monitor:Nm_monitor.monitor proxy interface
