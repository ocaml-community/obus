(*
 * nm_vpn_plugin.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

include OBus_proxy.Private

open Nm_interfaces.Org_freedesktop_NetworkManager_VPN_Plugin

let connect proxy ~connection = OBus_method.call m_Connect proxy connection

let need_secrets proxy ~settings = OBus_method.call m_NeedSecrets proxy settings

let disconnect proxy = OBus_method.call m_Disconnect proxy ()

let set_ip4_config proxy ~config = OBus_method.call m_SetIp4Config proxy config

let set_failure proxy ~reason = OBus_method.call m_SetFailure proxy reason

let state proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:Nm_monitor.monitor p_State proxy)

let state_changed proxy =
  OBus_signal.map
    (fun state ->
      let state = Int32.to_int state in
      state)
    (OBus_signal.make s_StateChanged proxy)

let ip4_config proxy = OBus_signal.make s_Ip4Config proxy

let login_banner proxy = OBus_signal.make s_LoginBanner proxy

let failure proxy =
  OBus_signal.map
    (fun reason ->
      let reason = Int32.to_int reason in
      reason)
    (OBus_signal.make s_Failure proxy)
