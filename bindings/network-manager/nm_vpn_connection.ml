(*
 * nm_vpn_connection.ml
 * --------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

include OBus_proxy.Private

open Nm_interfaces.Org_freedesktop_NetworkManager_VPN_Connection

let properties_changed proxy =
  OBus_signal.make s_PropertiesChanged proxy

let vpn_state proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:Nm_monitor.monitor p_VpnState proxy)

let banner proxy =
  OBus_property.make ~monitor:Nm_monitor.monitor p_Banner proxy

let vpn_state_changed proxy =
  OBus_signal.map
    (fun (state, reason) ->
       let state = Int32.to_int state in
       let reason = Int32.to_int reason in
       (state, reason))
    (OBus_signal.make s_VpnStateChanged proxy)

let properties proxy =
  OBus_property.group ~monitor:Nm_monitor.monitor proxy interface
