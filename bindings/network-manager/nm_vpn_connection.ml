(*
 * nm_vpn_connection.ml
 * --------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

include OBus_proxy.Private

open Nm_interfaces.Org_freedesktop_NetworkManager_VPN_Connection

let notify_mode = OBus_property.notify_update "PropertiesChanged"

let properties_changed proxy =
  OBus_signal.connect s_PropertiesChanged proxy

let vpn_state proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_VpnState ~notify_mode proxy)

let banner proxy =
  OBus_property.make p_Banner ~notify_mode proxy

let vpn_state_changed proxy =
  OBus_signal.map
    (fun (state, reason) ->
       let state = Int32.to_int state in
       let reason = Int32.to_int reason in
       (state, reason))
    (OBus_signal.connect s_VpnStateChanged proxy)

type properties = {
  vpn_state : int;
  banner : string;
}

let properties proxy =
  OBus_property.map_r_with_context
    (fun context properties ->
       let find f = OBus_property.find (f proxy) context properties in
       {
         vpn_state = find vpn_state;
         banner = find banner;
       })
    (OBus_property.make_group proxy ~notify_mode interface)
