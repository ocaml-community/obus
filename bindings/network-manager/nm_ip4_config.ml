(*
 * nm_ip4_config.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

include OBus_proxy.Private

open Nm_interfaces.Org_freedesktop_NetworkManager_IP4Config

let addresses proxy =
  OBus_property.map_r
    (fun x -> List.map (List.map Int32.to_int) x)
    (OBus_property.make p_Addresses proxy)

let nameservers proxy =
  OBus_property.map_r
    (fun x -> List.map Int32.to_int x)
    (OBus_property.make p_Nameservers proxy)

let wins_servers proxy =
  OBus_property.map_r
    (fun x -> List.map Int32.to_int x)
    (OBus_property.make p_WinsServers proxy)

let domains proxy =
  OBus_property.make p_Domains proxy

let routes proxy =
  OBus_property.map_r
    (fun x -> List.map (List.map Int32.to_int) x)
    (OBus_property.make p_Routes proxy)

let properties proxy =
  OBus_property.group proxy interface
