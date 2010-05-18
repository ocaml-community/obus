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

type properties = {
  addresses : int list list;
  nameservers : int list;
  wins_servers : int list;
  domains : string list;
  routes : int list list;
}

let properties proxy =
  OBus_property.map_r_with_context
    (fun context properties ->
       let find f = OBus_property.find (f proxy) context properties in
       {
         addresses = find addresses;
         nameservers = find nameservers;
         wins_servers = find wins_servers;
         domains = find domains;
         routes = find routes;
       })
    (OBus_property.make_group proxy interface)
