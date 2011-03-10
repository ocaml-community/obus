(*
 * uDisks_expander.ml
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

include OBus_proxy.Private

open UDisks_interfaces.Org_freedesktop_UDisks_Expander

let changed proxy =
  OBus_signal.make s_Changed proxy

let native_path proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_NativePath proxy

let vendor proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_Vendor proxy

let model proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_Model proxy

let revision proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_Revision proxy

let num_ports proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_NumPorts proxy)

let upstream_ports proxy =
  OBus_property.map_r_with_context
    (fun context x -> List.map (fun path -> UDisks_port.of_proxy ( OBus_proxy.make (OBus_context.sender context) path)) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_UpstreamPorts proxy)

let adapter proxy =
  OBus_property.map_r_with_context
    (fun context x -> UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) x))
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_Adapter proxy)

let properties proxy =
  OBus_property.group ~monitor:UDisks_monitor.monitor proxy interface
