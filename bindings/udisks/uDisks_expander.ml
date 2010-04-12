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

let notify_mode = OBus_property.notify_global "Changed"

let changed proxy =
  OBus_signal.connect s_Changed proxy

let native_path proxy =
  OBus_property.make p_NativePath ~notify_mode proxy

let vendor proxy =
  OBus_property.make p_Vendor ~notify_mode proxy

let model proxy =
  OBus_property.make p_Model ~notify_mode proxy

let revision proxy =
  OBus_property.make p_Revision ~notify_mode proxy

let num_ports proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_NumPorts ~notify_mode proxy)

let upstream_ports proxy =
  OBus_property.map_r_with_context
    (fun context x -> List.map (fun path -> UDisks_port.of_proxy ( OBus_proxy.make (OBus_context.sender context) path)) x)
    (OBus_property.make p_UpstreamPorts ~notify_mode proxy)

let adapter proxy =
  OBus_property.map_r_with_context
    (fun context x -> UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) x))
    (OBus_property.make p_Adapter ~notify_mode proxy)
