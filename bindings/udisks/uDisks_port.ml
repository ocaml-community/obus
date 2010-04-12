(*
 * uDisks_port.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

include OBus_proxy.Private

open UDisks_interfaces.Org_freedesktop_UDisks_Port

let notify_mode = OBus_property.notify_global "Changed"

let changed proxy =
  OBus_signal.connect s_Changed proxy

let native_path proxy =
  OBus_property.make p_NativePath ~notify_mode proxy

let adapter proxy =
  OBus_property.map_r_with_context
    (fun context x -> UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) x))
    (OBus_property.make p_Adapter ~notify_mode proxy)

let parent proxy =
  OBus_property.map_r_with_context
    (fun context x -> UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) x))
    (OBus_property.make p_Parent ~notify_mode proxy)

let number proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_Number ~notify_mode proxy)

let connector_type proxy =
  OBus_property.make p_ConnectorType ~notify_mode proxy
