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

let changed proxy =
  OBus_signal.connect s_Changed proxy

let native_path proxy =
  OBus_property.make p_NativePath proxy

let adapter proxy =
  OBus_property.map_r_with_context
    (fun context x -> UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) x))
    (OBus_property.make p_Adapter proxy)

let parent proxy =
  OBus_property.map_r_with_context
    (fun context x -> UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) x))
    (OBus_property.make p_Parent proxy)

let number proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_Number proxy)

let connector_type proxy =
  OBus_property.make p_ConnectorType proxy

type properties = {
  connector_type : string;
  number : int;
  parent : UDisks_adapter.t;
  adapter : UDisks_adapter.t;
  native_path : string;
}

let properties proxy =
  OBus_property.map_r_with_context
    (fun context properties ->
       let find f = OBus_property.find (f proxy) context properties in
       {
         connector_type = find connector_type;
         number = find number;
         parent = find parent;
         adapter = find adapter;
         native_path = find native_path;
       })
    (OBus_property.make_group proxy interface)
