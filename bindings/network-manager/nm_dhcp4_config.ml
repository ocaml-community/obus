(*
 * nm_dhcp4_config.ml
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

include OBus_proxy.Private

open Nm_interfaces.Org_freedesktop_NetworkManager_DHCP4Config

let options proxy =
  OBus_property.make p_Options proxy

let properties_changed proxy =
  OBus_signal.connect s_PropertiesChanged proxy
