(*
 * nm_ppp.ml
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

include OBus_proxy.Private

open Nm_interfaces.Org_freedesktop_NetworkManager_PPP

let need_secrets proxy =
  OBus_method.call m_NeedSecrets proxy ()

let set_ip4_config proxy ~config =
  OBus_method.call m_SetIp4Config proxy config

let set_state proxy ~state =
  let state = Int32.of_int state in
  OBus_method.call m_SetState proxy state
