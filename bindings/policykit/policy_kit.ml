(*
 * policy_kit.ml
 * -------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let not_authorized = "org.freedesktop.PolicyKit.Error.NotAuthorized"

open Policy_kit_interfaces.Org_freedesktop_PolicyKit_AuthenticationAgent

let obtain_authorization ~action_id ?(xid=0) ~pid () =
  let%lwt session_bus = OBus_bus.session () in
  let proxy =
    OBus_proxy.make
      (OBus_peer.make session_bus "org.freedesktop.PolicyKit.AuthenticationAgent")
      []
  in
  OBus_method.call m_ObtainAuthorization proxy (action_id, Int32.of_int xid, Int32.of_int pid)
