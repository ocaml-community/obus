(*
 * policy_kit.ml
 * -------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_pervasives

exception Not_authorized of string
 with obus("org.freedesktop.PolicyKit.Error.NotAuthorized")

let op_interface = OBus_proxy.make_interface "org.freedesktop.PolicyKit.AuthenticationAgent"

OP_method ObtainAuthorization : action_id : string -> xid : uint -> pid : uint -> bool

let obtain_authorization ~action_id ?(xid=0) ~pid () =
  lwt session_bus = OBus_bus.session () in
  let proxy =
    OBus_proxy.make
      (OBus_peer.make session_bus "org.freedesktop.PolicyKit.AuthenticationAgent")
      []
  in
  obtain_authorization proxy ~action_id ~xid ~pid
