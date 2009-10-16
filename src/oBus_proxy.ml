(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_peer
open OBus_type.Perv

type t = {
  peer : OBus_peer.t;
  path : OBus_path.t;
} with projection

let make peer path = {
  peer = peer;
  path = path;
}

let connection proxy = proxy.peer.connection
let name proxy = proxy.peer.name

let obus_t = OBus_type.map_with_context <:obus_type< object_path >>
  (fun context path -> match context with
     | OBus_connection.Context(connection, message) ->
         { peer = { connection = connection;
                    name = OBus_message.sender message };
           path = path }
     | _ ->
         raise OBus_type.Cast_failure)
  (fun proxy -> proxy.path)

let method_call proxy ?interface ~member typ =
  OBus_connection.method_call proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    typ

let method_call_no_reply proxy ?interface ~member typ =
  OBus_connection.method_call_no_reply proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    typ

let method_call' proxy ?interface ~member body typ =
  OBus_connection.method_call' proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    body
    typ

let dyn_method_call proxy ?interface ~member body =
  OBus_connection.dyn_method_call proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    body

let dyn_method_call_no_reply proxy ?interface ~member body =
  OBus_connection.dyn_method_call_no_reply proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    body

type introspection = OBus_introspect.interface list * t list

let raw_introspect proxy =
  method_call proxy ~interface:"org.freedesktop.DBus.Introspectable" ~member:"Introspect" <:obus_func< OBus_introspect.document >>

let introspect proxy =
  lwt (ifaces, sub_nodes) = raw_introspect proxy in
  return (ifaces, List.map (fun node -> { peer = proxy.peer;
                                          path = proxy.path @ [node] }) sub_nodes)

let children proxy = introspect proxy >|= snd
