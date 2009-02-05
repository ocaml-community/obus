(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_peer

type t = {
  peer : OBus_peer.t;
  path : OBus_path.t;
} with projection

let make peer path = {
  peer = peer;
  path = path;
}

let obus_t = OBus_type.wrap_with_context <:obus_type< object_path >>
  (fun context path -> match context with
     | OBus_connection.Context(connection, msg) ->
         { peer = { connection = connection;
                    name = OBus_message.sender msg };
           path = path }
     | _ -> raise OBus_type.Cast_failure)
  path

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
  raw_introspect proxy >>= fun (ifaces, sub_nodes) ->
    return (ifaces, List.map (fun node -> { peer = proxy.peer;
                                            path = proxy.path @ [node] }) sub_nodes)
