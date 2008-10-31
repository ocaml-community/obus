(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type
open OBus_peer
open OBus_connection

type t = {
  peer : OBus_peer.t;
  path : OBus_path.t;
}

let make peer path = {
  peer = peer;
  path = path;
}

let peer p = p.peer
let path p = p.path

let tt = OBus_type.wrap_basic_ctx OBus_type.tobject_path
  (fun context path -> match context with
     | Context(connection, msg) ->
         { peer = { connection = connection;
                    name = OBus_message.sender msg };
           path = path }
     | _ -> raise Cast_failure)
  path

let call proxy ?interface ~member typ =
  method_call proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    typ

let call_no_reply proxy ?interface ~member typ =
  method_call_no_reply proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    typ

let call' proxy ?interface ~member body typ =
  method_call' proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    body
    typ

let dcall proxy ?interface ~member body =
  dmethod_call proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    body

let dcall_no_reply proxy ?interface ~member body =
  dmethod_call_no_reply proxy.peer.connection
    ?destination:proxy.peer.name
    ~path:proxy.path
    ?interface
    ~member
    body

type introspection = OBus_introspect.interface list * t list

let raw_introspect proxy =
  call proxy ~interface:"org.freedesktop.DBus.Introspectable" ~member:"Introspect" << OBus_introspect.document >>

let introspect proxy =
  raw_introspect proxy >>= fun (ifaces, sub_nodes) ->
    return (ifaces, List.map (fun node -> { peer = proxy.peer;
                                            path = proxy.path @ [node] }) sub_nodes)
