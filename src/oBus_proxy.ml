(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(proxy)"

open Lwt
open OBus_peer
open OBus_pervasives

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type t = {
  peer : OBus_peer.t;
  path : OBus_path.t;
}

let make ~peer ~path = { peer = peer; path = path }

module Interface =
struct
  type 'proxy t = {
    name : OBus_name.interface;
    method_call : 'a 'b. OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> 'proxy -> 'a;
    signal : 'a 'cl. OBus_name.member -> ('a, 'cl) OBus_type.cl_sequence -> 'proxy -> 'a OBus_signal.t;
    property : 'a 'cl 'access. OBus_name.member -> 'access OBus_property.access -> ('a, 'cl) OBus_type.cl_single -> 'proxy -> ('a, 'access) OBus_property.t;
  }

  let name iface = iface.name
  let method_call iface member typ = iface.method_call member typ
  let signal iface member typ = iface.signal member typ
  let property iface member access = iface.property member access
end

(* +-----------------------------------------------------------------+
   | Signatures                                                      |
   +-----------------------------------------------------------------+ *)

module type Custom = sig
  type proxy
  val cast : proxy -> t
  val make : t -> proxy
end

module type S = sig
  type proxy with obus(basic)
  type broken = proxy with obus(basic)
  val make_interface : ?changed : OBus_name.member -> OBus_name.interface -> proxy Interface.t
  val peer : proxy -> OBus_peer.t
  val path : proxy -> OBus_path.t
  val connection : proxy -> OBus_connection.t
  val name : proxy -> OBus_name.bus option
  val introspect : proxy -> OBus_introspect.document Lwt.t
  val method_call : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, 'b Lwt.t, 'b) OBus_type.func -> 'a
  val method_call_no_reply : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, unit Lwt.t, unit) OBus_type.func -> 'a
  val method_call' : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_message.body ->
    ('a, _) OBus_type.cl_sequence -> 'a Lwt.t
  val dyn_method_call : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_message.body ->
    OBus_message.body Lwt.t
  val dyn_method_call_no_reply : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_message.body -> unit Lwt.t
  val connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_sequence -> 'a OBus_signal.t
  val dyn_connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member -> OBus_value.sequence OBus_signal.t
  val property : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    access : 'access OBus_property.access ->
    ?changed : OBus_name.member ->
    ('a, _) OBus_type.cl_single -> ('a, 'access) OBus_property.t
  val dyn_property : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    access : 'access OBus_property.access ->
    ?changed : OBus_name.member -> unit -> (OBus_value.single, 'access) OBus_property.t
end

(* +-----------------------------------------------------------------+
   | Custom proxy implementation                                     |
   +-----------------------------------------------------------------+ *)

module Make(Proxy : Custom) : S with type proxy = Proxy.proxy =
struct
  type proxy = Proxy.proxy

  let obus_proxy = OBus_type.map_with_context <:obus_type< object_path >>
    (fun context path ->
       let connection, message = OBus_connection.cast_context context in
       Proxy.make { peer = { connection = connection; name = OBus_message.sender message }; path = path })
    (fun proxy -> (Proxy.cast proxy).path)

  type broken = proxy

  let obus_broken = OBus_type.map_with_context <:obus_type< broken_path >>
    (fun context path ->
       let connection, message = OBus_connection.cast_context context in
       Proxy.make { peer = { connection = connection; name = OBus_message.sender message }; path = path })
    (fun proxy -> (Proxy.cast proxy).path)

  let peer proxy = (Proxy.cast proxy).peer
  let path proxy = (Proxy.cast proxy).path
  let connection proxy = (Proxy.cast proxy).peer.connection
  let name proxy = (Proxy.cast proxy).peer.name

  (* +---------------------------------------------------------------+
     | Method calls                                                  |
     +---------------------------------------------------------------+ *)

  let method_call proxy ?interface ~member typ =
    let proxy = Proxy.cast proxy in
    OBus_connection.method_call proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      typ

  let method_call_no_reply proxy ?interface ~member typ =
    let proxy = Proxy.cast proxy in
    OBus_connection.method_call_no_reply proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      typ

  let method_call' proxy ?interface ~member body typ =
    let proxy = Proxy.cast proxy in
    OBus_connection.method_call' proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      body
      typ

  let dyn_method_call proxy ?interface ~member body =
    let proxy = Proxy.cast proxy in
    OBus_connection.dyn_method_call proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      body

  let dyn_method_call_no_reply proxy ?interface ~member body =
    let proxy = Proxy.cast proxy in
    OBus_connection.dyn_method_call_no_reply proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      body

  (* +---------------------------------------------------------------+
     | Introspection                                                 |
     +---------------------------------------------------------------+ *)

  let introspect proxy =
    method_call proxy ~interface:"org.freedesktop.DBus.Introspectable" ~member:"Introspect" <:obus_func< OBus_introspect.document >>

  (* +---------------------------------------------------------------+
     | Properties                                                    |
     +---------------------------------------------------------------+ *)

  let property proxy ~interface ~member ~access ?changed typ =
    let proxy = Proxy.cast proxy in
    OBus_property.make
      ~connection:proxy.peer.connection
      ?sender:proxy.peer.name
      ~path:proxy.path
      ~interface
      ~member
      ~access
      ?changed
      typ

  let dyn_property proxy ~interface ~member ~access ?changed () =
    let proxy = Proxy.cast proxy in
    OBus_property.dyn_make
      ~connection:proxy.peer.connection
      ?sender:proxy.peer.name
      ~path:proxy.path
      ~interface
      ~member
      ~access
      ?changed
      ()

  (* +---------------------------------------------------------------+
     | Signals                                                       |
     +---------------------------------------------------------------+ *)

  let connect proxy ~interface ~member typ =
    let proxy = Proxy.cast proxy in
    OBus_signal.connect
      ~connection:proxy.peer.connection
      ?sender:proxy.peer.name
      ~path:proxy.path
      ~interface
      ~member
      typ

  let dyn_connect proxy ~interface ~member =
    let proxy = Proxy.cast proxy in
    OBus_signal.dyn_connect
      ~connection:proxy.peer.connection
      ?sender:proxy.peer.name
      ~path:proxy.path
      ~interface
      ~member
      ()

  (* +---------------------------------------------------------------+
     | Interface creation                                            |
     +---------------------------------------------------------------+ *)

  let make_interface ?changed interface = {
    Interface.name = interface;
    Interface.method_call = (fun member typ proxy -> method_call proxy ~interface ~member typ);
    Interface.signal = (fun member typ proxy -> connect proxy ~interface ~member typ);
    Interface.property = (fun member access typ proxy -> property proxy ~interface ~member ~access ?changed typ);
  }
end

(* +-----------------------------------------------------------------+
   | Implementation using native proxies                             |
   +-----------------------------------------------------------------+ *)

include Make(struct
               type proxy = t
               let cast proxy = proxy
               let make proxy = proxy
             end)

let obus_t = obus_proxy

(* +-----------------------------------------------------------------+
   | Private proxyes                                                 |
   +-----------------------------------------------------------------+ *)

module type Private = sig
  type t = private proxy
  val obus_t : t OBus_type.basic
  type broken = t
  val obus_broken : broken OBus_type.basic
  external of_proxy : proxy -> t = "%identity"
  external to_proxy : t -> proxy = "%identity"
end

module Private =
struct
  type t = proxy
  let obus_t = obus_t
  type broken = t
  let obus_broken = obus_broken
  external of_proxy : proxy -> t = "%identity"
  external to_proxy : t -> proxy = "%identity"
end
