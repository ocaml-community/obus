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
  val make_interface : ?notify : OBus_property.notify_mode -> OBus_name.interface -> proxy Interface.t
  val peer : proxy -> OBus_peer.t
  val path : proxy -> OBus_path.t
  val connection : proxy -> OBus_connection.t
  val name : proxy -> OBus_name.bus option
  val introspect : proxy -> OBus_introspect.document Lwt.t
  val call : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, 'b Lwt.t, 'b) OBus_type.func -> 'a
  val call_no_reply : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, unit Lwt.t, unit) OBus_type.func -> 'a
  val dyn_call : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_message.body ->
    OBus_message.body Lwt.t
  val dyn_call_no_reply : proxy ->
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
  val raw_connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member -> OBus_message.t OBus_signal.t
  val property : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    access : 'access OBus_property.access ->
    ?notify : OBus_property.notify_mode ->
    ('a, _) OBus_type.cl_single -> ('a, 'access) OBus_property.t
  val dyn_property : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    access : 'access OBus_property.access ->
    ?notify : OBus_property.notify_mode ->
    unit -> (OBus_value.single, 'access) OBus_property.t
  val get : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_single -> 'a Lwt.t
  val set : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_single -> 'a -> unit Lwt.t
  val get_all : proxy -> interface : OBus_name.interface -> OBus_value.single Map.Make(String).t Lwt.t
  val dyn_get : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member -> OBus_value.single Lwt.t
  val dyn_set : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_value.single -> unit Lwt.t
end

(* +-----------------------------------------------------------------+
   | Custom proxy implementation                                     |
   +-----------------------------------------------------------------+ *)

module Make(Proxy : Custom) : S with type proxy = Proxy.proxy =
struct
  type proxy = Proxy.proxy

  let obus_proxy = OBus_type.map_with_context <:obus_type< object_path >>
    (fun (connection, message) path ->
       Proxy.make { peer = { connection = connection;
                             name = OBus_message.sender message };
                    path = path })
    (fun proxy -> (Proxy.cast proxy).path)

  type broken = proxy

  let obus_broken = OBus_type.map_with_context <:obus_type< broken_path >>
    (fun (connection, message) path ->
       Proxy.make { peer = { connection = connection;
                             name = OBus_message.sender message };
                    path = path })
    (fun proxy -> (Proxy.cast proxy).path)

  let peer proxy = (Proxy.cast proxy).peer
  let path proxy = (Proxy.cast proxy).path
  let connection proxy = (Proxy.cast proxy).peer.connection
  let name proxy = (Proxy.cast proxy).peer.name

  (* +---------------------------------------------------------------+
     | Method calls                                                  |
     +---------------------------------------------------------------+ *)

  let call proxy ?interface ~member typ =
    let proxy = Proxy.cast proxy in
    OBus_method.call
      ~connection:proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      typ

  let call_no_reply proxy ?interface ~member typ =
    let proxy = Proxy.cast proxy in
    OBus_method.call_no_reply
      ~connection:proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      typ

  let dyn_call proxy ?interface ~member body =
    let proxy = Proxy.cast proxy in
    OBus_method.dyn_call
      ~connection:proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      body

  let dyn_call_no_reply proxy ?interface ~member body =
    let proxy = Proxy.cast proxy in
    OBus_method.dyn_call_no_reply
      ~connection:proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      body

  (* +---------------------------------------------------------------+
     | Introspection                                                 |
     +---------------------------------------------------------------+ *)

  let introspect proxy =
    call proxy ~interface:"org.freedesktop.DBus.Introspectable" ~member:"Introspect" <:obus_func< OBus_introspect.document >>

  (* +---------------------------------------------------------------+
     | Properties                                                    |
     +---------------------------------------------------------------+ *)

  let property proxy ~interface ~member ~access ?notify typ =
    let proxy = Proxy.cast proxy in
    OBus_property.make
      ~connection:proxy.peer.connection
      ?owner:proxy.peer.name
      ~path:proxy.path
      ~interface
      ~member
      ~access
      ?notify
      typ

  let dyn_property proxy ~interface ~member ~access ?notify () =
    let proxy = Proxy.cast proxy in
    OBus_property.dyn_make
      ~connection:proxy.peer.connection
      ?owner:proxy.peer.name
      ~path:proxy.path
      ~interface
      ~member
      ~access
      ?notify
      ()

  let get proxy ~interface ~member typ =
    OBus_property.get (property proxy ~interface ~member ~access:OBus_property.readable typ)

  let set proxy ~interface ~member typ value =
    OBus_property.set (property proxy ~interface ~member ~access:OBus_property.writable typ) value

  let dyn_get proxy ~interface ~member =
    OBus_property.get (dyn_property proxy ~interface ~member ~access:OBus_property.readable ())

  let dyn_set proxy ~interface ~member value =
    OBus_property.set (dyn_property proxy ~interface ~member ~access:OBus_property.writable ()) value

  let get_all proxy ~interface =
    let proxy = Proxy.cast proxy in
    OBus_property.get_all
      ~connection:proxy.peer.connection
      ?owner:proxy.peer.name
      ~path:proxy.path
      ~interface
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

  let raw_connect proxy ~interface ~member =
    let proxy = Proxy.cast proxy in
    OBus_signal.raw_connect
      ~connection:proxy.peer.connection
      ?sender:proxy.peer.name
      ~path:proxy.path
      ~interface
      ~member
      ()

  (* +---------------------------------------------------------------+
     | Interface creation                                            |
     +---------------------------------------------------------------+ *)

  let make_interface ?notify interface = {
    Interface.name = interface;
    Interface.method_call = (fun member typ proxy -> call proxy ~interface ~member typ);
    Interface.signal = (fun member typ proxy -> connect proxy ~interface ~member typ);
    Interface.property = (fun member access typ proxy -> property proxy ~interface ~member ~access ?notify typ);
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
