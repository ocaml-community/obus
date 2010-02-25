(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Log = Lwt_log.Make(struct let section = "obus(proxy)" end)

open Lwt
open OBus_private
open OBus_peer
open OBus_pervasives

type t = {
  peer : OBus_peer.t;
  path : OBus_path.t;
}

let make ~peer ~path = { peer = peer; path = path }

(* +-----------------------------------------------------------------+
   | Signatures                                                      |
   +-----------------------------------------------------------------+ *)

module type Interface_name = sig
  val name : OBus_name.interface
end

module type Custom = sig
  type proxy
  val get : proxy -> t
  val make : t -> proxy
end

module type S = sig
  type proxy with obus(basic)
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
  class type ['a] signal = object
    method event : 'a React.event
    method disconnect : unit
  end
  val connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_sequence -> 'a signal
  val dyn_connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member -> OBus_value.sequence signal
  val get : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_single -> 'a Lwt.t
  val set : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_single -> 'a -> unit Lwt.t
  val dyn_get : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member -> OBus_value.single Lwt.t
  val dyn_set : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_value.single -> unit Lwt.t
  val dyn_get_all : proxy -> interface : OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
  module Make_interface(Name : Interface_name) : sig
    val op_interface : OBus_name.interface
    val op_method_call : OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> proxy -> 'a
    val op_signal : OBus_name.member -> ('a, _) OBus_type.cl_sequence -> proxy -> 'a signal
    val op_property_reader : OBus_name.member -> ('a, _) OBus_type.cl_single -> proxy -> 'a Lwt.t
    val op_property_writer : OBus_name.member -> ('a, _) OBus_type.cl_single -> proxy -> 'a -> unit Lwt.t
  end
end

(* +-----------------------------------------------------------------+
   | Custom proxy implementation                                     |
   +-----------------------------------------------------------------+ *)

module Make(Proxy : Custom) =
struct
  type proxy = Proxy.proxy

  let obus_proxy = OBus_type.map_with_context <:obus_type< object_path >>
    (fun context path ->
       let connection, message = OBus_connection.cast_context context in
       Proxy.make { peer = { connection = connection; name = OBus_message.sender message }; path = path })
    (fun proxy -> (Proxy.get proxy).path)

  let peer proxy = (Proxy.get proxy).peer
  let path proxy = (Proxy.get proxy).path
  let connection proxy = (Proxy.get proxy).peer.connection
  let name proxy = (Proxy.get proxy).peer.name

  (* +---------------------------------------------------------------+
     | Method calls                                                  |
     +---------------------------------------------------------------+ *)

  let method_call proxy ?interface ~member typ =
    let proxy = Proxy.get proxy in
    OBus_connection.method_call proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      typ

  let method_call_no_reply proxy ?interface ~member typ =
    let proxy = Proxy.get proxy in
    OBus_connection.method_call_no_reply proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      typ

  let method_call' proxy ?interface ~member body typ =
    let proxy = Proxy.get proxy in
    OBus_connection.method_call' proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      body
      typ

  let dyn_method_call proxy ?interface ~member body =
    let proxy = Proxy.get proxy in
    OBus_connection.dyn_method_call proxy.peer.connection
      ?destination:proxy.peer.name
      ~path:proxy.path
      ?interface
      ~member
      body

  let dyn_method_call_no_reply proxy ?interface ~member body =
    let proxy = Proxy.get proxy in
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

  let op_method_call member typ proxy = method_call proxy ~interface:"org.freedesktop.DBus.Properties" ~member typ

  OP_method Get as _dyn_get : string -> string -> variant
  OP_method Set as _dyn_set : string -> string -> variant -> unit
  OP_method GetAll as _dyn_get_all : string -> (string, variant) dict

  OP_method Get as dyn_get_with_context : string -> string -> variant * OBus_connection.context

  let dyn_get proxy ~interface ~member = _dyn_get proxy interface member
  let dyn_set proxy ~interface ~member value = _dyn_set proxy interface member value
  let dyn_get_all proxy ~interface = _dyn_get_all proxy interface

  let get proxy ~interface ~member typ =
    lwt value, (connection, message) = dyn_get_with_context proxy interface member in
    return (OBus_type.cast_single typ ~context:(OBus_connection.make_context (connection, message)) value)

  let set proxy ~interface ~member typ x =
    dyn_set proxy ~interface ~member (OBus_type.make_single typ x)

  (* +---------------------------------------------------------------+
     | Signals                                                       |
     +---------------------------------------------------------------+ *)

  class type ['a] signal = object
    method event : 'a React.event
    method disconnect : unit
  end

  let _connect proxy ~interface ~member ~push ~until =
    let proxy = Proxy.get proxy in
    match proxy.peer.connection#get with
      | Crashed exn ->
          fail exn

      | Running connection ->
          let make_signal_receiver sender_resolver_opt = {
            sr_sender = sender_resolver_opt;
            sr_path = proxy.path;
            sr_interface = interface;
            sr_member = member;
            sr_push = push;
          } in

          if connection.OBus_private.name = None then
            (* If the connection is a peer-to-peer connection the only
               thing to do is to locally add the receiver *)
            let node = Lwt_sequence.add_r (make_signal_receiver None) connection.signal_receivers in
            lwt () = until in
            Lwt_sequence.remove node;
            return ()

          else begin

            let match_rule = OBus_match.rule
              ~typ:`Signal
              ?sender:proxy.peer.name
              ~path:proxy.path
              ~interface
              ~member
              ()
            in

            match proxy.peer.name with
              | None ->
                  let node = Lwt_sequence.add_r (make_signal_receiver None) connection.signal_receivers in
                  lwt () = OBus_private_bus.add_match connection.packed match_rule in
                  lwt () = until in
                  Lwt_sequence.remove node;
                  OBus_private_bus.remove_match connection.packed match_rule

              | Some name ->
                  lwt resolver = OBus_resolver.make connection.packed name in
                  let node = Lwt_sequence.add_r (make_signal_receiver (Some resolver#name)) connection.signal_receivers in
                  lwt () = OBus_private_bus.add_match connection.packed match_rule in
                  lwt () = until in
                  Lwt_sequence.remove node;
                  OBus_private_bus.remove_match connection.packed match_rule <&> resolver#disable
          end

  let dyn_connect proxy ~interface ~member =
    let event, push = React.E.create () and until_waiter, until_wakener = wait () in
    ignore_result (_connect proxy ~interface ~member ~push ~until:until_waiter);
    let event = React.E.map (fun (connection, message) -> OBus_message.body message) event
    and disconnect = lazy(wakeup until_wakener ()) in
    (object
       method event = event
       method disconnect = Lazy.force disconnect
     end)

  let cast interface member typ (connection, message) =
    try
      Some(OBus_type.cast_sequence typ ~context:(OBus_connection.make_context (connection, message)) (OBus_message.body message))
    with exn ->
      ignore (
        Log.exn_f exn "failed to cast signal from %S, interface %S, member %S with signature %S to %S"
          (match OBus_message.sender message with None -> "" | Some n -> n) interface member
          (OBus_value.string_of_signature (OBus_value.type_of_sequence (OBus_message.body message)))
          (OBus_value.string_of_signature (OBus_type.type_sequence typ))
      );
      None

  let connect proxy ~interface ~member typ =
    let event, push = React.E.create () and until_waiter, until_wakener = wait () in
    ignore_result (_connect proxy ~interface ~member ~push ~until:until_waiter);
    let event = React.E.fmap (cast interface member typ) event
    and disconnect = lazy(wakeup until_wakener ()) in
    (object
       method event = event
       method disconnect = Lazy.force disconnect
     end)

  (* +---------------------------------------------------------------+
     | Interface                                                     |
     +---------------------------------------------------------------+ *)

  module Make_interface(Name : Interface_name) =
  struct
    let op_interface = Name.name
    let op_method_call member typ proxy = method_call proxy ~interface:op_interface ~member typ
    let op_signal member typ proxy = connect proxy ~interface:op_interface ~member typ
    let op_property_reader member typ proxy = get proxy ~interface:op_interface ~member typ
    let op_property_writer member typ proxy value = set proxy ~interface:op_interface ~member typ value
  end
end

include Make(struct
               type proxy = t
               let get proxy = proxy
               let make proxy = proxy
             end)

let obus_t = obus_proxy
