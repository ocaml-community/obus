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

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type t = {
  peer : OBus_peer.t;
  path : OBus_path.t;
}

let make ~peer ~path = { peer = peer; path = path }

class type ['a] signal = object
  method event : 'a React.event
  method set_filters : (int * OBus_match.argument_filter) list -> unit Lwt.t
  method disconnect : unit Lwt.t
end

module Interface =
struct
  type 'proxy t = {
    name : OBus_name.interface;
    method_call : 'a 'b. OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> 'proxy -> 'a;
    signal : 'a 'cl. OBus_name.member -> ('a, 'cl) OBus_type.cl_sequence -> 'proxy -> 'a signal;
    property_reader : 'a 'cl. OBus_name.member -> ('a, 'cl) OBus_type.cl_single -> 'proxy -> 'a Lwt.t;
    property_writer : 'a 'cl. OBus_name.member -> ('a, 'cl) OBus_type.cl_single -> 'proxy -> 'a -> unit Lwt.t;
  }

  let name iface = iface.name
  let method_call iface = iface.method_call
  let signal iface = iface.signal
  let property_reader iface = iface.property_reader
  let property_writer iface = iface.property_writer
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
  val make_interface : OBus_name.interface -> proxy Interface.t
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

  let interface = "org.freedesktop.DBus.Properties"

  let dyn_get proxy = method_call proxy ~interface ~member:"Get" <:obus_func< string -> string -> variant >>
  let dyn_set proxy = method_call proxy ~interface ~member:"Set" <:obus_func< string -> string -> variant -> unit >>
  let dyn_get_all proxy = method_call proxy ~interface ~member:"GetAll" <:obus_func< string -> (string, variant) dict >>
  let dyn_get_with_context proxy = method_call proxy ~interface ~member:"Get" <:obus_func< string -> string -> variant * OBus_connection.context >>

  let dyn_get proxy ~interface ~member = dyn_get proxy interface member
  let dyn_set proxy ~interface ~member value = dyn_set proxy interface member value
  let dyn_get_all proxy ~interface = dyn_get_all proxy interface

  let get proxy ~interface ~member typ =
    lwt value, (connection, message) = dyn_get_with_context proxy interface member in
    return (OBus_type.cast_single typ ~context:(OBus_connection.make_context (connection, message)) value)

  let set proxy ~interface ~member typ x =
    dyn_set proxy ~interface ~member (OBus_type.make_single typ x)

  (* +---------------------------------------------------------------+
     | Signals                                                       |
     +---------------------------------------------------------------+ *)

  type signal_action =
    | Set_filters of unit Lwt.u * (int * OBus_match.argument_filter) list
    | Done of unit Lwt.u

  let _connect proxy ~interface ~member ~push ~commands =
    let proxy = Proxy.cast proxy in
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
            Lwt_stream.find_map
              (function
                 | Done wakener ->
                     Some wakener
                 | Set_filters(wakener, _) ->
                     wakeup wakener ();
                     None)
              commands >>= function
                | Some wakener ->
                    Lwt_sequence.remove node;
                    wakeup wakener ();
                    return ()
                | None ->
                    Lwt_sequence.remove node;
                    return ()

          else begin

            let make_match_rule arguments = OBus_match.rule
              ~typ:`Signal
              ?sender:proxy.peer.name
              ~path:proxy.path
              ~interface
              ~member
              ~arguments
              ()
            in

            let rec loop commands match_rule =
              Lwt_stream.last_new commands >>= function
                | Done wakener ->
                    return (wakener, Some match_rule)
                | Set_filters(wakener, filters) ->
                    let new_match_rule = make_match_rule filters in
                    if new_match_rule <> match_rule then begin
                      let t1 = OBus_private_bus.add_match connection.packed new_match_rule in
                      let t2 = OBus_private_bus.remove_match connection.packed match_rule in
                      lwt () = t1 and () = t2 in
                      wakeup wakener ();
                      loop commands new_match_rule
                    end else begin
                      wakeup wakener ();
                      loop commands new_match_rule
                    end

            and init commands =
              Lwt_stream.last_new commands >>= function
                | Done wakener ->
                    return (wakener, None)
                | Set_filters(wakener, filters) ->
                    let match_rule = make_match_rule filters in
                    lwt () = OBus_private_bus.add_match connection.packed match_rule in
                    wakeup wakener ();
                    loop commands match_rule
            in

            (* Yield the first time to let the user add argument
               filters: *)
            lwt () = pause () in
            match proxy.peer.name with
              | None ->
                  let node = Lwt_sequence.add_r (make_signal_receiver None) connection.signal_receivers in
                  lwt wakener, match_rule = init commands in
                  Lwt_sequence.remove node;
                  lwt () =
                    match match_rule with
                      | Some match_rule -> OBus_private_bus.remove_match connection.packed match_rule
                      | None -> return ()
                  in
                  wakeup wakener ();
                  return ()

              | Some name ->
                  lwt resolver = OBus_resolver.make connection.packed name in
                  let node = Lwt_sequence.add_r (make_signal_receiver (Some resolver#name)) connection.signal_receivers in
                  lwt wakener, match_rule = init commands in
                  Lwt_sequence.remove node;
                  lwt () =
                    match match_rule with
                      | Some match_rule -> OBus_private_bus.remove_match connection.packed match_rule
                      | None -> return ()
                  and () = resolver#disable in
                  wakeup wakener ();
                  return ()
          end

  let stop_signal stop () =
    ignore_result (Lazy.force stop)

  let make_signal event push_command =
    let disable = lazy(
      let waiter, wakener = Lwt.task () in
      push_command (Some(Done wakener));
      waiter
    ) in
    let event = Lwt_event.with_finaliser (stop_signal disable) event in
    let _, wakener = Lwt.wait () in
    push_command (Some(Set_filters(wakener, [])));
    (object
       val mutable connected = true
       method event =
         if connected then
           event
         else
           failwith "OBus_proxy.event: signal disconnected"
       method set_filters filters =
         if connected then begin
           let waiter, wakener = Lwt.task () in
           push_command (Some(Set_filters(wakener, filters)));
           waiter
         end else
           fail (Failure "OBus_proxy.set_filters: signal disconnected")
       method disconnect =
         if connected then begin
           connected <- false;
           Lazy.force disable
         end else begin
           return ()
         end
     end)

  let dyn_connect proxy ~interface ~member =
    let event, push = React.E.create () and commands, push_command = Lwt_stream.create () in
    ignore (_connect proxy ~interface ~member ~push ~commands);
    make_signal (React.E.map (fun (connection, message) -> OBus_message.body message) event) push_command

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
    let event, push = React.E.create () and commands, push_command = Lwt_stream.create () in
    ignore (_connect proxy ~interface ~member ~push ~commands);
    make_signal (React.E.fmap (cast interface member typ) event) push_command

  (* +---------------------------------------------------------------+
     | Interface creation                                            |
     +---------------------------------------------------------------+ *)

  let make_interface interface = {
    Interface.name = interface;
    Interface.method_call = (fun member typ proxy -> method_call proxy ~interface ~member typ);
    Interface.signal = (fun member typ proxy -> connect proxy ~interface ~member typ);
    Interface.property_reader = (fun member typ proxy -> get proxy ~interface ~member typ);
    Interface.property_writer = (fun member typ proxy value -> set proxy ~interface ~member typ value);
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
