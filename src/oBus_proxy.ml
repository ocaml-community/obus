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
  method start : unit
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

  (* Update rules on the message bus *)
  let update_rules connection set =
    let rules = Lwt_sequence.fold_l (fun sr rules -> RuleSet.add sr.sr_rule rules) set.srs_receivers RuleSet.empty in
    (* If rules have changed, compute the minimal set of changes to
       make with the message bus and do them: *)
    if RuleSet.compare rules set.srs_rules <> 0 then
      Lwt_mutex.with_lock set.srs_mutex
        (fun () ->
           let new_rules = RuleSet.diff rules set.srs_rules
           and old_rules = RuleSet.diff set.srs_rules rules in
           set.srs_rules <- new_rules;
           lwt () = RuleSet.fold (fun rule thread -> thread <&> OBus_private_bus.add_match connection rule) new_rules (return ())
           and () = RuleSet.fold (fun rule thread -> thread <&> OBus_private_bus.remove_match connection rule) old_rules (return ()) in
           return ())
    else
      return ()

  type signal_info = {
    sig_receiver : signal_receiver;
    (* The signal reciever itself *)

    sig_node : signal_receiver Lwt_sequence.node;
    (* The node to remove it *)

    sig_set : signal_receiver_set;
    (* The set the signal receiver is part of *)

    sig_resolver : OBus_resolver.t option;
    (* Resolver for the sender *)
  }

  (* Connect a signal to the given push function: *)
  let connect_signal ~proxy ~interface ~member ~start_waiter ~push =
    match proxy.peer.connection#get with
      | Crashed exn ->
          fail exn

      | Running connection ->
          let key = (proxy.path, interface, member) in
          let set =
            match SignalMap.lookup key connection.signal_receivers with
              | Some set ->
                  set
              | None ->
                  let set = {
                    srs_rules = RuleSet.empty;
                    srs_mutex = Lwt_mutex.create ();
                    srs_receivers = Lwt_sequence.create ();
                  } in
                  connection.signal_receivers <- SignalMap.add key set connection.signal_receivers;
                  set
          in

          try_lwt
            if connection.OBus_private.name = None then begin
              (* If the connection is a peer-to-peer connection the
                 only thing to do is to locally add the receiver *)
              let receiver = { sr_sender = None; sr_rule = OBus_match.rule (); sr_push = push } in
              let node = Lwt_sequence.add_r receiver set.srs_receivers in
              return {
                sig_receiver = receiver;
                sig_node = node;
                sig_set = set;
                sig_resolver = None;
              }

            end else begin
              (* Yield to let the user add argument filters: *)
              lwt () = pick [pause (); start_waiter] in

              let rule = OBus_match.rule
                ~typ:`Signal
                ?sender:proxy.peer.name
                ~path:proxy.path
                ~interface
                ~member
                ()
              in

              match proxy.peer.connection#get with
                | Crashed exn ->
                    fail exn

                | Running connection ->
                    match proxy.peer.name with
                      | None ->
                          let receiver = { sr_sender = None; sr_rule = rule; sr_push = push } in
                          let node = Lwt_sequence.add_r receiver set.srs_receivers in
                          lwt () =
                            try_lwt
                              update_rules connection.packed set
                            with exn ->
                              Lwt_sequence.remove node;
                              fail exn
                          in
                          return {
                            sig_receiver = receiver;
                            sig_node = node;
                            sig_set = set;
                            sig_resolver = None;
                          }

                      | Some name ->
                          lwt resolver = OBus_resolver.make connection.packed name in
                          let receiver = { sr_sender = Some(OBus_resolver.owner resolver); sr_rule = rule; sr_push = push } in
                          let node = Lwt_sequence.add_r receiver set.srs_receivers in
                          lwt () =
                            try_lwt
                              update_rules connection.packed set
                            with exn ->
                              Lwt_sequence.remove node;
                              fail exn
                          in
                          return {
                            sig_receiver = receiver;
                            sig_node = node;
                            sig_set = set;
                            sig_resolver = Some resolver;
                          }
            end
          with exn ->
            if Lwt_sequence.is_empty set.srs_receivers then
              connection.signal_receivers <- SignalMap.remove key connection.signal_receivers;
            fail exn

  let stop_signal stop () =
    ignore_result (Lazy.force stop)

  let make_signal proxy interface member event start_waiter start_wakener =
    let init_wakeup = lazy(wakeup start_wakener ()) in
    let disconnect = lazy(
      Lazy.force init_wakeup;
      lwt sig_info = start_waiter in
      React.E.stop event;
      Lwt_sequence.remove sig_info.sig_node;
      try_lwt
        update_rules proxy.peer.connection sig_info.sig_set
      finally
        if Lwt_sequence.is_empty sig_info.sig_set.srs_receivers then begin
          match proxy.peer.connection#get with
            | Crashed _ ->
                ()
            | Running connection ->
                connection.signal_receivers <- SignalMap.remove (proxy.path, interface, member) connection.signal_receivers
        end;
        return ()
    ) in
    let event = Lwt_event.with_finaliser (stop_signal disconnect) event in
    (object
       method event = event
       method start = Lazy.force init_wakeup
       method set_filters filters =
         Lazy.force init_wakeup;
         lwt sig_info = start_waiter in
         sig_info.sig_receiver.sr_rule <- OBus_match.rule
           ~typ:`Signal
           ?sender:proxy.peer.name
           ~path:proxy.path
           ~interface
           ~member
           ~arguments:filters
           ();
         update_rules proxy.peer.connection sig_info.sig_set
       method disconnect = Lazy.force disconnect
     end)

  let dyn_connect proxy ~interface ~member =
    let proxy = Proxy.cast proxy in
    let event, push = React.E.create ()
    and start_waiter, start_wakener = Lwt.wait () in
    let start_waiter = connect_signal ~proxy ~interface ~member ~start_waiter ~push in
    make_signal proxy interface member (React.E.map (fun (connection, message) -> OBus_message.body message) event) start_waiter start_wakener

  let cast interface member typ (connection, message) =
    try
      Some(OBus_type.cast_sequence typ ~context:(OBus_connection.make_context (connection, message)) (OBus_message.body message))
    with exn ->
      ignore (
        Lwt_log.exn_f ~section ~exn "failed to cast signal from %S, interface %S, member %S with signature %S to %S"
          (match OBus_message.sender message with None -> "" | Some n -> n) interface member
          (OBus_value.string_of_signature (OBus_value.type_of_sequence (OBus_message.body message)))
          (OBus_value.string_of_signature (OBus_type.type_sequence typ))
      );
      None

  let connect proxy ~interface ~member typ =
    let proxy = Proxy.cast proxy in
    let event, push = React.E.create ()
    and start_waiter, start_wakener = Lwt.wait () in
    let start_waiter = connect_signal ~proxy ~interface ~member ~start_waiter ~push in
    make_signal proxy interface member (React.E.fmap (cast interface member typ) event) start_waiter start_wakener

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
