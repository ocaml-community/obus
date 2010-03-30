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
  method set_filters : (int * OBus_match.argument_filter) list -> unit
  method auto_match_rule : bool
  method set_auto_match_rule : bool -> unit
  method init : ?filters : (int * OBus_match.argument_filter) list -> ?auto_match_rule : bool -> unit -> 'a React.event
  method disconnect : unit
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
  type broken = proxy with obus(basic)
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

  (* Commit rule changes on the message bus *)
  let commit_rules connection set =
    let rules =
      Lwt_sequence.fold_l
        (fun sr rules ->
           if sr.sr_active then
             match sr.sr_rule with
               | Some rule -> RuleSet.add rule rules
               | None -> rules
           else
             rules)
        set.srs_receivers RuleSet.empty
    in
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

  (* State of a signal receiver *)
  type signal_state =
    | Sig_init
        (* Still initialising *)
    | Sig_disconnected
        (* The receiver has been disconnected *)
    | Sig_connected
        (* Initialisation is done, the signal receiver is up and
           running *)

  let stop disconnect () = Lazy.force disconnect

  (* Creates a signal receiver. [event] is the event that is returned
     to the caller, and [push] the function passed to the connection
     to dispatch signals. *)
  class ['a] connect proxy interface member push (event : 'a React.event) =
    let proxy = Proxy.cast proxy in
    let connection = unpack_connection proxy.peer.connection in
    (* Signal sets are indexed by the tuple [(path, interface, member)]: *)
    let key = (proxy.path, interface, member) in
    let set =
      match SignalMap.lookup key connection.signal_receivers with
        | Some set ->
            set
        | None ->
            (* If the set do not exists, create a new one *)
            let set = {
              srs_rules = RuleSet.empty;
              srs_mutex = Lwt_mutex.create ();
              srs_receivers = Lwt_sequence.create ();
            } in
            connection.signal_receivers <- SignalMap.add key set connection.signal_receivers;
            set
    in
    let receiver = {
      sr_active = false;
      sr_sender = None;
      sr_rule = Some(OBus_match.rule
                       ~typ:`Signal
                       ?sender:proxy.peer.name
                       ~path:proxy.path
                       ~interface
                       ~member
                       ());
      sr_push = push;
    } in
    (* Immediatly add the recevier to avoid race condition *)
    let node = Lwt_sequence.add_r receiver set.srs_receivers in

    (* The state of the receiver: *)
    let state = ref Sig_init  in

    (* Thread which is wake up when the signal is disconnected *)
    let done_waiter, done_wakener = Lwt.wait () in

    let disconnect = lazy(
      state := Sig_disconnected;
      wakeup done_wakener ()
    ) in

    (* Disable the receiver on garbage collection: *)
    let event = Lwt_event.with_finaliser (stop disconnect) event in
  object(self)
    method event = event

    val mutable auto_match_rule_enabled = true
    val mutable current_filters = []

    method auto_match_rule = auto_match_rule_enabled

    method set_auto_match_rule auto_match_rule  =
      if auto_match_rule <> auto_match_rule_enabled then begin
        auto_match_rule_enabled <- auto_match_rule;
        if auto_match_rule then
          receiver.sr_rule <- Some(OBus_match.rule
                                     ~typ:`Signal
                                     ?sender:proxy.peer.name
                                     ~path:proxy.path
                                     ~interface
                                     ~member
                                     ~arguments:current_filters
                                     ())
        else
          receiver.sr_rule <- None;
        match !state with
          | Sig_init ->
              ()
          | Sig_disconnected ->
              invalid_arg "OBus_proxy.signal#set_auto_match_rule: disconnected signal"
          | Sig_connected ->
              ignore (commit_rules connection.packed set)
      end

    method set_filters filters =
      current_filters <- filters;
      if auto_match_rule_enabled then begin
        receiver.sr_rule <-Some(OBus_match.rule
                                  ~typ:`Signal
                                  ?sender:proxy.peer.name
                                  ~path:proxy.path
                                  ~interface
                                  ~member
                                  ~arguments:filters
                                  ());
        match !state with
          | Sig_init ->
              ()
          | Sig_disconnected ->
              invalid_arg "OBus_proxy.signal#set_filters: disconnected signal"
          | Sig_connected ->
              ignore (commit_rules connection.packed set)
      end

    method init ?(filters=[]) ?(auto_match_rule=true) () =
      current_filters <- filters;
      (* Force a refresh of the match rule if necessary: *)
      auto_match_rule_enabled <- not auto_match_rule;
      self#set_auto_match_rule auto_match_rule;
      event

    method disconnect =
      Lazy.force disconnect

    initializer
      ignore begin
        try_lwt
          if connection.OBus_private.name = None then begin
            (* If the connection is a peer-to-peer connection, there is
               nothing else to do. *)
            receiver.sr_active <- true;
            done_waiter
          end else begin
            lwt resolver =
              match proxy.peer.name with
                | None ->
                    return None
                | Some name ->
                    (* If we are interested on signals coming from a
                       particular peer, we need a name resolver: *)
                    lwt resolver = OBus_resolver.make connection.packed name in
                    receiver.sr_sender <- Some(OBus_resolver.owner resolver);
                    return (Some resolver)
            in
            try_lwt
              (* Yield to let the user add argument filters: *)
              lwt () = pause () in
              (* Since we yielded, check the connection again: *)
              check_connection connection.packed;
              if !state = Sig_disconnected then
                return ()
              else begin
                receiver.sr_active <- true;
                lwt () = commit_rules connection.packed set in
                done_waiter
              end
            finally
              match resolver with
                | None -> return ()
                | Some resolver -> OBus_resolver.disable resolver
          end
        finally
          React.E.stop event;
          Lwt_sequence.remove node;
          lwt () = commit_rules connection.packed set in
          if Lwt_sequence.is_empty set.srs_receivers then
            connection.signal_receivers <- SignalMap.remove key connection.signal_receivers;
          return ()
      end
  end

  let dyn_connect proxy ~interface ~member =
    let event, push = React.E.create () in
    new connect proxy interface member push (React.E.map (fun (connection, message) -> OBus_message.body message) event)

  let cast interface member typ (connection, message) =
    try
      Some(OBus_type.cast_sequence typ ~context:(OBus_connection.make_context (connection, message)) (OBus_message.body message))
    with exn ->
      ignore (
        Lwt_log.error_f ~section ~exn "failed to cast signal from %S, interface %S, member %S with signature %S to %S"
          (match OBus_message.sender message with None -> "" | Some n -> n) interface member
          (OBus_value.string_of_signature (OBus_value.type_of_sequence (OBus_message.body message)))
          (OBus_value.string_of_signature (OBus_type.type_sequence typ))
      );
      None

  let connect proxy ~interface ~member typ =
    let event, push = React.E.create () in
    new connect proxy interface member push (React.E.fmap (cast interface member typ) event)

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
