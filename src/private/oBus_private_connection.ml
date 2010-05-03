(*
 * oBus_private_connction.ml
 * -------------------------
 * Copyright : (c) 2009-2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* This file contains type definitions for D-Bus connections.

   These definitions cannot go into [OBus_connection] because several
   modules of obus need to access them directly. *)

let section = Lwt_log.Section.make "obus(connection)"

type void
  (* Empty type *)

(* +-----------------------------------------------------------------+
   | Maps and sets used in connections                               |
   +-----------------------------------------------------------------+ *)

module Serial_map = Map.Make
  (struct
     type t = OBus_message.serial
     let compare : int32 -> int32 -> int = compare
   end)

module Signal_map = Map.Make
  (struct
     type t = OBus_path.t * OBus_name.interface * OBus_name.member
     let compare = Pervasives.compare
   end)

module Property_map = Map.Make
  (struct
     type t = OBus_name.bus option * OBus_path.t * OBus_name.interface
     let compare = Pervasives.compare
   end)

module Object_map = Map.Make(OBus_path)

module String_map = Map.Make(String)
module String_set = Set.Make(String)

module Name_map = String_map
module Name_set = String_set

(* +-----------------------------------------------------------------+
   | Local D-Bus objects                                             |
   +-----------------------------------------------------------------+ *)

(* Type of static object *)
type static_object = {
  static_object_handle : OBus_value.V.sequence context -> unit Lwt.t;
  (* The method call handler *)

  static_object_connection_closed : t -> unit;
  (* Hook for when the connection is closed *)
}

(* Type of a dynamic node, which creates object on the fly when
   accessed *)
and dynamic_object = OBus_value.V.sequence context -> OBus_path.t -> static_object option Lwt.t

(* +-----------------------------------------------------------------+
   | Name resolvers                                                  |
   +-----------------------------------------------------------------+ *)

(* We keep on each connection a mapping from names we are interested
   in to their owner.

   These are basically the ones for which there is a signal receiver
   filtering on this name (module OBus_signal), or ones the user is
   interested in (module OBus_resolver).

   - if the name is not a unique name, then this let us know who is
   the current owner of the name. This is important because the sender
   field of messages is awlays set to the sender unique name, so we
   need to have this information in order to correctly dispatch
   signals.

   - if the name is a unique name then this let us know when the peer
   exit. When it happen, since the name will never be valid again, we
   can destroy all resources using this name
*)
and resolver = {
  resolver_owner : OBus_name.bus option React.signal;
  (* The owner of the name *)

  resolver_set : OBus_name.bus option -> unit;
  (* Setter for the name owner. *)

  mutable resolver_ref_count : int;
  (* Number of resolver for this name. When this number is 0, the name
     resolver can be removed. *)

  resolver_match_rule : string;
  (* The matching rule, for the message bus *)

  mutable resolver_state : resolver_state;
  (* State of the resolver *)
}

and resolver_state =
  | Resolver_init of resolver Lwt.t * resolver Lwt.u
      (* The resolver is being initialised. The arguemnts are:

         - the thread waiting for the resolver

         - the wakener for the first argument. The wakener is stored
         here because the resolver can be initialised both in
         OBus_resolver when the initial "GetnameOwner" returns or by
         OBus_connection if a signal "NameOwnerChanged" is received
         before. *)
  | Resolver_running
      (* The resolver is up and running *)

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

(* Type of signal receiver *)
and receiver = {
  mutable receiver_active : bool;
  (* Whether this receiver is active or not.

     If the connection is a peer-to-peer connection, resolver are
     immediatly active. Otherwise they are not active until the
     associated name resolver gets ready. *)

  mutable receiver_sender : OBus_name.bus option React.signal option;
  (* The sender of signals we are interested in *)

  mutable receiver_rule : string;
  (* The matching rule used for this receiver. It is not directly a
     matching rule of type OBus_match.t because this would create a
     circular dependency. *)

  mutable receiver_filter : OBus_message.t -> bool;
  (* Message filtering. It is used to filter on message body if the
     user defined argument filters. *)

  receiver_push : void context -> unit;
  (* Function used to send new events *)
}

(* Type of groups of signal receivers. Signal receivers are grouped by
   (path, interface, member) in order to factorise matching rules. *)
and receiver_group = {
  mutable receiver_group_rules : String_set.t;
  (* The set of rules that are currently sets on the message bus *)

  mutable receiver_group_mutex : Lwt_mutex.t;
  (* Mutex used to prevent concurrent updates of rules on the message
     bus *)

  receiver_group_receivers : receiver Lwt_sequence.t;
  (* The set of signal receivers on this grouup *)

  receiver_group_connection : t;
  receiver_group_path : OBus_path.t;
  receiver_group_interface : OBus_name.interface;
  receiver_group_member : OBus_name.member;
  (* Informations shared by all receivers of the group *)
}

(* +-----------------------------------------------------------------+
   | Properties                                                      |
   +-----------------------------------------------------------------+ *)

(* We keep properties on the message bus to allow caching when asked
   or needed. *)

(* State of a property group *)
and property_group_state =
  | Property_group_simple
      (* The property is not monitored *)
  | Property_group_cached of notify_data Lwt.t
      (* Properties are cached until the next iteration of the main
         loop *)
  | Property_group_monitor of notifier Lwt.t
      (* Properties are being monitored *)

(* Type of all properties of an interface *)
and property_group = {
  mutable property_group_ref_count : int;
  (* How many user properties (of type OBus_property.t) are using this
     property group ? *)

  mutable property_group_state : property_group_state;

  property_group_connection : t;
  property_group_owner : OBus_name.bus option;
  property_group_path : OBus_path.t;
  property_group_interface : OBus_name.interface;
  (* Property parameters *)
}

(* Type of a property notifier. It should contains the state of all
   properties of an interface: *)
and notifier = {
  notifier_signal : notify_data React.signal;
  (* Event which occurs each time one or more proeprties change *)

  notifier_stop : unit -> unit;
  (* Stop the notifier *)
}

and notify_data = void context * OBus_value.V.single String_map.t
    (* Mapping from member names to their current value *)

(* +-----------------------------------------------------------------+
   | Contexts                                                        |
   +-----------------------------------------------------------------+ *)

and 'a context = {
  context_connection : t;
  context_message : OBus_message.t;
  context_body : 'a -> OBus_value.V.sequence;
  context_replied : bool ref;
}

(* +-----------------------------------------------------------------+
   | Connection                                                      |
   +-----------------------------------------------------------------+ *)

and filter = OBus_message.t -> OBus_message.t option
  (* Type of message filters *)

(* Connection are wrapped into object in order to make them
   comparable. In the code, wrapped connection are simply referred has
   "connection" and internal connection details are referred as
   "running". *)

(* Type of running connections *)
and running = {
  mutable running_name : OBus_name.bus option;
  (* Unique name of the connection. If set this means that the other
     side is a message bus. *)

  running_acquired_names : Name_set.t React.signal;
  running_set_acquired_names : Name_set.t -> unit;
  (* List of names we currently own *)

  running_transport : OBus_transport.t;
  (* The transport used for messages *)

  running_on_disconnect : (exn -> unit) ref;
  (* [on_disconnect] is called the connection is closed
     prematurely. This happen on transport errors. *)

  running_guid : OBus_address.guid option;
  (* Guid of the connection. It may is [Some guid] if this is the
     client-side part of a peer-to-peer connection and the connection
     is shared. *)

  running_down : (unit Lwt.t * unit Lwt.u) option React.signal;
  running_set_down : (unit Lwt.t * unit Lwt.u) option -> unit;
  (* Waiting thread used to make the connection to stop dispatching
     messages. *)

  running_state : [ `Up | `Down ] React.signal;

  running_abort_recv_wakener : OBus_message.t Lwt.u;
  running_abort_send_wakener : unit Lwt.u;
  running_abort_recv : OBus_message.t Lwt.t;
  running_abort_send : unit Lwt.t;
  (* Waiting threads wakeup when the connection is closed or
     aborted. It is used to make the dispatcher/writer to exit. *)

  running_watch : unit Lwt.t;
  (* Thread returned by [OBus_connection.watch] *)

  mutable running_resolvers : resolver Name_map.t;
  (* Mapping bus-name <-> resolver *)

  running_exited_peers : OBus_name.bus OBus_cache.t;
  (* OBus_cache of bus names of exited peers. It is used by [OBus_resolver]
     to minimize the number of request to the message bus *)

  mutable running_next_serial : OBus_message.serial;
  (* The first available serial, incremented for each message *)

  mutable running_outgoing_m : Lwt_mutex.t;
  (* Mutex used to serialise message sending *)

  mutable running_static_objects : static_object Object_map.t;
  (* Mapping path -> objects for all objects exported on the
     connection *)

  mutable running_dynamic_objects : dynamic_object Object_map.t;
  (* List of dynamic object nodes *)

  running_incoming_filters : filter Lwt_sequence.t;
  running_outgoing_filters : filter Lwt_sequence.t;

  mutable running_reply_waiters : OBus_message.t Lwt.u Serial_map.t;
  (* Mapping serial -> thread waiting for a reply *)

  mutable running_receiver_groups : receiver_group Signal_map.t;
  (* Mapping (inteface, member, path) -> set of signal receivers *)

  mutable running_properties : property_group Property_map.t;
  (* Mapping holding all properties currently in use *)

  running_wrapper : t;
  (* The wrapper containing the connection *)
}

(* State of a connection *)
and connection_state =
  | Crashed of exn
      (* The connection has crashed with this exception *)
  | Running of running
      (* The connection is currently running *)

(* Connections are packed into objects to make them comparable *)
and t = <
  get : connection_state;
  (* Get the connection state *)

  set_crash : exn -> exn Lwt.t;
  (* Put the connection in a 'crashed' state if not already
     done. Returns the exception to which the connection is set to. *)

  running : bool React.signal;
  (* Signal holding the current connection state. *)
>

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

(* The exception that must be raised when a method is unknown *)
let unknown_method_message message = match message with
  | { OBus_message.typ = OBus_message.Method_call(path, Some interface, member); OBus_message.body = body } ->
      Printf.sprintf "Method %S with siganture %S on interface %S doesn't exist"
        member
        (OBus_value.string_of_signature (OBus_value.V.type_of_sequence body))
        interface
  | { OBus_message.typ = OBus_message.Method_call(path, None, member); OBus_message.body = body } ->
      Printf.sprintf "Method %S with signature %S doesn't exist"
        member
        (OBus_value.string_of_signature (OBus_value.V.type_of_sequence body))
  | _ ->
      invalid_arg "OBus_private_connection.unknown_mehtod_exn"

(* Returns the list of children of a node *)
let children running prefix =
  String_set.elements
    (Object_map.fold
       (fun path obj acc -> match OBus_path.after prefix path with
          | Some(element :: _) -> String_set.add element acc
          | _ -> acc)
       running.running_static_objects
       String_set.empty)

let running_of_connection connection =
  match connection#get with
    | Crashed exn ->
        raise exn
    | Running running ->
        running

let check_connection connection =
  match connection#get with
    | Crashed exn ->
        raise exn
    | Running running ->
        ()

(* Apply a list of filter on a message, logging failure *)
let apply_filters typ message filters =
  try
    Lwt_sequence.fold_l
      (fun filter message -> match message with
         | Some message -> filter message
         | None -> None)
      filters (Some message)
  with exn ->
    ignore (Lwt_log.error_f ~section ~exn "an %s filter failed with" typ);
    None

(* +-----------------------------------------------------------------+
   | Errors                                                          |
   +-----------------------------------------------------------------+ *)

exception Connection_closed
exception Connection_lost
exception Transport_error of exn

(* +-----------------------------------------------------------------+
   | Sending messages                                                |
   +-----------------------------------------------------------------+ *)

open Lwt
open OBus_message

(* Send a message, maybe adding a reply waiter and return
   [return_thread] *)
let send_message_backend connection reply_waiter_opt message =
  let running = running_of_connection connection in
  Lwt_mutex.with_lock running.running_outgoing_m begin fun () ->
    let send_it, closed = match connection#get with
      | Running _ ->
          (true, false)
      | Crashed Connection_closed ->
          (true, true)
      | Crashed _ ->
          (false, true)
    in
    if send_it then begin
      let message = { message with serial = running.running_next_serial } in
      match apply_filters "outgoing" message running.running_outgoing_filters with
        | None ->
            lwt () = Lwt_log.debug ~section "outgoing message dropped by filters" in
            fail (Failure "message dropped by filters")

        | Some message ->
            if not closed then begin
              match reply_waiter_opt with
                | Some(waiter, wakener) ->
                    running.running_reply_waiters <- Serial_map.add message.serial wakener running.running_reply_waiters;
                    on_cancel waiter (fun () ->
                                        match connection#get with
                                          | Crashed _ ->
                                              ()
                                          | Running running ->
                                              running.running_reply_waiters <- Serial_map.remove message.serial running.running_reply_waiters)
                | None ->
                    ()
            end;

            try_lwt
              lwt () = choose [running.running_abort_send;
                               (* Do not cancel a thread while it is marshaling message: *)
                               protected (OBus_transport.send running.running_transport message)] in
              (* Everything went OK, continue with a new serial *)
              running.running_next_serial <- Int32.succ running.running_next_serial;
              return ()
            with
              | OBus_wire.Data_error _ as exn ->
                  (* The message can not be marshaled for some
                     reason. This is not a fatal error. *)
                  fail exn

              | Canceled ->
                  (* Message sending have been canceled by the
                     user. This is not a fatal error either. *)
                  fail Canceled

              | exn ->
                  (* All other errors are considered as fatal. They
                     are fatal because it is possible that a message
                     has been partially sent on the connection, so the
                     message stream is broken *)
                  lwt exn = connection#set_crash (Transport_error exn) in
                  fail exn
    end else
      match connection#get with
        | Crashed exn ->
            fail exn
        | Running _ ->
            return ()
  end

let send_message packed message =
  send_message_backend packed None message

let send_message_with_reply packed message =
  let (waiter, wakener) as v = task () in
  lwt () = send_message_backend packed (Some v) message in
  waiter

(* +-----------------------------------------------------------------+
   | Contexts and replies                                            |
   +-----------------------------------------------------------------+ *)

let make_context ~connection ~message = {
  context_connection = connection;
  context_message = message;
  context_body = (fun void -> assert false);
  context_replied = ref false;
}

let make_context_with_reply ~connection ~message = {
  context_connection = connection;
  context_message = message;
  context_body = (fun body -> body);
  context_replied = ref false;
}

let send_reply context x =
  if !(context.context_replied) then
    return ()
  else begin
    context.context_replied := true;
    let msg = context.context_message in
    send_message context.context_connection {
      destination = msg.sender;
      sender = None;
      flags = { no_reply_expected = true; no_auto_start = true };
      serial = 0l;
      typ = Method_return msg.serial;
      body = context.context_body x
    }
  end

let send_error_by_name context name error_message =
  if !(context.context_replied) then
    return ()
  else begin
    context.context_replied := true;
    let msg = context.context_message in
    send_message context.context_connection {
      destination = msg.sender;
      sender = None;
      flags = { no_reply_expected = true; no_auto_start = true };
      serial = 0l;
      typ = Error(msg.serial, name);
      body = [OBus_value.V.basic_string error_message];
    }
  end

let send_error context exn error_message =
  send_error_by_name context (OBus_error.name_of_exn exn) error_message
