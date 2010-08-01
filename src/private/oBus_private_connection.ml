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
  so_handle : OBus_value.V.sequence message_context -> OBus_message.t -> [ `Replied | `No_reply ] Lwt.t;
  (* The method call handler *)

  so_connection_closed : t -> unit;
  (* Hook for when the connection is closed *)
}

(* Type of a dynamic node, which creates object on the fly when
   accessed *)
and dynamic_object = OBus_value.V.sequence message_context -> OBus_path.t -> [ `Replied | `No_reply | `Object of static_object | `Not_found ] Lwt.t

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
and name_resolver = {
  nr_owner : OBus_name.bus option React.signal;
  (* The owner of the name *)

  nr_set : OBus_name.bus option -> unit;
  (* Setter for the name owner. *)

  mutable nr_ref_count : int;
  (* Number of resolver for this name. When this number is 0, the name
     resolver can be removed. *)

  nr_match_rule : string;
  (* The matching rule, for the message bus *)

  mutable nr_state : resolver_state;
  (* State of the resolver *)
}

and resolver_state =
  | Resolver_init of name_resolver Lwt.t * name_resolver Lwt.u
      (* The resolver is being initialised. Arguemnts are:

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
and signal_receiver = {
  mutable sr_active : bool;
  (* Whether this receiver is active or not.

     If the connection is a peer-to-peer connection, resolver are
     immediatly active. Otherwise they are not active until the
     associated name resolver gets ready. *)

  mutable sr_sender : OBus_name.bus option React.signal option;
  (* The sender of signals we are interested in *)

  mutable sr_rule : OBus_match.rule option;
  (* The matching rule used for this receiver. *)

  mutable sr_filter : OBus_message.t -> bool;
  (* Message filtering. It is used to filter on message body if the
     user defined argument filters. *)

  sr_push : void message_context * OBus_message.t -> unit;
  (* Function used to send new events *)
}

(* Type of groups of signal receivers. Signal receivers are grouped by
   (path, interface, member) in order to factorise matching rules. *)
and signal_receiver_group = {
  mutable srg_rules : String_set.t;
  (* The set of rules that are currently sets on the message bus *)

  mutable srg_mutex : Lwt_mutex.t;
  (* Mutex used to prevent concurrent updates of rules on the message
     bus *)

  srg_receivers : signal_receiver Lwt_sequence.t;
  (* The set of signal receivers on this grouup *)

  srg_connection : t;
  srg_path : OBus_path.t;
  srg_interface : OBus_name.interface;
  srg_member : OBus_name.member;
  (* Informations shared by all receivers of the group *)
}

(* +-----------------------------------------------------------------+
   | Properties                                                      |
   +-----------------------------------------------------------------+ *)

and properties = OBus_value.V.single String_map.t
    (* Mapping from property name to values *)

(* Action send to property watchers *)
and action =
  | Invalidate
      (* Invalidate the cache of properties *)
  | Update of void message_context * (OBus_name.interface * (string * OBus_value.V.single) list * string list)
      (* Update the cache with the given property values *)

(* Type of all properties of an interface *)
and property_group = {
  mutable pg_ref_count : int;
  (* How many user properties (of type OBus_property.t) are using this
     property group ? *)

  mutable pg_watcher : property_group_watcher option;

  pg_connection : t;
  pg_owner : OBus_name.bus option;
  pg_path : OBus_path.t;
  pg_interface : OBus_name.interface;
  (* Property parameters *)
}

(* Type of a property group watcher. It keeps and updates a cache of
   all properties of an interface *)
and property_group_watcher = {
  pgw_cache : (void message_context * properties) Lwt.t React.signal;
  (* Current contents of the cache *)

  pgw_send : action -> unit;
  (* Send an action to the property watcher *)

  pgw_stop : unit -> unit Lwt.t;
  (* Stop the property watcher *)
}

(* +-----------------------------------------------------------------+
   | Contexts                                                        |
   +-----------------------------------------------------------------+ *)

and 'a message_context = {
  mc_connection : t;
  mc_flags : OBus_message.flags;
  mc_sender : OBus_name.bus option;
  mc_destination : OBus_name.bus option;
  mc_serial : OBus_message.serial;
  mc_make_body : 'a -> OBus_value.V.sequence;
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
and running_connection = {
  mutable rc_name : OBus_name.bus option;
  (* Unique name of the connection. If set this means that the other
     side is a message bus. *)

  rc_acquired_names : Name_set.t React.signal;
  rc_set_acquired_names : Name_set.t -> unit;
  (* List of names we currently own *)

  rc_transport : OBus_transport.t;
  (* The transport used for messages *)

  rc_on_disconnect : (exn -> unit) ref;
  (* [on_disconnect] is called the connection is closed
     prematurely. This happen on transport errors. *)

  rc_guid : OBus_address.guid option;
  (* Guid of the connection. It may is [Some guid] if this is the
     client-side part of a peer-to-peer connection and the connection
     is shared. *)

  rc_down : (unit Lwt.t * unit Lwt.u) option React.signal;
  rc_set_down : (unit Lwt.t * unit Lwt.u) option -> unit;
  (* Waiting thread used to make the connection to stop dispatching
     messages. *)

  rc_state : [ `Up | `Down ] React.signal;

  rc_abort_recv_wakener : OBus_message.t Lwt.u;
  rc_abort_send_wakener : unit Lwt.u;
  rc_abort_recv : OBus_message.t Lwt.t;
  rc_abort_send : unit Lwt.t;
  (* Waiting threads wakeup when the connection is closed or
     aborted. It is used to make the dispatcher/writer to exit. *)

  rc_watch : unit Lwt.t;
  (* Thread returned by [OBus_connection.watch] *)

  mutable rc_resolvers : name_resolver Name_map.t;
  (* Mapping bus-name <-> resolver *)

  rc_exited_peers : OBus_name.bus OBus_cache.t;
  (* OBus_cache of bus names of exited peers. It is used by [OBus_resolver]
     to minimize the number of request to the message bus *)

  mutable rc_next_serial : OBus_message.serial;
  (* The first available serial, incremented for each message *)

  mutable rc_outgoing_m : Lwt_mutex.t;
  (* Mutex used to serialise message sending *)

  mutable rc_static_objects : static_object Object_map.t;
  (* Mapping path -> objects for all objects exported on the
     connection *)

  mutable rc_dynamic_objects : dynamic_object Object_map.t;
  (* List of dynamic object nodes *)

  rc_incoming_filters : filter Lwt_sequence.t;
  rc_outgoing_filters : filter Lwt_sequence.t;

  mutable rc_reply_waiters : OBus_message.t Lwt.u Serial_map.t;
  (* Mapping serial -> thread waiting for a reply *)

  mutable rc_receiver_groups : signal_receiver_group Signal_map.t;
  (* Mapping (inteface, member, path) -> set of signal receivers *)

  mutable rc_properties : property_group Property_map.t;
  (* Mapping holding all properties currently in use *)

  rc_wrapper : t;
  (* The wrapper containing the connection *)
}

(* State of a connection *)
and connection_state =
  | Crashed of exn
      (* The connection has crashed with this exception *)
  | Running of running_connection
      (* The connection is currently running *)

(* Connections are packed into objects to make them comparable *)
and t = <
  state : connection_state;
  (* Get the connection state *)

  get : running_connection;
  (* Returns the connection if it is running, and fail otherwise *)

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
       running.rc_static_objects
       String_set.empty)

let check_connection connection =
  match connection#state with
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
  let running = connection#get in
  Lwt_mutex.with_lock running.rc_outgoing_m begin fun () ->
    let send_it, closed = match connection#state with
      | Running _ ->
          (true, false)
      | Crashed Connection_closed ->
          (true, true)
      | Crashed _ ->
          (false, true)
    in
    if send_it then begin
      let message = { message with serial = running.rc_next_serial } in
      match apply_filters "outgoing" message running.rc_outgoing_filters with
        | None ->
            lwt () = Lwt_log.debug ~section "outgoing message dropped by filters" in
            fail (Failure "message dropped by filters")

        | Some message ->
            if not closed then begin
              match reply_waiter_opt with
                | Some(waiter, wakener) ->
                    running.rc_reply_waiters <- Serial_map.add message.serial wakener running.rc_reply_waiters;
                    on_cancel waiter (fun () ->
                                        match connection#state with
                                          | Crashed _ ->
                                              ()
                                          | Running running ->
                                              running.rc_reply_waiters <- Serial_map.remove message.serial running.rc_reply_waiters)
                | None ->
                    ()
            end;

            try_lwt
              lwt () = choose [running.rc_abort_send;
                               (* Do not cancel a thread while it is marshaling message: *)
                               protected (OBus_transport.send running.rc_transport message)] in
              (* Everything went OK, continue with a new serial *)
              running.rc_next_serial <- Int32.succ running.rc_next_serial;
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
      match connection#state with
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
  mc_connection = connection;
  mc_flags = message.flags;
  mc_sender = message.sender;
  mc_destination = message.destination;
  mc_serial = message.serial;
  mc_make_body = (fun void -> assert false);
}

let make_context_with_reply ~connection ~message = {
  mc_connection = connection;
  mc_flags = message.flags;
  mc_sender = message.sender;
  mc_destination = message.destination;
  mc_serial = message.serial;
  mc_make_body = (fun body -> body);
}

let send_reply context x =
  send_message context.mc_connection {
    destination = context.mc_sender;
    sender = None;
    flags = { no_reply_expected = true; no_auto_start = true };
    serial = 0l;
    typ = Method_return context.mc_serial;
    body = context.mc_make_body x
  }

let send_error context exn =
  let name, error_message = OBus_error.cast exn in
  send_message context.mc_connection {
    destination = context.mc_sender;
    sender = None;
    flags = { no_reply_expected = true; no_auto_start = true };
    serial = 0l;
    typ = Error(context.mc_serial, name);
    body = [OBus_value.V.basic_string error_message];
  }
