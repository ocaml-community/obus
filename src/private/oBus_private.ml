(*
 * oBus_private.ml
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* This file contain data type that need to be shared between the
   different modules of OBus *)

open Lwt

module SerialMap = OBus_util.MakeMap
  (struct
     type t = OBus_message.serial
     let compare : int32 -> int32 -> int = compare
   end)

module ObjectMap = OBus_util.MakeMap
  (struct
     type t = OBus_path.t
     let compare = Pervasives.compare
   end)

module SignalMap = OBus_util.MakeMap
  (struct
     type t = OBus_path.t * OBus_name.interface * OBus_name.member
     let compare = Pervasives.compare
   end)

module RuleSet = Set.Make
  (struct
     type t = OBus_match.rule
     let compare = Pervasives.compare
   end)

module PropertyMap = OBus_util.MakeMap
  (struct
     type t = OBus_name.bus option * OBus_path.t * OBus_name.interface
     let compare = Pervasives.compare
   end)

module StringMap = OBus_util.MakeMap(String)
module StringSet = Set.Make(String)

module NameMap = StringMap
module NameSet = StringSet

(* +-----------------------------------------------------------------+
   | Objects                                                         |
   +-----------------------------------------------------------------+ *)

type packed_object = exn

type obus_object = {
  oo_handle : packed_object -> packed_connection -> OBus_message.t -> unit Lwt.t;
  (* Method call handler *)

  oo_connection_closed : packed_connection -> unit;
  (* Hook for when a connection is closed *)

  oo_object : packed_object;
  (* The object, hidden in an exception *)
}

and dynamic_object = {
  do_prefix : OBus_path.t;
  (* The prefix of the dynamic node *)

  do_create : OBus_path.t -> obus_object Lwt.t;
  (* Function used to create the object on the fly *)
}

(* +-----------------------------------------------------------------+
   | Name resolvers                                                  |
   +-----------------------------------------------------------------+ *)

(* We keep on each connection a mapping from names we are interested
   to their owner.

   These are basically the ones for which there is a signal receiver
   filtering on this name.

   - if the name is not a unique name, then this let us know who is
   the current owner of the name. This is important because the sender
   field of messages is awlays set to the sender unique name, so we
   need to have this information in order to correctly dispatch the
   message.

   - if the name is a unique name then this let us know when the peer
   exit. When it happen, since the name will never be valid again, we
   can destroy all resources using this name
*)
and name_resolver = {
  nr_owner : OBus_name.bus option React.signal;
  (* The owner of the name *)

  nr_set : OBus_name.bus option -> unit;
  (* Setters for the name owner. *)

  mutable nr_ref_count : int;
  (* Number of resolver for this name. When this number is 0, the name
     resolver can be removed. *)

  nr_match_rule : OBus_match.rule;
  (* The matching rule, for the message bus *)

  mutable nr_state : name_resolver_state;
  (* State of the resolver *)
}

and name_resolver_state =
  | Nrs_init of name_resolver Lwt.t * name_resolver Lwt.u
      (* The resolver is being initialised *)
  | Nrs_running
      (* The resolver has already been initalised and is now
         running *)

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

and signal_receiver = {
  mutable sr_active : bool;
  (* Whether this receiver is active or not. Initially receiver are
     not active. *)

  mutable sr_sender : OBus_name.bus option React.signal option;
  (* The sender that must be matched *)

  mutable sr_rule : OBus_match.rule option;
  (* The rule used for this receiver *)

  sr_push : packed_connection * OBus_message.t -> unit;
  (* Function used to send new events *)
}

and signal_receiver_set = {
  mutable srs_rules : RuleSet.t;
  (* The set of rules used by receivers, that are currently set on the
     message bus *)

  mutable srs_mutex : Lwt_mutex.t;
  (* Mutex used to prevent concurrent updates of rules *)

  srs_receivers : signal_receiver Lwt_sequence.t;
}

(* +-----------------------------------------------------------------+
   | Properties                                                      |
   +-----------------------------------------------------------------+ *)

and property_state =
  | Prop_simple
      (* Simpe state: no property is monitored for this interface *)
  | Prop_monitor of (OBus_value.single StringMap.t * OBus_private_type.context) React.signal Lwt.t
      (* At least one property is monitored. In this we use [GetAll]
         to minimise the number fo method calls *)

and property = {
  mutable prop_ref_count : int;
  (* How many user properties (of type OBus_property.t) are using this
     property ? *)

  mutable prop_state : property_state;
}

(* +-----------------------------------------------------------------+
   | Connection                                                      |
   +-----------------------------------------------------------------+ *)

and filter = OBus_message.t -> OBus_message.t option

and connection = {
  mutable name : OBus_name.bus option;
  (* Unique name of the connection. If set this means that the other
     side is a message bus. *)

  acquired_names : NameSet.t React.signal;
  set_acquired_names : NameSet.t -> unit;
  (* List of names we currently own *)

  transport : OBus_transport.t;
  (* The transport used for messages *)

  on_disconnect : (exn -> unit) ref;
  (* [on_disconnect] is called the connection is closed
     prematurely. This happen on transport errors. *)

  guid : OBus_address.guid option;
  (* Guid of the connection. It may is [Some guid] if this is the
     client-side part of a peer-to-peer connection and the connection
     is shared. *)

  down : (unit Lwt.t * unit Lwt.u) option React.signal;
  set_down : (unit Lwt.t * unit Lwt.u) option -> unit;
  (* Waiting thread used to make the connection to stop dispatching
     messages. *)

  state : [ `Up | `Down ] React.signal;

  abort_recv_wakener : OBus_message.t Lwt.u;
  abort_send_wakener : unit Lwt.u;
  abort_recv : OBus_message.t Lwt.t;
  abort_send : unit Lwt.t;
  (* Waiting threads wakeup when the connection is closed or
     aborted. It is used to make the dispatcher/writer to exit. *)

  watch : unit Lwt.t;
  (* Thread returned by [OBus_connection.watch] *)

  mutable name_resolvers : name_resolver NameMap.t;
  (* Mapping bus-name <-> resolver *)

  exited_peers : OBus_name.bus OBus_cache.t;
  (* OBus_cache of bus names of exited peers. It is used by [OBus_resolver]
     to minimize the number of request to the message bus *)

  mutable next_serial : OBus_message.serial;
  (* The first available serial, incremented for each message *)

  mutable outgoing_m : Lwt_mutex.t;
  (* Mutex used to serialise message sending *)

  mutable exported_objects : obus_object ObjectMap.t;
  (* Mapping path -> objects method call handler for all objects
     exported on the connection *)

  mutable dynamic_objects : dynamic_object list;
  (* List of dynamic object nodes *)

  incoming_filters : filter Lwt_sequence.t;
  outgoing_filters : filter Lwt_sequence.t;

  mutable reply_waiters : OBus_message.t Lwt.u SerialMap.t;
  (* Mapping serial -> thread waiting for a reply *)

  mutable signal_receivers : signal_receiver_set SignalMap.t;
  (* Mapping (inteface, member, path) -> set of signal receivers *)

  mutable properties : property PropertyMap.t;
  (* Mapping holding all properties currently in use *)

  packed : packed_connection;
  (* The pack containing the connection *)
}

and connection_state =
  | Crashed of exn
  | Running of connection

(* Connections are packed into objects to make them comparable *)
and packed_connection = <
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

let unknown_method_exn message = match message with
  | { OBus_message.typ = OBus_message.Method_call(path, interface_opt, member) } ->
      let signature = OBus_value.string_of_signature
        (OBus_value.type_of_sequence (OBus_message.body message)) in
      begin match interface_opt with
        | Some interface ->
            OBus_error.Unknown_method
              (Printf.sprintf "Method %S with signature %S on interface %S doesn't exist"
                 member signature interface)
        | None ->
            OBus_error.Unknown_method
              (Printf.sprintf "Method %S with signature %S doesn't exist"
                 member signature)
      end

  | _ ->
      invalid_arg "OBus_internals.unknown_mehtod_exn"

let children connection path =
  ObjectMap.fold
    (fun p obj acc -> match OBus_path.after path p with
       | Some(elt :: _) -> if List.mem elt acc then acc else elt :: acc
       | _ -> acc)
    connection.exported_objects []

let unpack_connection packed =
  match packed#get with
    | Crashed exn ->
        raise exn
    | Running connection ->
        connection

let check_connection packed =
  match packed#get with
    | Crashed exn ->
        raise exn
    | Running connection ->
        ()
