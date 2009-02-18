(*
 * oBus_intern.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This file contain data type that need to be shared between the
   different modules of OBus *)

open Lwt

module Serial_map = Util.Make_map(struct type t = OBus_message.serial end)
module Object_map = Util.Make_map(struct type t = OBus_path.t end)
module Name_map = Util.Make_map(struct type t = OBus_name.bus end)

(* +--------------------+
   | Callback functions |
   +--------------------+ *)

type 'a callback_mode =
    (* Calling mode for a callback function *)
  | Callback_mode_serial of unit Lwt.t ref
      (* Serialized call: each call is done when the previous is
         terminated. The argument is the thread of the previous call. *)
  | Callback_mode_parallel
      (* Parallel mode: call are done immediatly after an event is
         received *)

type 'a callback = {
  cb_mode : 'a callback_mode;
  cb_func : 'a -> unit Lwt.t;
}

let make_callback serial func = {
  cb_func = func;
  cb_mode =
    if serial then
      Callback_mode_serial(ref (return ()))
    else
      Callback_mode_parallel
}

(* Apply [f] to [x] and print an error if [f] fail. It is a
   programming error if a callback function fail. *)
let safe_call name f x =
  catch
    (fun _ -> f x)
    (fun exn ->
       Log.failure exn "%s failed with" name;
       return ())

(* Safely call a callback function according to its calling mode *)
let callback_apply name cb x = match cb.cb_mode with
  | Callback_mode_serial t_ref ->
      t_ref := !t_ref >>= (fun _ -> safe_call name cb.cb_func x)
  | Callback_mode_parallel ->
      ignore (safe_call name cb.cb_func x)

(* +--------------+
   | DBus objects |
   +--------------+ *)

type member_info =
  | MI_method of OBus_name.member * OBus_value.tsequence * (packed_connection -> OBus_message.t -> unit)
  | MI_signal
  | MI_property of OBus_name.member * (unit -> OBus_value.single Lwt.t) option * (OBus_value.single -> unit Lwt.t) option

and member_desc = OBus_introspect.declaration * member_info

(* Signature that [OBus_connection] need to known for handling
   objects *)
and dbus_object = <
    obus_handle_call : packed_connection -> OBus_message.t -> unit;
  (* Handle a method call *)

  obus_connection_closed : packed_connection -> unit;
  (* Do wathever needed when the connection is closed *)
  >

(* +----------------+
   | Name resolvers |
   +----------------+ *)

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
  nr_owner : name_resolver_internal;
  (* Owner of the name. This name is always a unique name *)

  mutable nr_ref_count : int;
  (* Number of things using it *)

  nr_match_rule : Match_rule.t;
  (* The matching rule, in case the end-point of the connection is a
     message bus *)

  nr_on_change : name_change_callback MSet.t;
  (* Functions called when the name owner change *)

  nr_init : unit Lwt.t;
  (* Sleeping thread which is wakeup after the initial resolution,
     even if it fails. *)

  mutable nr_initialized : bool;
  (* Tell wether the name resolver have been initialized. This is to
     avoid race conditions. *)
}

and name_resolver_internal = OBus_name.bus option ref

and name_change_callback = {
  ncc_callback : OBus_name.bus option callback;
  (* The callback function *)

  mutable ncc_initialized : bool;
  (* Wether the function has been called after the initial name
     resolution, to avoid race conditions. *)
}

(* +---------+
   | Signals |
   +---------+ *)

and signal_receiver = {
  (* Matching rules *)
  sr_sender : name_resolver_internal option;
  sr_path : OBus_path.t option;
  sr_interface : OBus_name.interface;
  sr_member : OBus_name.member;

  (* Matching on signal arguments:

     The message bus offer the possibility to match string arguments
     of messages. To implement this we also need to match arguments
     when the signal is received. *)
  sr_args : (int * string) list;
  (* This is a list of (relative position, constraint). Each position
     is relative to the previous one.

     So for example the filter [(0, "a"); (1, "b"); (0, "c")] will
     match any signal which have at least 4 arguments with:

     - "a" for argument 0
     - "b" for argument 2
     - "c" for argument 3
  *)

  sr_callback : (packed_connection * OBus_message.t) callback;
}

(* +------------+
   | Connection |
   +------------+ *)

and filter = OBus_message.t -> OBus_message.t option

and connection = {
  mutable name : OBus_name.bus option;
  (* Unique name of the connection. If set this means that the other
     side is a message bus. *)

  mutable acquired_names : OBus_name.bus list;
  (* List of names we currently own *)

  transport : OBus_lowlevel.transport;
  (* The transport used for messages *)

  on_disconnect : (exn -> unit) ref;
  (* [on_disconnect] is called the connection is closed
     prematurely. This happen on transport errors. *)

  guid : OBus_address.guid option;
  (* Guid of the connection. It may is [Some guid] if this is the
     client-side part of a peer-to-peer connection and the connection
     is shared. *)

  mutable down : unit Lwt.t option;
  (* Waiting thread used to make the connection to stop dispatching
     messages. *)

  abort : OBus_message.t Lwt.t;
  (* Waiting thread which is wakeup when the connection is closed or
     aborted. It is used to make the dispatcher to exit. *)

  watch : unit Lwt.t;
  (* Thread returned by [OBus_connection.watch] *)

  mutable name_resolvers : name_resolver Name_map.t;
  (* Mapping bus-name <-> resolver *)

  exited_peers : OBus_name.bus Cache.t;
  (* Cache of bus names of exited peers. It is used by [OBus_resolver]
     to minimize the number of request to the message bus *)

  mutable outgoing : OBus_message.serial Lwt.t;
  (* The ougoing thread.

     If a message is being sent, it is a waiting thread which is
     wakeup when the message is sent. It returns the current
     serial. *)

  mutable exported_objects : dbus_object Object_map.t;
  (* Mapping path -> objects for all objects exported on the
     connection *)

  incoming_filters : filter MSet.t;
  outgoing_filters : filter MSet.t;

  mutable reply_waiters : OBus_message.t Lwt.t Serial_map.t;
  (* Mapping serial -> thread waiting for a reply *)

  signal_receivers : signal_receiver MSet.t;

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
>;;

(* +-------+
   | Utils |
   +-------+ *)

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

(* Call the name owner change handler of a name resolver. [init] tell
   wether it is for initialization purpose or not. If it is and it has
   already been initialized then the call is discarded. *)
let call_resolver_handler ?(init=false) name_change_callback owner =
  if not init || not name_change_callback.ncc_initialized then begin
    (* The handler is now initialized *)
    name_change_callback.ncc_initialized <- true;

    callback_apply "resolver callback" name_change_callback.ncc_callback owner
  end

let children connection path =
  Object_map.fold
    (fun p obj acc -> match OBus_path.after path p with
       | Some(elt :: _) -> if List.mem elt acc then acc else elt :: acc
       | _ -> acc)
    connection.exported_objects []

let exit_hooks = MSet.make ()

let cleanup _ =
  (* Get the list of exit hooks *)
  let hooks = MSet.fold (fun x l -> x :: l) [] exit_hooks in
  MSet.clear exit_hooks;
  (* Call them all *)
  Lwt_unix.run
    (Lwt_util.iter
       (fun hook -> catch hook (fun exn -> Log.failure exn "exit hook failed with"; return ()))
       hooks)

let _ = at_exit cleanup
