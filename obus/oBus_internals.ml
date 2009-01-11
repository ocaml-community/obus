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
open OBus_message
open OBus_info
open OBus_value

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

(* +------------+
   | Connection |
   +------------+ *)

type 'a any_message = 'a OBus_message.t
constraint 'a = [< OBus_message.any_type ]

type member_info =
  | MI_method of OBus_name.member * tsequence * (connection -> method_call -> unit)
  | MI_signal
  | MI_property of OBus_name.member * (unit -> single Lwt.t) option * (single -> unit Lwt.t) option

and member_desc = OBus_introspect.declaration * member_info

and dbus_object = <
  obus_path : OBus_path.t;
  obus_handle_call : connection -> method_call -> unit;
  introspect : connection -> OBus_introspect.document Lwt.t;
  get : OBus_name.interface -> OBus_name.member -> OBus_value.single Lwt.t;
  set : OBus_name.interface -> OBus_name.member -> OBus_value.single -> unit Lwt.t;
  get_all : OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t;
  obus_add_interface : OBus_name.interface -> member_desc list -> unit;
  obus_export : connection -> unit;
  obus_remove : connection -> unit;
  obus_destroy : unit;
  obus_connection_closed : connection -> unit;
>

and filter = OBus_message.any -> OBus_message.any option

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
  (* Owner of the name. This name is always a unique name *)
  nr_owner : name_resolver_internal;

  (* Number of things using it *)
  mutable nr_ref_count : int;

  (* The matching rule, in case the end-point of the connection is a
     message bus *)
  nr_match_rule : Match_rule.t;

  (* Functions called when the name owner change *)
  nr_on_change : name_change_callback MSet.t;

  (* Sleeping thread which is wakeup after the initial resolution,
     even if it fails. *)
  nr_init : unit Lwt.t;

  (* Tell wether the name resolver have been initialized. This is to
     avoid race conditions. *)
  mutable nr_initialized : bool;
}

and name_resolver_internal = OBus_name.bus option ref

and name_change_callback = {
  ncc_callback : OBus_name.bus option callback;
  (* The callback function *)

  mutable ncc_initialized : bool;
  (* Wether the function has been called after the initial name
     resolution, to avoid race conditions. *)
}

and connection = <
  transport : OBus_lowlevel.transport;
  shutdown_transport_on_close : bool ref;
  on_disconnect : (exn -> unit) ref;
  guid : OBus_address.guid option;
  is_up : bool;
  set_up : unit;
  set_down : unit;
  watch : unit Lwt.t;
  add_signal_receiver : signal_receiver -> signal_receiver MSet.node;
  add_incoming_filter : filter -> filter MSet.node;
  add_outgoing_filter : filter -> filter MSet.node;
  add_name_resolver : OBus_name.bus -> name_resolver -> unit;
  remove_name_resolver : OBus_name.bus -> unit;
  find_name_resolver : OBus_name.bus -> name_resolver option;
  name : OBus_name.bus option;
  set_name : OBus_name.bus -> unit;
  peer_has_exited : OBus_name.bus -> bool;
  add_exited_peer : OBus_name.bus -> unit;
  export_object : OBus_path.t -> dbus_object -> unit;
  remove_object : OBus_path.t -> unit;
  find_object : OBus_path.t -> dbus_object option;
  send_message : 'a. 'a any_message -> unit Lwt.t;
  send_message_with_reply : OBus_message.method_call -> OBus_message.method_return Lwt.t;
  children : OBus_path.t -> OBus_introspect.node list;
  close : unit;
  is_bus : bool;
  running : bool;
>

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

  sr_callback : (connection * signal) callback;
}

(* +-------+
   | Utils |
   +-------+ *)

let unknown_method_exn message =
  let `Method_call(path, interface_opt, member) = message.typ in
  match interface_opt with
    | Some interface ->
        OBus_error.Unknown_method
          (Printf.sprintf "Method %S with signature %S on interface %S doesn't exist"
             member (string_of_signature (type_of_sequence message.body)) interface)
    | None ->
        OBus_error.Unknown_method
          (Printf.sprintf "Method %S with signature %S doesn't exist"
             member (string_of_signature (type_of_sequence message.body)))

(* Call the name owner change handler of a name resolver. [init] tell
   wether it is for initialization purpose or not. If it is and it has
   already been initialized then the call is discarded. *)
let call_resolver_handler ?(init=false) name_change_callback owner =
  if not init || not name_change_callback.ncc_initialized then begin
    (* The handler is now initialized *)
    name_change_callback.ncc_initialized <- true;

    callback_apply "resolver callback" name_change_callback.ncc_callback owner
  end
