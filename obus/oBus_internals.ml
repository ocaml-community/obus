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

module Serial_map = Util.Make_map(struct type t = serial end)
module Object_map = Util.Make_map(struct type t = OBus_path.t end)
module Name_map = Util.Make_map(struct type t = OBus_name.bus end)

type 'connection member_info =
  | MI_method of OBus_name.member * tsequence * ('connection -> method_call -> unit)
  | MI_signal
  | MI_property of OBus_name.member * (unit -> single Lwt.t) option * (single -> unit Lwt.t) option

type 'connection member_desc = OBus_introspect.declaration * 'connection member_info

class type ['connection] _dbus_object = object
  method obus_path : OBus_path.t
  method obus_handle_call : 'connection -> method_call -> unit
  method introspect : 'connection -> OBus_introspect.document Lwt.t
  method get : OBus_name.interface -> OBus_name.member -> OBus_value.single Lwt.t
  method set : OBus_name.interface -> OBus_name.member -> OBus_value.single -> unit Lwt.t
  method get_all : OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
  method obus_add_interface : OBus_name.interface -> 'connection member_desc list -> unit
  method obus_export : 'connection -> unit
  method obus_remove : 'connection -> unit
  method obus_destroy : unit
  method obus_connection_closed : 'connection -> unit
end

type filter = OBus_message.any -> OBus_message.any option

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
type name_resolver = {
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

type connection = {
  (* The backend transport used by the connection *)
  transport : OBus_lowlevel.transport;

  (* This tell weather we must shutdown the transport when the
     connection is closed by the programmer or by a crash *)
  shutdown_transport_on_close : bool ref;

  (* For client-side connection, if specified this means that the
     connection is shared and it is the guid of the server. *)
  guid : OBus_address.guid option;

  (* Set when the connection has crashed or has been closed, all
     functions will fail with this exception if it is set *)
  mutable crashed : exn option;

  (* up/down state. If [None] it means that the connection is up, if
     [Some w] it means that the connection is down, [w] being a
     waiting thread which will be wake up when the connection is set
     up *)
  mutable down : unit Lwt.t option;

  (* [abort] is a waiting thread which is wakeup when the connection
     is closed or aborted. It is used to make the dispatcher to
     exit. *)
  abort : any Lwt.t;

  (* [watch = abort >>= fun _ -> return ()], this is the value
     returned by [OBus_connection.watch] *)
  watch : unit Lwt.t;

  (* Unique name of the connection. If set this means that the other
     side is a message bus. *)
  mutable name : OBus_name.bus option;

  (* The ougoing thread.

     If a message is being sent it is a waiting thread which is wakeup
     when the message is sent. It return the current serial. *)
  mutable outgoing : serial Lwt.t;

  (* Association bus-name <-> owner *)
  mutable name_mapping : name_resolver Name_map.t;

  (* Cache of bus names of peer which has exited. It is used by
     [OBus_resolver] to minimize the number of request to the message
     bus *)
  mutable exited_peers : OBus_name.bus Cache.t;

  signal_receivers : signal_receiver MSet.t;

  (* Mapping serial -> thread waiting for a reply *)
  mutable reply_waiters : method_return Lwt.t Serial_map.t;

  (* Objects which are exported on a connection, and available to
     other applications *)
  mutable exported_objects : dbus_object Object_map.t;

  incoming_filters : filter MSet.t;
  outgoing_filters : filter MSet.t;

  (* [on_disconnect] is called when the connection is disconnect. This
     can happen is receiving a message on the transport fail, or if a
     failure happen while a message is being sent. *)
  on_disconnect : (exn -> unit) ref;
}

and dbus_object = connection _dbus_object

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
     is relative to the previous.

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

let is_bus = function
  | { name = Some _ } -> true
  | _ -> false

let lwt_with_running f connection = match connection.crashed with
  | Some exn -> fail exn
  | None -> f connection

let with_running f connection = match connection.crashed with
  | Some exn -> raise exn
  | None -> f connection

let with_bus f = with_running
  (fun connection -> match connection.name with
     | Some _ -> f connection
     | None -> ())

let lwt_with_bus f = lwt_with_running
  (fun connection -> match connection.name with
     | Some _ -> f connection
     | None -> return ())

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

let children connection path = with_running
  (fun connection ->
     Object_map.fold (fun p obj acc -> match OBus_path.after path p with
                        | Some(elt :: _) -> if List.mem elt acc then acc else elt :: acc
                        | _ -> acc) connection.exported_objects [])
  connection

(* Call the name owner change handler of a name resolver. [init] tell
   wether it is for initialization purpose or not. If it is and it has
   already been initialized then the call is discarded. *)
let call_resolver_handler ?(init=false) name_change_callback owner =
  if not init || not name_change_callback.ncc_initialized then begin
    (* The handler is now initialized *)
    name_change_callback.ncc_initialized <- true;

    callback_apply "resolver callback" name_change_callback.ncc_callback owner
  end

(* Update a name resolver. Called by the dispatcher when a
   NameOwnerChange signal is received or initially by
   [OBus_resolver] *)
let update_resolver connection name owner =
  if OBus_name.is_unique name && owner = None then
    (* If the resovler was monitoring a unique name and it is not
       owned anymore, this means that the peer with this name has
       exited. We remember this information here. *)
    Cache.add connection.exited_peers name;

  begin match Name_map.lookup name connection.name_mapping with
    | Some nr ->
        Log.debug "updating internal name resolver: %S -> %S" name (match owner with
                                                                      | Some n -> n
                                                                      | None -> "");
        nr.nr_owner := owner;

        if not nr.nr_initialized then begin
          (* The resolver has not yet been initialized; this means
             that the reply to GetNameOwner (done by
             [OBus_resolver.make]) has not yet been received. We
             consider that this first signal has precedence and
             terminate initialization. *)
          nr.nr_initialized <- true;

          (* Wakeup threads waiting for initialization *)
          Lwt.wakeup nr.nr_init ()
        end;

        MSet.iter (fun ncc -> call_resolver_handler ncc owner) nr.nr_on_change
    | None ->
        ()
  end
