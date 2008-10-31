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

module My_map(T : sig type t end) =
struct
  include Map.Make(struct type t = T.t let compare = compare end)

  let lookup key map =
    try
      Some(find key map)
    with
        Not_found -> None
end

(***** Connection *****)

module Serial_map = My_map(struct type t = serial end)
module Object_map = My_map(struct type t = OBus_path.t end)

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
  mutable name : OBus_name.unique option;

  (* The ougoing thread.

     If a message is being sent it is a waiting thread which is wakeup
     when the message is sent. It return the current serial. *)
  mutable outgoing : serial Lwt.t;

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
  sr_sender : OBus_name.unique option;
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

  sr_handler : connection -> signal -> unit;
}

(***** Utils *****)

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
