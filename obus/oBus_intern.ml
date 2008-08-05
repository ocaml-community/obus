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

open OBus_header

let ($) a b = a b
let (|>) a b x = b (a x)
let (<|) a b x = a (b x)
let (>>) a b = Lwt.bind a $ fun _ -> b

module My_map(T : sig type t end) =
struct
  include Map.Make(struct type t = T.t let compare = compare end)

  let lookup key map =
    try
      Some(find key map)
    with
        Not_found -> None
end

module Serial_map = My_map(struct type t = serial end)
module Interf_map = My_map(struct type t = string end)
module Signal_map = My_map(struct type t = string * string end)

type body = OBus_value.sequence
type filter_id = int

let gen_signal_handler_key = let c = ref 0 in fun () -> incr c; !c

type reply_handler = method_return -> context -> OBus_types.signature -> int -> body Lazy.t -> unit
  (* Type of a reply handler. Since the signature can be seen as a
     dynamically typed value or can be read with a custom reader (with
     a type combinator) it receive all the necessary information to
     unmarshal the message + a lazy version of the body as a
     dynamically typed value. It is a lazy value because if several
     function want the message like that it will be unmarshaled only
     once. *)

and method_call_handler_result =
    (* Result of a method call handling *)
  | Mchr_no_such_method
      (* The method do not exists *)
  | Mchr_no_such_object
      (* The object do not exists *)
  | Mchr_ok of (context -> int -> unit)
      (* It know how to handle the method call, it must return a
         closure which when exectuted will unmarshal the message and
         launch a thread executing the function handling the call and
         sending the reply *)

and service_handler = method_call -> OBus_types.signature -> method_call_handler_result
  (* A service handler take the header of the call, the signature of
     the message and must lookup for if it know how to handle the
     call *)

and signal_handler = int * (Obj.t -> unit, unit, unit, unit) wire * (signal -> Obj.t) list
    (* Handling of signal is a bit different from the others. The
       reason is that the user can define multiple handler for the
       same signal. In this case we want to use the same version of
       the unmarshaled signal for all the handler.

       So an it contain

       - a key used to identify a reader
       - a reader used to read the message
       - a list of user-defined handlers
    *)

and filter = OBus_header.any -> body -> unit

and running_connection = {
  transport : OBus_transport.t;
  shared : bool;

  (* Unique name of the connection *)
  mutable name : string;

  (* The server guid *)
  guid : OBus_address.guid;

  (* The ougoing thread. To send a message we just have bind the
     result of this thread to the action of sending a message. *)
  mutable outgoing : (serial * string) Lwt.t;

  mutable next_filter_id : int;
  mutable filters : (filter_id * filter) list;
  mutable reply_handlers : (reply_handler * (exn -> unit)) Serial_map.t;
  mutable signal_handlers : signal_handler list Signal_map.t;
  mutable service_handlers : service_handler Interf_map.t;

  (* Handling of fatal errors *)
  on_disconnect : (exn -> unit) ref;
}

and connection_state =
  | Crashed of exn
      (* If the connection has crashed. *)
  | Running of running_connection

and connection = connection_state ref

and context = {
  connection : connection;
  bus_name : string option;
  byte_order : OBus_info.byte_order;
  buffer : string
}

and ('a, 'b, 'c, 'typ) wire = context -> int -> int * 'a

open Lwt

let lwt_with_running connection f = match !connection with
  | Crashed exn -> fail exn
  | Running running -> f running

let with_running connection f = match !connection with
  | Crashed exn -> raise exn
  | Running running -> f running

(* Do an IO operation, and verify before and after that the connection
   is OK *)
let wrap_io func connection buffer pos count =
  lwt_with_running connection
    (fun running -> func running.transport buffer pos count
       >>= fun result -> match !connection with
         | Crashed exn -> fail exn
         | _ -> return result)

let recv = wrap_io OBus_transport.recv
let send = wrap_io OBus_transport.send
let recv_exactly = wrap_io OBus_transport.recv_exactly
let send_exactly = wrap_io OBus_transport.send_exactly

(* The following function should be in [Wire] because they are needed
   by [OBus_wire] and [Wire_message] but the depend on the definition
   of context so they must be here to avoid circular dependencies
   problem *)

open Wire

let pad2 i = i land 1
let pad4 i = (4 - i) land 3
let pad8 i = (8 - i) land 7

let wpadn f ctx i =
  let count = f i in
    if i + count > String.length ctx.buffer then out_of_bounds ();
    for j = 0 to count - 1 do
      String.unsafe_set ctx.buffer (i + j) '\x00'
    done;
    (i + count, ())

let wpad2 = wpadn pad2
let wpad4 = wpadn pad4
let wpad8 = wpadn pad8

let rpadn f ctx i =
  let count = f i in
    if i + count > String.length ctx.buffer then out_of_bounds ();
    for j = 0 to count - 1 do
      if String.unsafe_get ctx.buffer (i + j) <> '\x00'
      then raise (Reading_error "unitialized padding bytes")
    done;
    (i + count, ())

let rpad2 = rpadn pad2
let rpad4 = rpadn pad4
let rpad8 = rpadn pad8
