(*
 * connection.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module I = Interface

module SMap = Map.Make(struct type t = string let compare = compare end)
module SSet = Set.Make(struct type t = string let compare = compare end)
module IMap = Map.Make(struct type t = int32 let compare = compare end)

module GuidMap = SMap
module SerialMap = IMap
module InterfMap = SMap
module ObjectSet = SSet

type guid = Auth.guid

type 'a reader = Header.t -> string -> int -> 'a
type writer = Header.t -> stirng -> int -> unit

type waiting_mode =
  | Wait_sync of Mutex.t
      (* The thread is blocked until the reply came *)
  | Wait_async
      (* The reply will stored somewhere and the caller will get it
         later *)

type filter = Header.t -> Value.t Lazy.t -> bool

type any_filter =
  | User of filter
  | Raw of bool reader
      (* We make a difference between user and raw filter because user
         filters receive an unmarshaled message, so we do not want to
         unmarshal it several times. Raw filters unmarshal the message
         only if they have to and do not unmarshal it the same
         way... *)

type t = {
  (* Transport used for the connection *)
  transport : Transport.t;

  (* Incomming buffer, it is not protected by a mutex because there is
     only one thread using it. We use a reference because the message
     marshaler/unmarshaler can grow it. *)
  incoming_buffer : string ref;

  (* Outgoing buffer *)
  outgoing_buffer : string ref;
  outgoing_buffer_m : Mutex.t;

  (* Next available serial for sending message *)
  next_serial : Header.serial Protected.t;

  (* The server guid *)
  guid : guid;

  (* Message filters *)
  filters : (any_filter * wait_mode) list Protected.t;

  (* There are always one predefined filter which handle method call
     and reply, it use these structure for that: *)
  serial_waiters : (unit reader * waiting_mode) SerialMap.t Protected.t;
  (* Note: once a reply came, the waiter is removed *)
  interfaces : Interface.handlers InterfMap.t Protected.t;
}

(* Mapping from server guid to connection *)
let guid_connection_map = guid GuidMap.t Protected.t

(* Remove a connection from the mapping, for when a connection is
   being garbage collected *)
let remove_connection connection =
  Protected.update (GuidMap.remove connection.guid) guid_connection_map

type message = Header.t * Value.t

let gen_serial connection = Protected.process (fun x -> (x, Int32.succ c)) connection.next_serial

let write_message connection header body_writer =
  Mutex.lock connection.outgoing_buffer_m;
  try
    Message.write connection.transport connection.outgoing_buffer_m header body_writer;
    Mutex.unlock connection.outgoing_buffer_m
  with
      e ->
        Mutex.unlock connection.outgoing_buffer_m;
        raise e

let raw_send_message_sync connection header body_writer body_reader =
  let m = Mutex.create () in
    Mutex.lock m;
    Protected.update  (SerialMap.add header.H.serial (body_reader, Wait_sync m)) connection.serial_waiters;
    write_message connection header body_writer;
    Mutex.lock m

let raw_send_message_async connection header body_writer body_reader =
  Protected.update (SerialMap.add header.H.serial (body_reader, Wait_async)) connection.serial_waiters;
  write_message connection header body_writer

let raw_send_message_no_reply connection header body_writer =
  write_message connection header body_writer

let raw_add_filter connection reader =
  Protected.update (fun l -> Raw(reader) :: l) connection.filters;

let send_message_sync connection (header, body) =
  let v = ref [] in
    raw_send_message_sync connection header
      (Wire.write_value body) (fun buffer ptr -> v := Wire.Wire.read_value buffer ptr);
    !v
let send_message_async connection (header, body) f =
  raw_send_message_async connection header (Wire.write_value body)
    (fun buffer ptr -> f (Wire.read_value buffer ptr))
let send_message_no_reply connection (header, body) =
  raw_send_message_no_reply connection header (Wire.write_value body)
let add_filter connection filter =
  Protected.update (fun l -> User(filter) :: l) connection.filters
let add_interface connection interface =
  Protected.update (fun m -> InterfMap.add (I.get_handlers interface) interface m) connection.interfaces

let wakeup = function
  | Wait_sync(m) ->
      (* We wake up the thread by unlocking his mutex *)
      Mutex.unlock m;
  | Wait_async ->
      (* Nothing to do *)
      ()

(* Read and wakeup a thread *)
let read connection (reader, mode) header body_start =
  reader header !(connection.incoming_buffer) body_start;
  wakeup mode

open Header

let internal_dipastch connection =
  let header, body_start = Message.read connection.transport connection.incoming_buffer in
    (* The body is a lazy value so it is computed at most one time *)
  let body = Lazy.lazy_from_fun (fun () -> read_value header body_start) in
  let rec aux = function
    | (filter, mode) :: l ->
        try
          let handled = begin match filter with
            | User filter -> filter header body
            | Raw reader -> reader header !(connection.incoming_buffer) body_start
          end in
            wakeup mode;
            if handled
            then ()
            else aux l
        with
            _ ->
              (* We can not do anything if the filter raise an exception.. *)
              aux l
    | [] ->
        (* The message has not been handled, try with internal dispatching *)
        match header.message_type, header.fields with
          | Method_return, { reply_serial = Some(serial) }
          | Error, { reply_serial = Some(serial) } ->
              begin match Protected.update connection.serial_waiters begin fun map ->
                try
                  let waiter = SerialMap.find serial map in
                    (Some(waiter), SerialMap.remove serial map)
                with
                    Not_found -> (None, map)
              end with
                | Some w -> read connection w header body_start
                | None -> ()
              end
          | Method_call, { interface = Some(interface) } ->
              begin try
                ingore (read connection
                          (InterfMap.find interface
                             (Protected.get connection.interfaces)).I.method_call
                          header body_start)
              with
                  Not_found ->
                    (* XXX TODO: send an error message XXX *)
                    ()
              end
          | Method_call, { interface = None } ->
              (* Specification say we must handle this case, so lets try
                 with every interfaces... *)
              begin try
                InterfMap.iter begin fun _ interface ->
                  try
                    if reader connection interface.I.method_call header body_start then
                      raise Exit
                  with
                      _ -> ()
                end (Protected.get connection.interfaces);
              with Exit -> ()
              end
  in
    (* Try all the filters *)
    aux (Protected.get connection.filters)

let dispatch connection =
  if ThreadConfig.use_threads
  then ()
  else internal_dispatch connection

let transport connection = connection.transport
let guid connection = connection.guid

let of_transport transport priv =
  let guid = Auth.launch (transport.Transport.lexbuf ()) in
  let make () =
    {
      transport = transport;
      incoming_buffer = String.create 65536;
      outgoing_buffer = Protected.make (String.create 65536);
      next_serial = Protected.make Int32.zero;
      guid = guid
    }
  in
  let connection = match priv with
    | true -> make ()
    | false ->
        Protected.safe_process begin fun m -> try
          (GuidMap.find guid m, m)
        with
            Not_found ->
              let connection = make () in
                (try Gc.finalize remove_connection connection with _ -> ());
                (connection, GuidMap.add guid m)
        end guid_connetion_map
  in
    (* Launch dispatcher *)
    Thread.create (fun () -> while true do internal_dispatch connection done) ()

let of_addresses addresses =
  let transport = Util.find begin fun addr ->
    try
      match Transport.create addr with
        | None -> None
        | Some(transport) -> Some(of_transport transport)
    with
        _ -> None
  end addresses
