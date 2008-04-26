(*
 * connection.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Header
module I = Interface

module SMap = Map.Make(struct type t = string let compare = compare end)
module SSet = Set.Make(struct type t = string let compare = compare end)
module IMap = Map.Make(struct type t = int32 let compare = compare end)

module GuidMap = SMap
module SerialMap = IMap
module InterfMap = SMap
module ObjectSet = SSet

type guid = Address.guid

type 'a reader = Header.recv Header.t -> string -> int -> 'a
type writer = byte_order -> string -> int -> int

type waiting_mode =
  | Wait_sync of Mutex.t
      (* The thread is blocked until the reply came *)
  | Wait_async
      (* The reply will stored somewhere and the caller will get it
         later *)

type body = Val.value list
type filter = recv Header.t -> body Lazy.t -> bool

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
  next_serial : serial Protected.t;

  (* The server guid *)
  guid : guid;

  (* Message filters *)
  filters : (any_filter * waiting_mode) list Protected.t;

  (* There are always one predefined filter which handle method call
     and reply, it use these structure for that: *)
  serial_waiters : (unit reader * waiting_mode) SerialMap.t Protected.t;
  (* Note: once a reply came, the waiter is removed *)
  interfaces : Interface.handlers InterfMap.t Protected.t;
}

(* Mapping from server guid to connection *)
let guid_connection_map = Protected.make GuidMap.empty

(* Remove a connection from the mapping, for when a connection is
   being garbage collected *)
let remove_connection connection =
  Protected.update (GuidMap.remove connection.guid) guid_connection_map

type 'a message = 'a Header.t * body

let gen_serial connection = Protected.process (fun x -> (x, Int32.succ x)) connection.next_serial

let write_message connection header serial body_writer =
  Mutex.lock connection.outgoing_buffer_m;
  try
    MessageRW.write connection.transport connection.outgoing_buffer header serial body_writer;
    Mutex.unlock connection.outgoing_buffer_m
  with
      e ->
        Mutex.unlock connection.outgoing_buffer_m;
        raise e

let raw_send_message_async connection header body_writer body_reader =
  let serial = gen_serial connection in
    Protected.update (SerialMap.add serial (body_reader, Wait_async)) connection.serial_waiters;
    write_message connection header serial body_writer

let raw_send_message_no_reply connection header body_writer =
  let serial = gen_serial connection in
    write_message connection header serial body_writer

let raw_add_filter connection reader =
  Protected.update (fun l -> (Raw(reader), Wait_async) :: l) connection.filters

let send_message_async connection (header, body) f =
  raw_send_message_async connection header (Val.write_value body)
    (fun header buffer ptr -> f (header, Val.read_value header buffer ptr))
let send_message_no_reply connection (header, body) =
  raw_send_message_no_reply connection header (Val.write_value body)
let add_filter connection filter =
  Protected.update (fun l -> (User(filter), Wait_async) :: l) connection.filters
let add_interface connection interface =
  Protected.update (fun m -> InterfMap.add (I.name interface) (I.get_handlers interface) m) connection.interfaces

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

let internal_dispatch connection =
  let header, body_start = MessageRW.read connection.transport connection.incoming_buffer in
    (* The body is a lazy value so it is computed at most one time *)
  let body = Lazy.lazy_from_fun (fun () -> Val.read_value header !(connection.incoming_buffer) body_start) in
  let rec aux = function
    | (filter, mode) :: l ->
        begin try
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
        end
    | [] ->
        (* The message has not been handled, try with internal dispatching *)
        match header.message_type, header.fields with
          | Method_return, { reply_serial = Some(serial) }
          | Error, { reply_serial = Some(serial) } ->
              begin match (Protected.process begin fun map ->
                             try
                               let waiter = SerialMap.find serial map in
                                 (Some(waiter), SerialMap.remove serial map)
                             with
                                 Not_found -> (None, map)
                           end connection.serial_waiters) with
                | Some w -> read connection w header body_start
                | None -> ()
              end
          | Method_call, { interface = Some(interface) } ->
              begin try
                ignore ((InterfMap.find interface
                           (Protected.get connection.interfaces)).I.method_call
                          header !(connection.incoming_buffer) body_start)
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
                    if interface.I.method_call header !(connection.incoming_buffer) body_start then
                      raise Exit
                  with
                      _ -> ()
                end (Protected.get connection.interfaces);
              with Exit -> ()
              end
          | _ ->
              (* Message dropped *)
              ()
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
  match Auth.launch transport with
    | None -> raise (Failure "cannot authentificate on the given transport")
    | Some(guid) ->
        let make () =
          {
            transport = transport;
            incoming_buffer = ref (String.create 65536);
            outgoing_buffer = ref (String.create 65536);
            outgoing_buffer_m = Mutex.create ();
            serial_waiters = Protected.make SerialMap.empty;
            interfaces = Protected.make InterfMap.empty;
            filters = Protected.make [];
            next_serial = Protected.make 1l;
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
                      (try Gc.finalise remove_connection connection with _ -> ());
                      (connection, GuidMap.add guid connection m)
              end guid_connection_map
        in
          (* Launch dispatcher *)
          ignore (Thread.create (fun () -> while true do internal_dispatch connection done) ());
          connection

let of_addresses addresses = function
  | true -> of_transport (Transport.of_addresses addresses) true
  | false ->
      (* Try to find an guid that we already have *)
      let guids = Util.filter_map (fun (_, _, g) -> g) addresses in
      let map = Protected.get guid_connection_map in
        match Util.find_map (Util.exn_to_opt (fun guid -> GuidMap.find guid map)) guids with
          | Some(connection) -> connection
          | None -> of_transport (Transport.of_addresses addresses) false

let rec wait_for_reply connection v = match !v with
  | None -> internal_dispatch connection; wait_for_reply connection v
  | Some x -> x

let raw_send_message_sync connection header body_writer body_reader =
  let serial = gen_serial connection in
  let m = Mutex.create () in
  let v = ref None in
    Mutex.lock m;
    Protected.update (SerialMap.add serial
                        ((fun header buffer ptr ->
                            v := Some(body_reader header buffer ptr)),
                         Wait_sync m)) connection.serial_waiters;
    write_message connection header serial body_writer;
    if ThreadConfig.use_threads
    then begin
      Mutex.lock m;
      match !v with
        | Some x -> x
        | None -> assert false
    end else
      wait_for_reply connection v

let send_message_sync connection (header, body) =
  raw_send_message_sync connection header
    (Val.write_value body) (fun header buffer ptr -> (header, Val.read_value header buffer ptr))
