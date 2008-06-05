(*
 * connection.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open ThreadImplem

open Header
module I = Interface

module SerialMap = Map.Make(struct type t = serial let compare = compare end)
module InterfMap = Map.Make(struct type t = string let compare = compare end)

type guid = Address.guid

type 'a reader = Header.recv -> string -> int -> 'a
type writer = string -> int -> int

type body = Values.values
type filter = recv -> body Lazy.t -> bool

type filter_desc =
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
     only one thread using it. It is mutable because it can grow if
     needed. *)
  mutable incoming_buffer : string;

  (* Outgoing buffer, can grow too *)
  outgoing_buffer : string Protected.t;

  (* Next available serial for sending message *)
  next_serial : serial Protected.t;

  (* The server guid *)
  guid : guid;

  (* Message filters *)
  filters : filter_desc list Protected.t;

  (* There is always one predefined filter which handle method call
     and reply, it use these structure for that: *)
  serial_waiters : unit reader SerialMap.t Protected.t;
  (* Note: once a reply came, the waiter is removed *)
  interfaces : Interface.handlers InterfMap.t Protected.t;
}

type connection = t

(* Mapping from server guid to connection. *)
module GuidTable = Weak.Make
  (struct
     type t = connection
     let equal x y = x.guid = y.guid
     let hash x = Hashtbl.hash x.guid
   end)
let guid_connection_table = GuidTable.create 4
let guid_connection_table_m = Mutex.create ()

let find_guid guid =
  GuidTable.fold begin fun connection acc -> match acc with
    | None -> if connection.guid = guid then Some(connection) else None
    | x -> x
  end guid_connection_table None

type send_message = Header.send * body
type recv_message = Header.recv * body

let gen_serial connection = Protected.process (fun x -> (x, Int32.succ x)) connection.next_serial

let write_message connection header serial body_writer =
  Protected.safe_update
    (fun buffer ->
       WireMessage.send_one_message connection.transport buffer header serial body_writer)
    connection.outgoing_buffer

let raw_send_message_async connection header body_writer body_reader =
  let serial = gen_serial connection in
    Protected.update (SerialMap.add serial body_reader) connection.serial_waiters;
    write_message connection header serial body_writer

let raw_send_message_no_reply connection header body_writer =
  let serial = gen_serial connection in
    write_message connection header serial body_writer

let raw_add_filter connection reader =
  Protected.update (fun l -> Raw(reader) :: l) connection.filters

let read_values raise_exn header buffer ptr =
  if raise_exn && header.message_type = Error
  then
    Error.raise_error header buffer ptr
  else
    let ts = (match header.fields.signature with
                | Some s -> Values.dtypes_of_signature s
                | _ -> []) in
      match header.byte_order with
        | Wire.Little_endian -> snd (Values.LEReader.read_values ts buffer ptr)
        | Wire.Big_endian -> snd (Values.BEReader.read_values ts buffer ptr)

let write_values body byte_order buffer ptr = match byte_order with
  | Wire.Little_endian -> Values.LEWriter.write_values buffer ptr body
  | Wire.Big_endian -> Values.BEWriter.write_values buffer ptr body

let send_message_async connection (header, body) f =
  raw_send_message_async connection header (write_values body header.byte_order)
    (fun header buffer ptr -> f (header, read_values false header buffer ptr))
let send_message_no_reply connection (header, body) =
  raw_send_message_no_reply connection header (write_values body header.byte_order)
let add_filter connection filter =
  Protected.update (fun l -> User(filter) :: l) connection.filters
let add_interface connection interface =
  Protected.update (fun m -> InterfMap.add (I.name interface) (I.get_handlers interface) m) connection.interfaces

let internal_dispatch connection =
  let header, buffer, body_start = WireMessage.recv_one_message connection.transport connection.incoming_buffer in
    connection.incoming_buffer <- buffer;
    (* The body is a lazy value so it is computed at most one time *)
    let body = Lazy.lazy_from_fun (fun () -> read_values false header buffer body_start) in
    let rec aux = function
      | filter :: l ->
          begin try
            let handled = begin match filter with
              | User filter -> filter header body
              | Raw reader -> reader header buffer body_start
            end in
              if handled
              then ()
              else aux l
          with
              _ ->
                (* We can not do anything if the filter raise an
                   exception. *)
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
                  | Some reader -> reader header buffer body_start
                  | None -> ()
                end
            | Method_call, { interface = Some(interface) } ->
                begin try
                  ignore ((InterfMap.find interface
                             (Protected.get connection.interfaces)).I.method_call
                            header buffer body_start)
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
                      if interface.I.method_call header buffer body_start then
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
      (* Write errors on stderr *)
      if Log.Verbose.connection && header.message_type = Error
      then begin
        let name, msg = Error.get_error header buffer body_start in
        let msg = match msg with Some s -> s | None -> "" in
          LOG("error received: %s: %s" name msg)
      end;
      (* Try all the filters *)
      aux (Protected.get connection.filters)

let dispatch connection =
  if ThreadConfig.use_threads
  then ()
  else internal_dispatch connection

let transport connection = connection.transport
let guid connection = connection.guid

let of_transport ?(shared=true) transport =
  match Auth.launch transport with
    | None -> raise (Failure "cannot authentificate on the given transport")
    | Some(guid) ->
        let make () =
          {
            transport = transport;
            incoming_buffer = String.create 65536;
            outgoing_buffer = Protected.make (String.create 65536);
            serial_waiters = Protected.make SerialMap.empty;
            interfaces = Protected.make InterfMap.empty;
            filters = Protected.make [];
            next_serial = Protected.make 1l;
            guid = guid
          }
        in
        let connection = match shared with
          | false -> make ()
          | true ->
              Util.with_mutex guid_connection_table_m begin fun () ->
                match find_guid guid with
                  | Some(connection) ->
                      connection
                  | None ->
                      let connection = make () in
                        GuidTable.add guid_connection_table connection;
                        connection
              end
        in
          (* Launch dispatcher *)
          ignore (Thread.create (fun () -> while true do internal_dispatch connection done) ());
          connection

let of_addresses ?(shared=true) addresses = match shared with
  | false -> of_transport (Transport.of_addresses addresses) ~shared:false
  | true ->
      (* Try to find an guid that we already have *)
      let guids = Util.filter_map (fun (_, _, g) -> g) addresses in
        match
          Util.with_mutex guid_connection_table_m
            (fun () -> Util.find_map find_guid guids)
        with
          | Some(connection) -> connection
          | None ->
              (* We ask again a shared connection even if we know that
                 there is no other connection to a server with the
                 same guid, because between the two lookup another
                 thread can add a new connection. *)
              of_transport (Transport.of_addresses addresses) ~shared:true

(* Handling of synchronous call.

   They are just implemented as asynchronous call which store the
   reply into a shared reference and wake up that emit the method
   call *)

type 'a sync_result =
  | Waiting
      (* The reply has not already come *)
  | Val of 'a
      (* The reply come and this is the readed value *)
  | Exn of exn
      (* An exception has been raised during the reading of the message *)

(* If not using threads we must do dispatching until the reply come. *)
let rec wait_for_reply connection result = match !result with
  | Waiting -> internal_dispatch connection; wait_for_reply connection result
  | Val x -> x
  | Exn exn -> raise exn

(* This handle a reply for a synchronous call *)
let handle_reply_for_sync result_storage sleep_mutex body_reader header buffer ptr =
  (* This store the result to let the calling thread find the value *)
  result_storage :=
    (try
       Val(body_reader header buffer ptr)
     with
         exn -> Exn exn);
  (* This wakeup the calling thread *)
  Mutex.unlock sleep_mutex

let raw_send_message_sync connection header body_writer body_reader =
  let sleep_mutex = Mutex.create () in
  let result = ref Waiting in
    Mutex.lock sleep_mutex;
    raw_send_message_async connection header body_writer
      (handle_reply_for_sync result sleep_mutex body_reader);
    if ThreadConfig.use_threads
    then begin
      Mutex.lock sleep_mutex;
      match !result with
        | Exn exn -> raise exn
        | Val x -> x
            (* This never happen since the only way to unlock the
               sleep_mutex is to read the message *)
        | Waiting -> assert false
    end else
      wait_for_reply connection result

let send_message_sync connection ?(raise_exn=true) (header, body) =
  raw_send_message_sync connection header
    (write_values body header.byte_order)
    (fun header buffer ptr -> (header, read_values raise_exn header buffer ptr))
