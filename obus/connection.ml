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

module SMap = Map.Make(struct type t = string let compare = compare end)
module SSet = Set.Make(struct type t = string let compare = compare end)
module IMap = Map.Make(struct type t = int32 let compare = compare end)

module SerialMap = IMap
module InterfMap = SMap
module ObjectSet = SSet

type guid = Address.guid

type 'a reader = Header.recv -> string -> int -> 'a
type writer = byte_order -> string -> int -> int

type waiting_mode =
  | Wait_sync of Mutex.t
      (* The thread is blocked until the reply came *)
  | Wait_async
      (* The reply will stored somewhere and the caller will get it
         later *)

type body = Values.values
type filter = recv -> body Lazy.t -> bool

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

let align i = i + ((8 - i) land 7)

let read_one_message transport buffer =
  (* Read the minimum for knowing the total size of the message *)
  assert (transport.Transport.recv !buffer 0 16 = 16);
  (* We immediatly look for the byte order, then read the header *)
  let (constant_part_reader,
       fields_reader,
       byte_order) = match !buffer.[0] with
    | 'l' -> (HeaderRW.read_constant_part_le,
              HeaderRW.read_fields_le,
              Little_endian)
    | 'B' -> (HeaderRW.read_constant_part_be,
              HeaderRW.read_fields_be,
              Big_endian)
    | _ -> raise (Wire.Content_error "invalid byte order")
  in
  let (message_type,
       flags,
       length,
       fields_length) = constant_part_reader !buffer in
    (* Header fields array start on byte #16 and message start aligned
       on a 8-boundary after it, so we have: *)
  let total_length = align fields_length + length in
    (* Safety checking *)
    if fields_length > Constant.max_array_size then
      raise Wire.Reading.Array_too_big;
    if total_length > Constant.max_message_size then
      raise Invalid_argument "message too big";

    assert (transport.Transport.recv !buffer 0 total_length = total_length);

    let i, fields = fields_reader !buffer fields_length in
      ({ byte_order = byte_order;
         message_type = message_type;
         flags = flags;
         serial = serial;
         length = length;
         fields = fields }, align i)

let write_one_message transport buffer header serial body_writer =
  let (constant_part_writer,
       fields_writer,
       byte_order_code) =
    match header.byte_order with
      | Little_endian -> (HeaderRW.write_constant_part_le,
                          HeaderRW.write_fields_le,
                          'l')
      | Big_endian -> (HeaderRW.write_constant_part_be,
                       HeaderRW.write_fields_be,
                       'B')
  in
  let i, new_buffer = fields_writer !buffer header.fields in
  let length, new_buffer = body_writer new_buffer (align i) in
    constant_part_writer new_buffer
      header.message_type
      header.flags
      header.serial
      length;
    buffer := new_buffer

let write_message connection header serial body_writer =
  Mutex.lock connection.outgoing_buffer_m;
  try
    write_one_message connection.transport connection.outgoing_buffer header serial body_writer;
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

let read_values header byte_order buffer ptr =
  let t = (match header.fields.Header.signature with
             | Some s -> Values.dtypes_of_signature s
             | _ -> []) in
    match header.byte_order with
      | Little_endian -> Values.read_values_le buffer ptr
      | Big_endian -> Values.read_values_be buffer ptr

let write_values body byte_order buffer ptr = match byte_order with
  | Little_endian -> Values.write_values_le buffer ptr body
  | Big_endian -> Values.write_values_be buffer ptr body

let send_message_async connection (header, body) f =
  raw_send_message_async connection header (write_values body)
    (fun header buffer ptr -> f (header, read_values header buffer ptr))
let send_message_no_reply connection (header, body) =
  raw_send_message_no_reply connection header (write_values body)
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
  let header, body_start = read_one_message connection.transport connection.incoming_buffer in
    (* The body is a lazy value so it is computed at most one time *)
  let body = Lazy.lazy_from_fun (fun () -> Values.read_values header !(connection.incoming_buffer) body_start) in
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
    (* Write errors on stderr *)
    if Log.Verbose.connection && header.message_type = Error
    then begin
      let msg = match header.fields.signature with
        | Some "s" -> fst (Values.get_values (Values.cons Values.string Values.nil) (Lazy.force body))
        | _ -> ""
      and name = match header.fields.error_name with
        | Some name -> name
        | _ -> ""
      in
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
        let connection = match shared with
          | true -> make ()
          | false ->
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
  | true -> of_transport (Transport.of_addresses addresses) true
  | false ->
      (* Try to find an guid that we already have *)
      let guids = Util.filter_map (fun (_, _, g) -> g) addresses in
        Util.with_mutex guid_connection_table_m begin fun () ->
          match Util.find_map find_guid guids with
            | Some(connection) -> connection
            | None -> of_transport (Transport.of_addresses addresses) false
        end

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
    (write_values body) (fun header buffer ptr -> (header, read_values header buffer ptr))
