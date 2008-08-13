(*
 * wireMessage.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open Wire
open OBus_header
open OBus_internals
open OBus_value
open OBus_info
open Wire

type recv = {
  recv_header : OBus_header.any;
  recv_signature : OBus_value.signature;
  recv_byte_order : byte_order;
  recv_body_start : int;
  recv_length : int;
  recv_buffer : string;
}

type 'a send = {
  send_header : 'a OBus_header.t;
  send_signature : OBus_value.signature;
  send_writer : writer;
  send_byte_order : byte_order;
  send_serial : int32;
}

(* Raw description of an header *)

type raw_fields = {
  _path : path option;
  _member : member option;
  _interface : interface option;
  _error_name : error_name option;
  _reply_serial : reply_serial option;
  _destination : string option;
  _sender : string option;
  _signature : signature;
}

let empty_fields = {
  _path = None;
  _member = None;
  _interface = None;
  _error_name = None;
  _reply_serial = None;
  _destination = None;
  _sender = None;
  _signature = [];
}

let path = ("path", fun x -> x._path)
let member = ("member", fun x -> x._member)
let interface = ("interface", fun x -> x._interface)
let error_name = ("error_name", fun x -> x._error_name)
let reply_serial = ("reply_serial", fun x -> x._reply_serial)

let get_required message_type_name (field_name, get_field) fields =
  match get_field fields with
    | Some v -> v
    | None -> raise (Reading_error
                       (Printf.sprintf "invalid header, field '%s' is required for '%s'"
                          field_name message_type_name))

let method_call_of_raw fields =
  let req x = get_required "method_call" x in
    `Method_call(req path fields,
                 fields._interface,
                 req member fields)

let method_return_of_raw fields =
  let req x = get_required "method_return" x in
    `Method_return(req reply_serial fields)

let error_of_raw fields =
  let req x = get_required "error" x in
    `Error(req reply_serial fields,
           req error_name fields)

let signal_of_raw fields =
  let req x = get_required "signal" x in
    `Signal(req path fields,
            req interface fields,
            req member fields)

let read_fields fields_length ctx i =
  let i, fields = read_until
    (fun acc ctx i ->
       let i = rpad8 ctx i in
       let i, v = ruint8 ctx i in
       (match v with
          | 1 -> rfixed (Tbasic Tobject_path) (rwrap robject_path & fun x -> { acc with _path = Some x })
          | 2 -> rfixed (Tbasic Tstring) (rwrap rstring & fun x -> {  acc with _interface = Some x })
          | 3 -> rfixed (Tbasic Tstring) (rwrap rstring & fun x -> { acc with _member = Some x })
          | 4 -> rfixed (Tbasic Tstring) (rwrap rstring & fun x -> { acc with _error_name = Some x })
          | 5 -> rfixed (Tbasic Tuint32) (rwrap ruint32 & fun x -> { acc with _reply_serial = Some x })
          | 6 -> rfixed (Tbasic Tstring) (rwrap rstring & fun x -> { acc with _destination = Some x })
          | 7 -> rfixed (Tbasic Tstring) (rwrap rstring & fun x -> { acc with _sender = Some x })
          | 8 -> rfixed (Tbasic Tsignature) (rwrap rsignature & fun x -> { acc with _signature = x })
          | n -> rwrap rvariant (fun _ -> acc)) ctx i)
    empty_fields fields_length ctx i in
  (rpad8 ctx i, fields)

module Reader(BO : Byte_order) =
struct
  module R = Make_unsafe_reader(BO)
  open R

  let read connection buffer =
    let protocol_version = unsafe_read_byte_as_int buffer 3 in
    (* Check the protocol version first, since we can not do
       anything if it is not the same as our *)
    if protocol_version <> OBus_info.protocol_version
    then raise (Reading_error (sprintf "invalid protocol version: %d" protocol_version));

    let message_maker = let code = unsafe_read_byte_as_int buffer 1 in
    match code with
      | 1 -> method_call_of_raw
      | 2 -> method_return_of_raw
      | 3 -> error_of_raw
      | 4 -> signal_of_raw
      | n -> raise (Reading_error (sprintf "unknown message type: %d" n))

    and flags =
      let n = unsafe_read_byte_as_int buffer 2 in
      { no_reply_expected = n land 1 = 1;
        no_auto_start = n land 2 = 2 }

    and length = unsafe_read_uint32_as_int buffer 4
    and serial = unsafe_read_uint32_as_int32 buffer 8
    and fields_length = unsafe_read_uint32_as_int buffer 12 in

    (* Header fields array start on byte #16 and message start
       aligned on a 8-boundary after it, so we have: *)
    let total_length = 16 + fields_length + (pad8 fields_length) + length in
    (* Safety checking *)
    rcheck_array_len fields_length;

    if total_length > OBus_info.max_message_size
    then raise (Reading_error (sprintf "message size exceed the limit: %d" total_length));

    Lwt.bind
      (recv_exactly connection buffer 0 (total_length - 16))
      (fun _ ->
         try
           let ptr, fields = read_fields fields_length { connection = connection;
                                                         byte_order = BO.byte_order;
                                                         bus_name = None;
                                                         buffer = buffer } 0 in
           Lwt.return { recv_header = { flags = flags;
                                        sender = fields._sender;
                                        destination = fields._destination;
                                        serial = serial;
                                        typ = message_maker fields };
                        recv_signature = fields._signature;
                        recv_byte_order = BO.byte_order;
                        recv_body_start = ptr;
                        recv_buffer = buffer;
                        recv_length = length }
         with exn -> Lwt.fail exn)
end

module Writer(BO : Byte_order) =
struct
  module W = Make_unsafe_writer(BO)
  open W

  let write connection send buffer =
    let header = send.send_header in
    let code, fields = match header.typ with
      | `Method_call(path, interface, member) ->
          (1,
           { _path = Some path;
             _interface = interface;
             _member = Some member;
             _error_name = None;
             _reply_serial = None;
             _destination = header.destination;
             _sender = header.sender;
             _signature = send.send_signature })
      | `Method_return(reply_serial) ->
          (2,
           { _path = None;
             _interface = None;
             _member = None;
             _error_name = None;
             _reply_serial = Some reply_serial;
             _destination = header.destination;
             _sender = header.sender;
             _signature = send.send_signature })
      | `Error(reply_serial, error_name) ->
          (3,
           { _path = None;
             _interface = None;
             _member = None;
             _error_name = Some error_name;
             _reply_serial = Some reply_serial;
             _destination = header.destination;
             _sender = header.sender;
             _signature = send.send_signature })
      | `Signal(path, interface, member) ->
          (4,
           { _path = Some path;
             _interface = Some interface;
             _member = Some member;
             _error_name = None;
             _reply_serial = None;
             _destination = header.destination;
             _sender = header.sender;
             _signature = send.send_signature })
    in
    unsafe_write_char_as_byte BO.byte_order_char buffer 0;
    unsafe_write_int_as_byte code buffer 1;
    unsafe_write_int_as_byte
      ((if header.flags.no_reply_expected then 1 else 0) lor
         (if header.flags.no_auto_start then 2 else 0)) buffer 2;
    unsafe_write_int_as_byte OBus_info.protocol_version buffer 3;
    unsafe_write_int32_as_uint32 send.send_serial buffer 8;

    let wfield code typ writer field ctx i = match field with
      | None -> i
      | Some v ->
          let i = wpad8 ctx i in
          let i = wuint8 code ctx i in
          wfixed (Tbasic typ) (writer v) ctx i in
    let ctx = { connection = connection;
                bus_name = header.destination;
                byte_order = BO.byte_order;
                buffer = buffer } in
    let i = wfield 1 Tobject_path wobject_path fields._path ctx 12 in
    let i = wfield 2 Tstring wstring fields._interface ctx i in
    let i = wfield 3 Tstring wstring fields._member ctx i in
    let i =  wfield 4 Tstring wstring fields._error_name ctx i in
    let i =  wfield 5 Tuint32 wuint32 fields._reply_serial ctx i in
    let i =  wfield 6 Tstring wstring fields._destination ctx i in
    let i =  wfield 7 Tstring wstring fields._sender ctx i in
    let i =  wfield 8 Tsignature wsignature (Some fields._signature) ctx i in
    let len = i - 16 in
    wcheck_array_len len;
    unsafe_write_int_as_uint32 len buffer 12;
    let i = wpad8 ctx i in
    let j = send.send_writer ctx i in
    unsafe_write_int_as_uint32 (j - i) buffer 4;
    j
end

module LEW = Writer(Little_endian)
module BEW = Writer(Big_endian)
module LER = Reader(Little_endian)
module BER = Reader(Big_endian)

open Lwt

let recv_one_message connection buffer =
  let buffer =
    if String.length buffer < 16
    then String.create 65536
    else buffer in

  (* Read the minimum for knowing the total size of the message *)
  recv_exactly connection buffer 0 16
  >>= (fun _ -> lwt_with_running connection & fun running ->
         (* We immediatly look for the byte order *)
         Lwt.catch
           (fun _ -> match unsafe_read_byte_as_char buffer 0 with
              | 'l' -> LER.read connection buffer
              | 'B' -> BER.read connection buffer
              | c -> fail (Reading_error (Printf.sprintf "invalid byte order: %s" (Char.escaped c))))
           (function
              | Out_of_bounds ->
                  fail (Reading_error "invalid message size")
              | exn -> fail exn))

let rec try_write f connection send buffer =
  try
    return (f connection send buffer, buffer, None)
  with
    | Out_of_bounds ->
        try_write f connection send & String.create (String.length buffer * 2)
    | exn -> return (0, buffer, Some exn)

let send_one_message connection send buffer =
  (perform
     (ptr, buffer, exn_opt) <-- try_write
       (match send.send_byte_order with
          | Little_endian -> LEW.write
          | Big_endian -> BEW.write)
       connection send
       (if String.length buffer < 16
        then String.create 65536
        else buffer);

     match exn_opt with
       | Some exn -> return (buffer, Some exn)
       | None ->
           try_bind (fun _ -> send_exactly connection buffer 0 ptr)
             (fun _ -> return (buffer, None))
             (fun exn -> return (buffer, Some exn)))
