(*
 * wireMessage.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open Lwt
open Wire
open OBus_message
open OBus_internals
open OBus_value
open OBus_info

(* Serializaion buffers, since we do not use threads this is correct
   and this avoid to have to create one buffer by connection *)

let rbuffer = ref (String.create 65536)
let wbuffer = ref (String.create 65536)

(* Raw description of header fields *)

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

module Reader(BO : Byte_order) =
struct
  include Make_unsafe_reader(BO)

  let read_fields limit =
    let buffer = !rbuffer in

    let rfield code typ reader i acc f =
      let i, t = rtype buffer i in
      match t with
        | Tbasic t' when t' = typ ->
            let i, v = reader buffer i in
            (i, f v)
        | _ -> raise & Reading_error
            (sprintf "invalid header field signature for code %d: %S, should be %S"
               code (string_of_signature [t]) (string_of_signature [Tbasic typ]))
    in

    let rec aux (i, acc) =
      if i < limit
      then begin
        let i = rpad8 buffer i in
        let i, v = ruint8 buffer i in
        aux (match v with
               | 1 -> rfield 1 Tobject_path robject_path i acc (fun x -> { acc with _path = Some x })
               | 2 -> rfield 2 Tstring rstring i acc (fun x -> {  acc with _interface = Some x })
               | 3 -> rfield 3 Tstring rstring i acc (fun x -> { acc with _member = Some x })
               | 4 -> rfield 4 Tstring rstring i acc (fun x -> { acc with _error_name = Some x })
               | 5 -> rfield 5 Tuint32 ruint32 i acc (fun x -> { acc with _reply_serial = Some x })
               | 6 -> rfield 6 Tstring rstring i acc (fun x -> { acc with _destination = Some x })
               | 7 -> rfield 7 Tstring rstring i acc (fun x -> { acc with _sender = Some x })
               | 8 -> rfield 8 Tsignature rsignature i acc (fun x -> { acc with _signature = x })
               | n -> rwrap rvariant (fun _ -> acc) buffer i)
      end else
        if i > limit
        then raise & Reading_error "invalid array size"
        else (i, acc)

    in
    aux (0, empty_fields)

  let read connection =
    let buffer = !rbuffer in

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

    (* Header fields array start on byte #16 and message start aligned
       on a 8-boundary after it, so we have: *)
    let total_length = 16 + fields_length + (pad8 fields_length) + length in
    (* Safety checking *)
    rcheck_array_len fields_length;

    if total_length > OBus_info.max_message_size
    then raise (Reading_error (sprintf "message size exceed the limit: %d" total_length));

    recv_exactly connection buffer 0 (total_length - 16)
    >>= fun _ ->
      try
        let ptr, fields = read_fields fields_length in
        let ptr = rpad8 buffer ptr in
        let ptr, body = rsequence fields._signature buffer ptr in
        if ptr + 16 = total_length
        then return { flags = flags;
                      sender = fields._sender;
                      destination = fields._destination;
                      serial = serial;
                      typ = message_maker fields;
                      body = body }
        else fail & Reading_error "junk after message"
      with exn -> fail exn
end

module Writer(BO : Byte_order) =
struct
  include Make_unsafe_writer(BO)

  let write connection msg =
    let buffer = !wbuffer in

    let code, fields = match msg.typ with
      | `Method_call(path, interface, member) ->
          (1,
           { _path = Some path;
             _interface = interface;
             _member = Some member;
             _error_name = None;
             _reply_serial = None;
             _destination = msg.destination;
             _sender = msg.sender;
             _signature = type_of_sequence msg.body })
      | `Method_return(reply_serial) ->
          (2,
           { _path = None;
             _interface = None;
             _member = None;
             _error_name = None;
             _reply_serial = Some reply_serial;
             _destination = msg.destination;
             _sender = msg.sender;
             _signature = type_of_sequence msg.body })
      | `Error(reply_serial, error_name) ->
          (3,
           { _path = None;
             _interface = None;
             _member = None;
             _error_name = Some error_name;
             _reply_serial = Some reply_serial;
             _destination = msg.destination;
             _sender = msg.sender;
             _signature = type_of_sequence msg.body })
      | `Signal(path, interface, member) ->
          (4,
           { _path = Some path;
             _interface = Some interface;
             _member = Some member;
             _error_name = None;
             _reply_serial = None;
             _destination = msg.destination;
             _sender = msg.sender;
             _signature = type_of_sequence msg.body })
    in
    unsafe_write_char_as_byte buffer 0 BO.byte_order_char;
    unsafe_write_int_as_byte buffer 1 code;
    unsafe_write_int_as_byte buffer 2
      ((if msg.flags.no_reply_expected then 1 else 0) lor
         (if msg.flags.no_auto_start then 2 else 0));
    unsafe_write_int_as_byte buffer 3 OBus_info.protocol_version;
    unsafe_write_int32_as_uint32 buffer 8 msg.serial;

    let wfield code typ writer field i = match field with
      | None -> i
      | Some v ->
          let i = wpad8 buffer i in
          let i = wuint8 buffer i code in
          let i = wtype buffer i (Tbasic typ) in
          writer buffer i v in
    let i = wfield 1 Tobject_path wobject_path fields._path 12 in
    let i = wfield 2 Tstring wstring fields._interface i in
    let i = wfield 3 Tstring wstring fields._member i in
    let i = wfield 4 Tstring wstring fields._error_name i in
    let i = wfield 5 Tuint32 wuint32 fields._reply_serial i in
    let i = wfield 6 Tstring wstring fields._destination i in
    let i = wfield 7 Tstring wstring fields._sender i in
    let i = wfield 8 Tsignature wsignature (Some fields._signature) i in
    let len = i - 16 in
    wcheck_array_len len;
    unsafe_write_int_as_uint32 buffer 12 len;
    let i = wpad8 buffer i in
    let j = wsequence buffer i msg.body in
    let len = j - i in
    if len > OBus_info.max_message_size
    then raise (Writing_error (sprintf "message size exceed the limit: %d" len));
    unsafe_write_int_as_uint32 buffer 4 (j - i);
    j
end

module LEW = Writer(Little_endian)
module BEW = Writer(Big_endian)
module LER = Reader(Little_endian)
module BER = Reader(Big_endian)

let recv_one_message connection =
  (* Read the minimum for knowing the total size of the message *)
  recv_exactly connection !rbuffer 0 16 >>= fun _ ->
    (* We immediatly look for the byte order *)
    Lwt.catch
      (fun _ -> match String.unsafe_get !rbuffer 0 with
         | 'l' -> LER.read connection
         | 'B' -> BER.read connection
         | c -> fail & Reading_error (Printf.sprintf "invalid byte order: %s" & Char.escaped c))
      (function
         | Out_of_bounds ->
             fail & Reading_error "invalid message size"
         | exn -> fail exn)

let rec try_write f connection msg =
  try
    return & f connection msg
  with
    | Out_of_bounds ->
        wbuffer := String.create (String.length !wbuffer * 2);
        try_write f connection msg
    | exn -> fail exn

let send_one_message connection msg =
  try_write
    (match native_byte_order with
       | Little_endian -> LEW.write
       | Big_endian -> BEW.write)
    connection msg
  >>= send_exactly connection !wbuffer 0
