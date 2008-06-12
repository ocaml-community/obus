(*
 * wireMessage.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Wire
open Message
open Printf

(* Raw description of an header *)

type raw_fields = {
  _path : path option;
  _member : member option;
  _interface : interface option;
  _error_name : error_name option;
  _reply_serial : reply_serial option;
  _destination : destination option;
  _sender : sender option;
  _signature : string;
}

let empty_fields = {
  _path = None;
  _member = None;
  _interface = None;
  _error_name = None;
  _reply_serial = None;
  _destination = None;
  _sender = None;
  _signature = "";
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

module MakeReader(Reader : Wire.Reader) =
struct
  open Reader

  let read transport buffer =
    (* Check the protocol version first, since we can not do anything
       if it is not the same as our *)
    let protocol_version = int_of_char buffer.[3] in
      if protocol_version <> Constant.protocol_version
      then raise (Reading_error (sprintf "invalid protocol version: %d" protocol_version));

      (* Unmarshaling of all informations contained in the first
         16-byte of the header *)

      let message_maker = match int_of_char buffer.[1] with
        | 1 -> method_call_of_raw
        | 2 -> method_return_of_raw
        | 3 -> error_of_raw
        | 4 -> signal_of_raw
        | n -> raise (Reading_error (sprintf "unknown message type: %d" n))
      and flags =
        let flags = int_of_char buffer.[2] in
          { no_reply_expected = flags land 1 = 1;
            no_auto_start = flags land 2 = 2 }
      and length = snd (read_int_uint32 buffer 4)
      and serial = snd (read_int32_uint32 buffer 8)
      and fields_length = snd (read_int_uint32 buffer 12) in

      (* Header fields array start on byte #16 and message start aligned
         on a 8-boundary after it, so we have: *)
      let body_start = 16 + rpad8 fields_length in
      let total_length = body_start + length in
        (* Safety checking *)
        if fields_length > Constant.max_array_size then
          raise (Reading_error (sprintf "size of header fields exceed the limit: %d" fields_length));

        if total_length > Constant.max_message_size then
          raise (Reading_error (sprintf "message size exceed the limit: %d" total_length));

        let buffer =
          if total_length > String.length buffer
          then begin
            (* Do not grow up the buffer by a small amount of bytes,
               if it is to small, directly double the buffer length *)
            let needed = max total_length (String.length buffer * 2) in
              (* ensure that the length is still a multiple of 8 (this
                 is assumed by padding functions) *)
            let needed = rpad8 needed in
            let new_buffer = String.create needed in
              (* We keep the constant part, maybe this can be usefull *)
              String.blit buffer 0 new_buffer 0 16;
              new_buffer
          end else buffer in

          (* Read the rest of the message *)
          Transport.recv_exactly transport buffer 16 (total_length - 16);

          (* Parse header fields *)
          let read_field signature name reader f i =
            let j, len = read_int_byte buffer i in
            let j, ch = read_char_byte buffer j in
            let j, zt = read_char_byte buffer j in
              if len <> 1 || ch <> signature || zt <> '\000'
              then begin
                let i, s = read_string_signature buffer i in
                  raise (Reading_error
                           (Printf.sprintf
                              "invalid signature for header field %s: '%s', must be: '%c'"
                              name s signature))
              end else
                let i, x = reader buffer j in
                  f i x in
          let fields = read_until
            (fun i acc cont ->
               let i = rpad8 i in
               let i, code = read_int_byte buffer i in
                 match code with
                   | 1 -> read_field 'o' "path" read_string_string
                       (fun i x -> cont i { acc with _path = Some x }) i
                   | 2 -> read_field 's' "interface" read_string_string
                       (fun i x -> cont i { acc with _interface = Some x }) i
                   | 3 -> read_field 's' "member" read_string_string
                       (fun i x -> cont i { acc with _member = Some x }) i
                   | 4 -> read_field 's' "error_name" read_string_string
                       (fun i x -> cont i { acc with _error_name = Some x }) i
                   | 5 -> read_field 'u' "reply_serial" read_int32_uint32
                       (fun i x -> cont i { acc with _reply_serial = Some x }) i
                   | 6 -> read_field 's' "destination" read_string_string
                       (fun i x -> cont i { acc with _destination = Some x }) i
                   | 7 -> read_field 's' "sender" read_string_string
                       (fun i x -> cont i { acc with _sender = Some x }) i
                   | 8 -> read_field 'g' "signature" read_string_signature
                       (fun i x -> cont i { acc with _signature = x }) i
                   | n ->
                       (* Just ignore the field, as said in the
                          specification *)
                       let i, _ = match byte_order with
                         | Little_endian -> Values.LEReader.read_variant buffer (i + 1)
                         | Big_endian -> Values.BEReader.read_variant buffer (i + 1)
                       in
                         cont i acc)
            empty_fields 16 (fields_length + 16) in
            (intern_make_recv
               ~flags:flags
               ?sender:fields._sender
               ?destination:fields._destination
               ~serial:serial
               ~signature:fields._signature
               ~typ:(message_maker fields)
               ~body:(byte_order, buffer, body_start)
               (),
             buffer)
end

module MakeWriter(Writer : Wire.Writer) =
struct
  open Writer

  let write transport buffer serial message =
    buffer.[0] <- (match byte_order with
                     | Little_endian -> 'l'
                     | Big_endian -> 'B');
    let code, fields = match message.typ with
      | `Method_call(path, interface, member) ->
          (1,
           { _path = Some path;
             _interface = interface;
             _member = Some member;
             _error_name = None;
             _reply_serial = None;
             _destination = message.destination;
             _sender = message.sender;
             _signature = message.signature })
      | `Method_return(reply_serial) ->
          (2,
           { _path = None;
             _interface = None;
             _member = None;
             _error_name = None;
             _reply_serial = Some reply_serial;
             _destination = message.destination;
             _sender = message.sender;
             _signature = message.signature })
      | `Error(reply_serial, error_name) ->
          (3,
           { _path = None;
             _interface = None;
             _member = None;
             _error_name = Some error_name;
             _reply_serial = Some reply_serial;
             _destination = message.destination;
             _sender = message.sender;
             _signature = message.signature })
      | `Signal(path, interface, member) ->
          (4,
           { _path = Some path;
             _interface = Some interface;
             _member = Some member;
             _error_name = None;
             _reply_serial = None;
             _destination = message.destination;
             _sender = message.sender;
             _signature = message.signature })
    in
      buffer.[1] <- char_of_int code;
      buffer.[2] <- char_of_int ((if message.flags.no_reply_expected then 1 else 0) lor
                                   (if message.flags.no_auto_start then 2 else 0));
      buffer.[3] <- char_of_int Constant.protocol_version;
      ignore (write_int32_uint32 buffer 8 serial);

      let write_field code signature writer i = function
        | None -> i
        | Some v ->
            let i = wpad8 buffer i in
            let i = write_int_byte buffer i code in
            let i = write_string_signature buffer i signature in
              writer buffer i v in
      let i = write_field 1 "o" write_string_string 16 fields._path in
      let i = write_field 2 "s" write_string_string i fields._interface in
      let i = write_field 3 "s" write_string_string i fields._member in
      let i = write_field 4 "s" write_string_string i fields._error_name in
      let i = write_field 5 "u" write_int32_uint32 i fields._reply_serial in
      let i = write_field 6 "s" write_string_string i fields._destination in
      let i = write_field 7 "s" write_string_string i fields._sender in
      let i = write_field 8 "g" write_string_signature i (Some fields._signature) in
      let len = i - 16 in
        if len > Constant.max_array_size
        then raise (Writing_error (sprintf "header fields array exceed the limit: %d" len));

        ignore (write_int_uint32 buffer 12 len);
        (* The body start after alignement padding of the header to an
           8-boundary *)
        let i = wpad8 buffer i in
        let j = message.body byte_order buffer i in
          if j > Constant.max_message_size
          then raise (Writing_error (sprintf "message too big to be sent: %d" j));

          ignore (write_int_uint32 buffer 4 (j - i));
          Transport.send_exactly transport buffer 0 j
end

module LEReader = MakeReader(LEReader)
module BEReader = MakeReader(BEReader)
module LEWriter = MakeWriter(LEWriter)
module BEWriter = MakeWriter(BEWriter)

let recv_one_message transport buffer =
  let buffer =
    if String.length buffer < 16
    then String.create 65536
    else buffer in
    (* Read the minimum for knowing the total size of the message *)
    Transport.recv_exactly transport buffer 0 16;
    try
      (* We immediatly look for the byte order *)
      match buffer.[0] with
        | 'l' -> LEReader.read transport buffer
        | 'B' -> BEReader.read transport buffer
        | c -> raise (Reading_error (Printf.sprintf "invalid byte order: %s" (Char.escaped c)))
    with
        Out_of_bounds ->
          raise (Reading_error "invalid message size")

let send_one_message transport buffer serial message =
  let f = match Info.native_byte_order with
    | Little_endian -> LEWriter.write
    | Big_endian -> BEWriter.write
  in
  let rec aux buffer =
    try
      f transport buffer serial message;
      buffer
    with
        Out_of_bounds ->
          (* Try with a bigger buffer *)
          aux (String.create (String.length buffer * 2))
  in
    aux (if String.length buffer < 16
         then String.create 65536
         else buffer)
