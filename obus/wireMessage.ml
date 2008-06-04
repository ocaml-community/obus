(*
 * wireMessage.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)


open Wire
open Header
open Printf

module MakeReader(Reader : Wire.Reader) =
struct
  open Reader

  let read transport buffer =
    (* Check the protocol version first, since we can not do anything
       if it is not the same as our *)
    let protocol_version = int_of_char buffer.[3] in
      if protocol_version <> Constant.protocol_version
      then raise (Content_error (sprintf "invalid protocol version: %d" protocol_version));

      (* Unmarshaling of all informations contained in the first 16-byte
         of the header *)
      let message_type = match int_of_char buffer.[1] with
        | 1 -> Method_call
        | 2 -> Method_return
        | 3 -> Error
        | 4 -> Signal
        | n -> raise (Content_error (sprintf "unknown message type: %d" n))
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
          let read_field signature reader f i =
            check_signature buffer (i + 1) signature;
            let i, x = reader buffer (i + 4) in
              f i (Some x) in
          let fields = read_until
            (fun i acc cont -> let i = rpad8 i in
                 match snd (read_int_byte buffer i) with
                   | 1 -> read_field "o" read_string_string (fun i x -> cont i { acc with path = x }) i
                   | 2 -> read_field "s" read_string_string (fun i x -> cont i { acc with interface = x }) i
                   | 3 -> read_field "s" read_string_string (fun i x -> cont i { acc with member = x }) i
                   | 4 -> read_field "s" read_string_string (fun i x -> cont i { acc with error_name = x }) i
                   | 5 -> read_field "u" read_int32_uint32 (fun i x -> cont i { acc with reply_serial = x }) i
                   | 6 -> read_field "s" read_string_string (fun i x -> cont i { acc with destination = x }) i
                   | 7 -> read_field "s" read_string_string (fun i x -> cont i { acc with sender = x }) i
                   | 8 -> read_field "g" read_string_signature (fun i x -> cont i { acc with signature = x }) i
                   | n -> raise (Content_error (sprintf "unknown header fields: %d" n)))
            empty_fields 16 (fields_length + 16) in
            ({ byte_order = byte_order;
               message_type = message_type;
               flags = flags;
               serial = serial;
               length = length;
               fields = fields },
             buffer,
             body_start)
end

module MakeWriter(Writer : Wire.Writer) =
struct
  open Writer

  let write transport buffer header serial writer =
    buffer.[0] <- (match byte_order with
                     | Little_endian -> 'l'
                     | Big_endian -> 'B');
    buffer.[1] <- char_of_int begin match header.message_type with
      | Method_call -> 1
      | Method_return -> 2
      | Error -> 3
      | Signal -> 4
    end;
    buffer.[2] <- char_of_int ((if header.flags.no_reply_expected then 1 else 0) lor
                                 (if header.flags.no_auto_start then 2 else 0));
    buffer.[3] <- char_of_int Constant.protocol_version;
    ignore (write_int32_uint32 buffer 8 serial);

    let write_field code signature writer i = function
      | None -> i
      | Some v ->
          let i = wpad8 buffer i in
          let i = write_int_byte buffer i code in
          let i = write_string_signature buffer i signature in
            writer buffer i v in
    let i = write_field 1 "o" write_string_string 16 header.fields.path in
    let i = write_field 2 "s" write_string_string i header.fields.interface in
    let i = write_field 3 "s" write_string_string i header.fields.member in
    let i = write_field 4 "s" write_string_string i header.fields.error_name in
    let i = write_field 5 "u" write_int32_uint32 i header.fields.reply_serial in
    let i = write_field 6 "s" write_string_string i header.fields.destination in
    let i = write_field 7 "s" write_string_string i header.fields.sender in
    let i = write_field 8 "g" write_string_signature i header.fields.signature in
    let len = i - 16 in
      if len > Constant.max_array_size
      then raise (Writing_error (sprintf "header fields array exceed the limit: %d" len));

      ignore (write_int_uint32 buffer 12 len);
      (* The body start after alignement padding of the header to an
         8-boundary *)
      let i = wpad8 buffer i in
      let j = writer buffer i in
        if j > Constant.max_message_size
        then raise (Writing_error (sprintf "message too big to be send: %d" j));

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
      | Out_of_bounds ->
          raise (Reading_error "invalid message size")
      | Content_error msg ->
          (* All content errors in the header are considered as fatal *)
          raise (Reading_error msg)
      | Reading_error _ as exn ->
          raise exn
      | Transport.Error _ as exn ->
          raise exn
      | exn ->
          (* must never happen *)
          raise (Reading_error (sprintf "unexpected error: %s" (Printexc.to_string exn)))

let send_one_message transport buffer header serial writer =
  let f = match header.byte_order with
    | Little_endian -> LEWriter.write
    | Big_endian -> BEWriter.write
  in
  let rec aux buffer =
    try
      f transport buffer header serial writer;
      buffer
    with
      | Out_of_bounds ->
          (* Try with a bigger buffer *)
          aux (String.create (String.length buffer * 2))
      | Convertion_failed _ as exn ->
          raise exn
      | Writing_error _ as exn ->
          raise exn
      | Transport.Error _ as exn ->
          raise exn
      | exn ->
          raise (Convertion_failed exn)
  in
    aux (if String.length buffer < 16
         then String.create 65536
         else buffer)
