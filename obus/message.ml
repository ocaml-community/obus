(*
 * message.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Internal

module H = Header

module FieldsReader(R : Wire.BasicReader) =
struct
  let match_signature signature i =
    if (R.buffer.[i] <> '\x01' or R.buffer.[i + 1] <> signature)
    then
      raise (Read_error (Printf.sprintf "invalid signature: %s expected, got %s" signature (snd(R.string_signature i))))

  let read () =
    let i, fields = Wire.read_until begin fun i acc ->
      match buffer.[i] with
        | '\x01' -> match_signature 'o' i; let i, x = R.string_string (i + 4) in  (B.pad8 i, { acc with H.path = Some x })
        | '\x02' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (B.pad8 i, { acc with H.interface = Some x })
        | '\x03' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (B.pad8 i, { acc with H.member = Some x })
        | '\x04' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (B.pad8 i, { acc with H.error_name = Some x })
        | '\x05' -> match_signature 'u' i; let i, x = R.int32_uint32 (i + 4) in (B.pad8 i, { acc with H.reply_serial = Some x })
        | '\x06' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (B.pad8 i, { acc with H.destination = Some x })
        | '\x07' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (B.pad8 i, { acc with H.sender = Some x })
        | '\x08' -> match_signature 'g' i; let i, x = R.string_signature (i + 4) in (B.pad8 i, { acc with H.signature = Some x })
        | code when code < '\x01' || code > '\x08' -> failwith "unknown header field code"
        | _ -> raise (Read_error (Printf.sprintf "malformed header field(code = %d)" (int_of_char code)))
    end H.empty_fields fields_length 0 in
end

module Reader(MakeR : functor (B : Wire.Buffer) -> Wire.BasicReader) =
struct
  let read fd buffer byte_order =
    let module R = MakeR(struct let buffer = !buffer end) in
    let protocol_version = int_of_char R.buffer.[3] in
      if protocol_version <> Constant.protocol_version then
        raise (Read_error (Printf.sprinf
                             "invalid protocol version : %d (obus support only %d)"
                             protocol_version
                             obus_protocol_version));

      let message_type = match R.buffer.[1] with
        | '\x00' -> Invalid
        | '\x01' -> Method_call
        | '\x02' -> Method_return
        | '\x03' -> Error
        | '\x04' -> Signal
        | _ -> raise (Read_error "unknown message type")
      and flags =
        let flags = int_of_char R.buffer.[2] in
          { H.no_reply_expected = flags land 1 = 1;
            H.no_auto_start = flags land 2 = 2 }
      and length = R.int_uint32 4
      and serial = R.int32_uint32 8
        (* header fields array length *)
      and fields_length = R.int_uint32 12 in

        if fields_length > Constant.max_array_size then
          raise (Read_error "array too big");

        (* Header fields array start on byte #16 and message start aligned
           on a 8-boundary after it, so we have: *)
        let total_length = (R.pad8 fields_length) + length in

          if total_length > max_message_size then
            raise (Read_error "message too big");

          if total_length > String.length !buffer then
            buffer := String.create total_length;

          let count = Unix.read fd !buffer 0 total_length in
            if count <> total_length then raise Communication_error;

            let module R = FieldsReader(MakeR(struct let buffer = !buffer end)) in
            let start, fields = R.read () in
              (start, { byte_order = byte_order;
                        message_type = message_type;
                        flags = flags;
                        protocol_version = protocol_version;
                        serial = serial;
                        fields = fields })
end

module Writer(W : Wire.BasicWriter) =
struct
  let write_if code signature writer i = function
    | None  -> i
    | Some(v) ->
        W.buffer.[i] <- code;
        W.buffer.[i + 1] <- '\x01';
        W.buffer.[i + 2] <- signature;
        W.buffer.[i + 3] <- '\x00';
        B.pad8 (writer (i + 4) v)

  let write bus byte_order_char header writer =
    W.buffer.[0] <- byte_order_char;
    W.buffer.[1] <- begin match header.H.message_type with
        | Invalid -> '\x00'
        | Method_call -> '\x01'
        | Method_return -> '\x02'
        | Error -> '\x03'
        | Signal -> '\x04'
    end;
    W.buffer.[2] <- ((if header.flags.H.no_reply_expected then 1 else 0) lor
                       (if header.flags.H.no_auto_start then 2 else 0));
    W.buffer.[3] <- char_of_int Constant.protocol_version;
    W.int32_uint32 8 serial;
    let i = write_if '\x01' 'o' W.string_string 16 H.path in
    let i = write_if '\x02' 's' W.string_string i H.interface in
    let i = write_if '\x03' 's' W.string_string i H.member in
    let i = write_if '\x04' 's' W.string_string i H.error_name in
    let i = write_if '\x05' 'u' W.int32_uint32 i H.reply_serial in
    let i = write_if '\x06' 's' W.string_string i H.destination in
    let i = write_if '\x07' 's' W.string_string i H.sender in
    let i = write_if '\x08' 'g' W.string_signature i H.signature in
      if i > Constant.max_array_size
      then raise Wire.Write_error "array too big!"
      else begin
        W.int32_uint32 12 i;
        let j = writer i in
          if j > Constant.max_message_size
          then raise Wire.Write_error "message too big!"
          else begin
            W.int32_uint32 4 (j - i);
            let count = Unix.send bus W.buffer 0 j in
              if count <> j
              then raise Communication_error
end

let read transport buffer =
  (* Read the minimum for knowing the total size of the message *)
  let len = Unix.read fd !buffer 0 16 in
    if len <> 16 then raise Communication_error;
    (* We immediatly look for the byte order, then read the header *)
    match bus.buffer.[0] with
      | 'l' -> let module MakeR = Reader(Wire.LEWriter)
        in R.read_message fd buffer H.LittleEndian
      | 'B' -> let module MakeR = Reader(Wire.BEWriter)
        in R.read_message fd buffer H.BigEndian

let write transport buffer header body_writer =
  let rec aux () =
    try
      match header.H.byte_order with
        | H.LittleEndian -> let module W = Writeer(Wire.LEWriter(struct let buffer = bus.out_buffer end))
          in W.write_message fd 'l' header body_writer
        | H.BigEndian -> let module W = Writer(Wire.BEWriter(struct let buffer = bus.out_buffer end))
          in W.write_message fd 'B' header body_writer
    with
        Invalid_argument "index out of bounds" ->
          buffer := (String.create (String.length !buffer * 2));
          aux ()
  in
    aux ()
