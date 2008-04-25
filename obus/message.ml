(*
 * message.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Wire

module H = Header

module FieldsReader(R : Wire.Reader) =
struct
  let match_signature signature i =
    if (R.buffer.[i] <> '\x01' or R.buffer.[i + 1] <> signature)
    then
      raise (Read_error (Printf.sprintf "invalid signature: \"%c\" expected, got \"%s\"" signature (snd(R.string_signature i))))

  let read fields_length =
    Wire.read_until begin fun i acc ->
      match R.buffer.[i] with
        | '\x01' -> match_signature 'o' i; let i, x = R.string_string (i + 4) in  (R.pad8 i, { acc with H.path = Some x })
        | '\x02' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (R.pad8 i, { acc with H.interface = Some x })
        | '\x03' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (R.pad8 i, { acc with H.member = Some x })
        | '\x04' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (R.pad8 i, { acc with H.error_name = Some x })
        | '\x05' -> match_signature 'u' i; let x = R.int32_uint32 (i + 4) in (i + 8, { acc with H.reply_serial = Some x })
        | '\x06' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (R.pad8 i, { acc with H.destination = Some x })
        | '\x07' -> match_signature 's' i; let i, x = R.string_string (i + 4) in (R.pad8 i, { acc with H.sender = Some x })
        | '\x08' -> match_signature 'g' i; let i, x = R.string_signature (i + 4) in (R.pad8 i, { acc with H.signature = Some x })
        | code when code < '\x01' || code > '\x08' -> failwith "unknown header field code"
        | code -> raise (Read_error (Printf.sprintf "malformed header field(code = %d)" (int_of_char code)))
    end H.empty_fields fields_length 0
end

module Reader(MakeR : functor (B : Wire.Buffer) -> Wire.Reader) =
struct
  let read transport buffer byte_order =
    let module R = MakeR(struct let buffer = !buffer end) in
    let protocol_version = int_of_char R.buffer.[3] in
      if protocol_version <> Constant.protocol_version then
        raise (Read_error (Printf.sprintf
                             "invalid protocol version : %d (obus support only %d)"
                             protocol_version
                             Constant.protocol_version));

      let message_type = match R.buffer.[1] with
        | '\x00' -> H.Invalid
        | '\x01' -> H.Method_call
        | '\x02' -> H.Method_return
        | '\x03' -> H.Error
        | '\x04' -> H.Signal
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

          if total_length > Constant.max_message_size then
            raise (Read_error "message too big");

          if total_length > String.length !buffer then
            buffer := String.create total_length;

          assert (transport.Transport.recv !buffer 0 total_length = total_length);

          let module R = FieldsReader(MakeR(struct let buffer = !buffer end)) in
          let start, fields = R.read fields_length in
            ({ H.byte_order = byte_order;
               H.message_type = message_type;
               H.flags = flags;
               H.serial = serial;
               H.fields = fields }, start)
end

module Writer(W : Wire.Writer) =
struct
  let write_if code signature writer i = function
    | None  -> i
    | Some(v) ->
        W.buffer.[i] <- code;
        W.buffer.[i + 1] <- '\x01';
        W.buffer.[i + 2] <- signature;
        W.buffer.[i + 3] <- '\x00';
        W.pad8 (writer (i + 4) v)

  let write transport byte_order_char header writer =
    W.buffer.[0] <- byte_order_char;
    W.buffer.[1] <- begin match header.H.message_type with
        | H.Invalid -> '\x00'
        | H.Method_call -> '\x01'
        | H.Method_return -> '\x02'
        | H.Error -> '\x03'
        | H.Signal -> '\x04'
    end;
    W.buffer.[2] <- char_of_int ((if header.H.flags.H.no_reply_expected then 1 else 0) lor
                                   (if header.H.flags.H.no_auto_start then 2 else 0));
    W.buffer.[3] <- char_of_int Constant.protocol_version;
    W.int32_uint32 8 header.H.serial;
    let i = write_if '\x01' 'o' W.string_string 16 header.H.fields.H.path in
    let i = write_if '\x02' 's' W.string_string i header.H.fields.H.interface in
    let i = write_if '\x03' 's' W.string_string i header.H.fields.H.member in
    let i = write_if '\x04' 's' W.string_string i header.H.fields.H.error_name in
    let i = write_if '\x05' 'u' (fun i x -> W.int32_uint32 i x; i + 4) i header.H.fields.H.reply_serial in
    let i = write_if '\x06' 's' W.string_string i header.H.fields.H.destination in
    let i = write_if '\x07' 's' W.string_string i header.H.fields.H.sender in
    let i = write_if '\x08' 'g' W.string_signature i header.H.fields.H.signature in
      if i > Constant.max_array_size
      then raise (Wire.Write_error "array too big!")
      else begin
        W.int_uint32 12 i;
        let j = writer header.H.byte_order W.buffer i in
          if j > Constant.max_message_size
          then raise (Wire.Write_error "message too big!")
          else begin
            W.int_uint32 4 (j - i);
            assert (transport.Transport.send W.buffer 0 j = j)
          end
      end
end

let read transport buffer =
  (* Read the minimum for knowing the total size of the message *)
  assert (transport.Transport.recv !buffer 0 16 = 16);
  (* We immediatly look for the byte order, then read the header *)
  match !buffer.[0] with
    | 'l' -> let module R = Reader(Wire.LEReader)
      in R.read transport buffer H.Little_endian
    | 'B' -> let module R = Reader(Wire.BEReader)
      in R.read transport buffer H.Big_endian
    | _ -> raise (Read_error "invalid byte order")

let write transport buffer header body_writer =
  let rec aux () =
    try
      match header.H.byte_order with
        | H.Little_endian -> let module W = Writer(Wire.LEWriter(struct let buffer = !buffer end))
          in W.write transport 'l' header body_writer
        | H.Big_endian -> let module W = Writer(Wire.BEWriter(struct let buffer = !buffer end))
          in W.write transport 'B' header body_writer
    with
        Invalid_argument "index out of bounds" ->
          buffer := (String.create (String.length !buffer * 2));
          aux ()
  in
    aux ()
