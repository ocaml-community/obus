(*
 * header.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module L = LowLevel_internal

type typ =
  | Invalid
  | Method_call
  | Method_return
  | Error
  | Signal

type serial = int

type flags =
  | ReplyExpected
  | AutoStart

type fields =
  | Path of string
  | Interface of string
  | Member of string
  | Error_name of string
  | Reply_serial of serial
  | Destination of string
  | Sender of string
  | Signature of string

type t = {
  byte_order : L.byte_order;
  typ : typ;
  flags : flags list;
  protocol_version : int;
  length : int;
  serial : serial;
  fields : header_fields list;
}

module Write(BO : L.ByteOrder)(Buffer : L.BufferType) =
struct
  module W = L.Writer(BO)(Buffer)
  let write header i =
    let i = W.write_byte begin match header.byte_order with
      | L.LittleEndian -> 'l'
      | L.BigEndian -> 'B'
    end i in
    let i = W.write_byte begin match header.typ with
      | Invalid -> '\x00'
      | Method_call -> '\x01'
      | Method_return -> '\x02'
      | Error -> '\x03'
      | Signal -> '\x04'
    end i in
    let i = W.write_byte begin char_of_int
        (List.fold_left
           (fun acc flag -> match flag with
              | ReplyExpected -> acc land (lnot 1)
              | AutoStart -> acc land (lnot 2))
           0b11 header.flags)
    end i in
    let j = W.write_byte (char_of_int header.hProtocolVersion) i in
    let i = W.write_uint32 header.length j in
    let i = W.write_uint32_from_int32 header.serial i in
    let i = W.write_array begin fun v i ->
      write_struct begin fun v i -> match v with
        | Path(x) -> let i = W.write_byte '\x01' in W.write_fixed_variant 'o' write_object_path x i
        | Interface(x) -> let i = W.write_byte '\x02' in W.write_fixed_variant 's' write_string x
        | Member(x) -> let i = W.write_byte '\x03' in W.write_fixed_variant 's' write_string x
        | Error_name(x) -> let i = W.write_byte '\x04' in W.write_fixed_variant 's' write_string x
        | Reply_serial(x) -> let i = W.write_byte '\x05' in W.write_fixed_variant 'u' write_int32_from_int32 x
        | Destination(x) -> let i = W.write_byte '\x06' in W.write_fixed_variant 's' write_string x
        | Sender(x) -> let i = W.write_byte '\x07' in W.write_fixed_variant 's' write_string x
        | Signature(x) -> let i = W.write_byte '\x08' in W.write_fixed_variant 'g' write_signature x
      end v i
    end (fun f x l -> List.fold_left (fun acc v -> f v acc) x l) header.fields i
    in
      ((fun length -> ignore (W.write_uint32 length j)), W.pad8 i)
end

module WriteLE = Write(L.LittleEndian)
module WriteBE = Write(L.BigEndian)

let write buffer header i =
  let module Buffer = struct let buffer = buffer end in
    match header.byte_order with
      | L.LittleEndian -> let module W = WriteLE(Buffer) in
          W.write header i
      | L.BigEndian -> let module W = WriteBE(Buffer) in
          W.write header i

module Read(BO : L.ByteOrder)(Buffer : L.BufferType) =
struct
  module R = L.Reader(BO)(Buffer)
  let reader i =
    let i, byte_order = read_byte i in
    let i, message_type = read_byte i in
    let i, flags = read_byte i in
    let i, protocol_version = read_bute i in
    let i, length = read_uint32 i in
    let i, serial = read_uint32_as_int32 i in
    let i, fields = read_array begin fun i ->
      read_struct begin fun i ->
        let i, code = read_byte i in
          match code with
            | '\x01' -> let i, x = read_fixed_variant 'o' read_objectpath i (x) i in Path(x)
            | '\x02' -> let i, x = read_fixed_variant 's' read_string i (x) i in Interface(x)
            | '\x03' -> let i, x = read_fixed_variant 's' read_string i (x) i in Member(x)
            | '\x04' -> let i, x = read_fixed_variant 's' read_string i (x) i in Error_name(x)
            | '\x05' -> let i, x = read_fixed_variant 'u' read_uint32_as_int32 i (x) i in Reply_serial(Int32.to_int x)
            | '\x06' -> let i, x = read_fixed_variant 's' read_string i (x) i in Destination(x)
            | '\x07' -> let i, x = read_fixed_variant 's' read_string i (x) i in Sender(x)
            | '\x08' -> let i, x = read_fixed_variant 'g' read_signature i (x) i in Signature(x)
            | code when code < '\x01' || code > '\x08' -> failwith "unknown header field code"
            | _ -> let _, x = read_variant i in
                failwith (Printf.sprintf "malformed header field(code = %d, value = %s)"
                            (int_of_char code)
                            (Values.M.string_of_single x))
      end [] (fun e l -> e :: l) in
      (R.pad8 i,
       { byte_order = (match byte_order with
                         | 'l' -> LittleEndian
                         | 'B' -> BigEndian
                         | _ -> failwith "unknown endianess");
         typ = (match message_type with
                  | '\x00' -> Invalid
                  | '\x01' -> Method_call
                  | '\x02' -> Method_return
                  | '\x03' -> Error
                  | '\x04' -> Signal
                  | _ -> failwith "unknown message type");
         flags = begin
           let flags = int_of_char flags in
           let l = if flags land 1 = 0 then [ReplyExpected] else [] in
             if flags land 2 = 0 then AutoStart :: l else l
         end;
         protocol_version = int_of_char protocol_version;
         length = length;
         serial = serial;
         flags = fields })
end

let read buffer header =
  let module Buffer = struct let buffer = buffer end in
    match buffer.[i] with
      | 'l' -> let module R = ReadLE(Buffer) in
          R.read i
      | 'B' -> let module R = ReadBE(Buffer) in
          R.read i
