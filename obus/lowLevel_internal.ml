(*
 * lowLevel_internal.ml
 * --------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type buffer = string
exception Connection_failed

let max_array_size = 1 lsl 26

module type ByteOrder =
sig
  val data16_bit0 : int
  val data16_bit1 : int

  val data32_bit0 : int
  val data32_bit1 : int
  val data32_bit2 : int
  val data32_bit3 : int

  val data64_bit0 : int
  val data64_bit1 : int
  val data64_bit2 : int
  val data64_bit3 : int
  val data64_bit4 : int
  val data64_bit5 : int
  val data64_bit6 : int
  val data64_bit7 : int
end

module BigEndian : ByteOrder =
struct
  let data16_bit0 = 1
  let data16_bit1 = 0

  let data32_bit0 = 3
  let data32_bit1 = 2
  let data32_bit2 = 1
  let data32_bit3 = 0

  let data64_bit0 = 7
  let data64_bit1 = 6
  let data64_bit2 = 5
  let data64_bit3 = 4
  let data64_bit4 = 3
  let data64_bit5 = 2
  let data64_bit6 = 1
  let data64_bit7 = 0
end

module LittleEndian : ByteOrder =
struct
  let data16_bit0 = 0
  let data16_bit1 = 1

  let data32_bit0 = 0
  let data32_bit1 = 1
  let data32_bit2 = 2
  let data32_bit3 = 3

  let data64_bit0 = 0
  let data64_bit1 = 1
  let data64_bit2 = 2
  let data64_bit3 = 3
  let data64_bit4 = 4
  let data64_bit5 = 5
  let data64_bit6 = 6
  let data64_bit7 = 7
end

module type BufferType = sig val buffer : string end

type ptr = int

type byte_order = LittleEndian | BigEndian

exception Read_error of string
exception Write_error of string

type 'a reader = ptr -> ptr * 'a
type 'a writer = 'a -> ptr -> ptr

module T = Types_internal
module V = Values_internal

module Reader(BO : ByteOrder)(Buffer : BufferType) =
struct
  open BO
  open Buffer

  let pad1 x = x
  let pad2 i = i + (i mod 2)
  let pad4 i = i + (3 - (i-1) mod 4)
  let pad8 i = i + (7 - (i-1) mod 8)

  let pad_basic = function
    | T.Byte -> pad1
    | T.Boolean -> pad4
    | T.Int16 -> pad2
    | T.Int32 -> pad4
    | T.Int64 -> pad8
    | T.Uint16 -> pad2
    | T.Uint32 -> pad4
    | T.Uint64 -> pad8
    | T.Double -> pad8
    | T.String -> pad4
    | T.Signature -> pad1
    | T.Object_path -> pad4

  let pad_single = function
    | T.Basic(t) -> pad_basic t
    | T.Array _ -> pad4
    | T.Dict _ -> pad4
    | T.Structure _ -> pad8
    | T.Variant -> pad1

  let read_int16 i =
    let i = pad2 i in
    let v0 = int_of_char (buffer.[i + data16_bit0])
    and v1 = int_of_char (buffer.[i + data16_bit0]) in
      (i + 2,
       let v = v0 land (v1 lsl 8) in
         if v land (1 lsl 15) = 0
         then v
         else (-1 land (lnot 0x7fff)) lor v)

  let read_uint16 i =
    let i = pad2 i in
    let v0 = int_of_char buffer.[i + data16_bit0]
    and v1 = int_of_char buffer.[i + data16_bit1] in
      (i + 2,
       v0 lor (v1 lsl 8))

  let read_int32 i =
    let i = pad4 i in
    let v0 = int_of_char (buffer.[i + data32_bit0])
    and v1 = int_of_char (buffer.[i + data32_bit1])
    and v2 = int_of_char (buffer.[i + data32_bit2])
    and v3 = int_of_char (buffer.[i + data32_bit3]) in
      (i + 4,
       let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
         if v land (1 lsl 31) = 0
         then v
         else (-1 land (lnot 0x7fffffff)) lor v)

  let read_uint32 i =
    let i = pad4 i in
    let v0 = int_of_char (buffer.[i + data32_bit0])
    and v1 = int_of_char (buffer.[i + data32_bit1])
    and v2 = int_of_char (buffer.[i + data32_bit2])
    and v3 = int_of_char (buffer.[i + data32_bit3]) in
      (i + 4,
       v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24))

  let read_uint32 = read_int32

  let read_byte i =
    (i + 1,
     buffer.[i])

  let read_boolean i =
    let i = pad4 i in
      (i + 4,
       buffer.[i + data32_bit0] = '\001')

  let read_int32_as_int32 i =
    let i = pad4 i in
    let v0 = Int32.of_int (int_of_char (buffer.[i + data32_bit0]))
    and v1 = Int32.of_int (int_of_char (buffer.[i + data32_bit1]))
    and v2 = Int32.of_int (int_of_char (buffer.[i + data32_bit2]))
    and v3 = Int32.of_int (int_of_char (buffer.[i + data32_bit3])) in
      (i + 4,
       Int32.logor
         (Int32.logor
            v0
            (Int32.shift_left v1 8))
         (Int32.logor
            (Int32.shift_left v2 16)
            (Int32.shift_left v3 24)))

  let read_uint32_as_int32 = read_int32_as_int32

  let read_int64_as_int64 i =
    let i = pad8 i in
    let v0 = Int64.of_int (int_of_char (buffer.[i + data64_bit0]))
    and v1 = Int64.of_int (int_of_char (buffer.[i + data64_bit1]))
    and v2 = Int64.of_int (int_of_char (buffer.[i + data64_bit2]))
    and v3 = Int64.of_int (int_of_char (buffer.[i + data64_bit3]))
    and v4 = Int64.of_int (int_of_char (buffer.[i + data64_bit4]))
    and v5 = Int64.of_int (int_of_char (buffer.[i + data64_bit5]))
    and v6 = Int64.of_int (int_of_char (buffer.[i + data64_bit6]))
    and v7 = Int64.of_int (int_of_char (buffer.[i + data64_bit7])) in
      (i + 4,
       Int64.logor
         (Int64.logor
            (Int64.logor
               v0
               (Int64.shift_left v1 8))
            (Int64.logor
               (Int64.shift_left v2 16)
               (Int64.shift_left v3 24)))
         (Int64.logor
            (Int64.logor
               (Int64.shift_left v4 32)
               (Int64.shift_left v5 40))
            (Int64.logor
               (Int64.shift_left v6 48)
               (Int64.shift_left v7 56))))

  let read_uint64_as_int64 = read_int64_as_int64
  let read_int64 i = let i, v = read_int64_as_int64 i in (i, Int64.to_int v)
  let read_uint64 = read_int64

  let read_double i =
    let i, x = read_int64_as_int64 i in
      (i, Int64.to_float x)

  let read_string i =
    let i, length = read_uint32 i in
      (i + length + 1, String.sub buffer i length)

  let read_basic_type i =
    let i, c = read_byte i in
      (i + 1,
       match c with
         | 'y' -> T.Byte
         | 'b' -> T.Boolean
         | 'n' -> T.Int16
         | 'q' -> T.Uint16
         | 'i' -> T.Int32
         | 'u' -> T.Uint32
         | 'x' -> T.Int64
         | 't' -> T.Uint64
         | 'd' -> T.Double
         | 's' -> T.String
         | 'o' -> T.Object_path
         | 'g' -> T.Signature
         | _ -> raise (Read_error (Printf.sprintf "unknown type code %c" c)))

  let rec read_single_type i =
    let i, c = read_byte i in match c with
      | 'y' -> i, T.Basic T.Byte
      | 'b' -> i, T.Basic T.Boolean
      | 'n' -> i, T.Basic T.Int16
      | 'q' -> i, T.Basic T.Uint16
      | 'i' -> i, T.Basic T.Int32
      | 'u' -> i, T.Basic T.Uint32
      | 'x' -> i, T.Basic T.Int64
      | 't' -> i, T.Basic T.Uint64
      | 'd' -> i, T.Basic T.Double
      | 's' -> i, T.Basic T.String
      | 'o' -> i, T.Basic T.Object_path
      | 'g' -> i, T.Basic T.Signature
      | 'a' ->
          if buffer.[i] = '{'
          then begin
            let i = pad8 (i + 1) in
            let i, tkey = read_basic_type i in
            let i, tval = read_single_type i in
            let i, c = read_byte i in
              if c <> '}'
              then raise (Read_error "'}' expected")
              else i, T.Dict(tkey, tval)
          end else begin
            let i, t = read_single_type i in
              i, T.Array(t)
          end
      | '(' ->
          let i, t = read_single_type_until ')' i in
            i, T.Structure(t)
      | 'v' -> i,  T.Variant
      | _ -> raise (Read_error (Printf.sprintf "unknown type code %c" c))

  and read_single_type_until cend i =
    if buffer.[i] = cend
    then (i + 1, [])
    else
      let i, hd = read_single_type i in
      let i, tl = read_single_type_until cend i in
        (i, hd :: tl)

  let read_type = read_single_type_until '\x00'

  let read_signature i =
    read_type (i+1)

  let read_signature_as_string i =
    let length = int_of_char buffer.[i] in
      (i + length + 2, String.sub buffer (i+1) length)

  let read_object_path = read_string

  let read_basic_value t i = match t with
    | T.Byte -> let i, x = read_byte i in i, V.Byte x
    | T.Boolean -> let i, x = read_boolean i in i, V.Boolean x
    | T.Int16 -> let i, x = read_int16 i in i, V.Int16 x
    | T.Int32 -> let i, x = read_int32_as_int32 i in i, V.Int32 x
    | T.Int64 -> let i, x = read_int64_as_int64 i in i, V.Int64 x
    | T.Uint16 -> let i, x = read_uint16 i in i, V.Uint16 x
    | T.Uint32 -> let i, x = read_uint32_as_int32 i in i, V.Uint32 x
    | T.Uint64 -> let i, x = read_uint64_as_int64 i in i, V.Uint64 x
    | T.Double -> let i, x = read_double i in i, V.Double x
    | T.String -> let i, x = read_string i in i, V.String x
    | T.Signature -> let i, x = read_signature i in i, V.Signature x
    | T.Object_path -> let i, x = read_object_path i in i, V.Object_path x

  let rec read_until f x limit i =
    if i = limit
    then (i, x)
    else if i < limit
    then let j, y = f x i in
      read_until f y limit j
    else raise (Read_error "wrong array size!")

  let read_array elt_reader empty add i =
    let i, len = read_uint32 i in
      if len > max_array_size
      then raise (Read_error "array too big!")
      else
        read_until begin fun acc i ->
          let i, v = elt_reader i in
            (i, add v acc)
        end empty (i + len) i

  let read_dict key_reader val_reader empty add i =
    let i, len = read_uint32 i in
      if len > max_array_size
      then raise (Read_error "array too big!")
      else
        let i = pad8 i in
          read_until begin fun acc i ->
            let i = pad8 i in
            let i, key = key_reader i in
            let i, value = val_reader i in
              (i, add key value acc)
          end empty (i + len) i

  let read_structure reader i = reader (pad8 i)

  let rec read_variant i =
    let len = int_of_char buffer.[i] in
    let _, t = read_single_type (i + 1) in
      read_single_value t (i + len + 2)

  and read_single_value t i = match t with
    | T.Basic(t) ->
        let i, x = read_basic_value t i in
          (i, V.Basic x)
    | T.Array(t) ->
        let i, x = read_array (read_single_value t) [] (fun x l -> x :: l) i in
          (i, V.Array(t, x))
    | T.Dict(tk, tv) ->
        let i, x = read_dict (read_basic_value tk) (read_single_value tv) [] (fun k v l -> (k, v) :: l) i in
          (i, V.Dict(tk, tv, x))
    | T.Structure(t) ->
        let i, x = read_structure (read_value t) i in
          (i, V.Structure x)
    | T.Variant ->
        let i, x = read_variant i in
          (i, V.Variant x)

  and read_value t i =
    let i, v = List.fold_left begin fun (i,acc) t ->
      let i, v = read_single_value t i in
        i, v :: acc
    end (i, []) t in
      (i, List.rev v)

  let read_fixed_variant signature reader i =
    let i, s = read_signature_as_string i in
      if s <> signature
      then raise (Read_error "wrong variant signature")
      else reader i
end

module Writer(BO : ByteOrder)(Buffer : BufferType) =
struct
  open BO
  open Buffer

  let pad1 x = x

  (* Pad bytes must not be left uninitialized *)
  let pad2 i =
    let len = i mod 2 in
      String.fill buffer i len '\x00';
      i + len

  let pad4 i =
    let len = 3 - (i-1) mod 4 in
      String.fill buffer i len '\x00';
      i + len

  let pad8 i =
    let len = 7 - (i-1) mod 8 in
      String.fill buffer i len '\x00';
      i + len

  let pad_basic = function
    | T.Byte -> pad1
    | T.Boolean -> pad4
    | T.Int16 -> pad2
    | T.Int32 -> pad4
    | T.Int64 -> pad8
    | T.Uint16 -> pad2
    | T.Uint32 -> pad4
    | T.Uint64 -> pad8
    | T.Double -> pad8
    | T.String -> pad4
    | T.Signature -> pad1
    | T.Object_path -> pad4

  let pad_single = function
    | T.Basic(t) -> pad_basic t
    | T.Array _ -> pad4
    | T.Dict _ -> pad4
    | T.Structure _ -> pad8
    | T.Variant -> pad1

  let write_int16 v i =
    let i = pad2 i in
      buffer.[i + data16_bit0] <- char_of_int (v land 0xff);
      buffer.[i + data16_bit1] <- char_of_int ((v lsr 8) land 0xff);
      i + 2

  let write_uint16 = write_int16

  let write_int32 v i =
    let i = pad4 i in
      buffer.[i + data32_bit0] <- char_of_int (v land 0xff);
      buffer.[i + data32_bit1] <- char_of_int ((v lsr 8) land 0xff);
      buffer.[i + data32_bit2] <- char_of_int ((v lsr 16) land 0xff);
      buffer.[i + data32_bit3] <- char_of_int ((v lsr 24) land 0xff);
      i + 4

  let write_uint32 = write_int32

  let write_byte v i =
    buffer.[i] <- v;
    i + 1

  let write_boolean v i = write_int32 (if v then 1 else 0) i

  let write_int32_from_int32 v i =
    let i = pad4 i in
      buffer.[i + data32_bit0] <- char_of_int (Int32.to_int v land 0xff);
      buffer.[i + data32_bit1] <- char_of_int (Int32.to_int (Int32.shift_right v 8) land 0xff);
      buffer.[i + data32_bit2] <- char_of_int (Int32.to_int (Int32.shift_right v 16) land 0xff);
      buffer.[i + data32_bit3] <- char_of_int (Int32.to_int (Int32.shift_right v 24) land 0xff);
      i + 4

  let write_uint32_from_int32 = write_int32_from_int32

  let write_int64_from_int64 v i =
    let i = pad8 i in
      buffer.[i + data64_bit0] <- char_of_int (Int64.to_int v land 0xff);
      buffer.[i + data64_bit1] <- char_of_int (Int64.to_int (Int64.shift_right v 8) land 0xff);
      buffer.[i + data64_bit2] <- char_of_int (Int64.to_int (Int64.shift_right v 16) land 0xff);
      buffer.[i + data64_bit3] <- char_of_int (Int64.to_int (Int64.shift_right v 24) land 0xff);
      buffer.[i + data64_bit4] <- char_of_int (Int64.to_int (Int64.shift_right v 64) land 0xff);
      buffer.[i + data64_bit5] <- char_of_int (Int64.to_int (Int64.shift_right v 40) land 0xff);
      buffer.[i + data64_bit6] <- char_of_int (Int64.to_int (Int64.shift_right v 48) land 0xff);
      buffer.[i + data64_bit7] <- char_of_int (Int64.to_int (Int64.shift_right v 56) land 0xff);
      i + 8

  let write_uint64_from_int64 = write_int64_from_int64
  let write_int64 v i = write_int64_from_int64 (Int64.of_int v) i
  let write_uint64 v i = write_uint64_from_int64 (Int64.of_int v) i

  let write_double v i = write_int64_from_int64 (Int64.of_float v) i

  let write_string s i =
    let len = String.length s in
    let i = write_int32 len i in
      String.blit s 0 buffer i len;
      buffer.[i+len] <- '\x00';
      i+len+1

  let write_basic_type v =
    write_byte begin match v with
      | T.Byte -> 'y'
      | T.Boolean -> 'b'
      | T.Int16 -> 'n'
      | T.Uint16 -> 'q'
      | T.Int32 -> 'i'
      | T.Uint32 -> 'u'
      | T.Int64 -> 'x'
      | T.Uint64 -> 't'
      | T.Double -> 'd'
      | T.String -> 's'
      | T.Object_path -> 'o'
      | T.Signature -> 'g'
    end

  let rec write_single_type v i = match v with
    | T.Basic(t) -> write_basic_type t i
    | T.Array(t) -> write_single_type t (write_byte 'a' i)
    | T.Dict(tk, tv) ->
        write_byte '}'
          (write_single_type tv
             (write_basic_type tk
                (write_byte '{' (write_byte 'a' i))))
    | T.Structure(t) -> write_byte ')' (List.fold_left (fun i t -> write_single_type t i) (write_byte '(' i) t)
    | T.Variant -> write_byte 'v' i

  and write_type v i =
    write_byte '\x00' (List.fold_left (fun i t -> write_single_type t i) i v)

  let write_signature s i =
    let j = i + 1 in
    let k = write_type s j in
      buffer.[i] <- char_of_int (k-j);
      k

  let write_signature_from_string s i =
    let len = String.length s in
      buffer.[i] <- char_of_int len;
      let i = i + 1 in
        String.blit s 0 buffer i len;
        buffer.[i + len] <- '\x00';
        i + len + 1

  let write_object_path = write_string

  let write_basic_value = function
    | V.Byte x -> write_byte x
    | V.Boolean x -> write_boolean x
    | V.Int16 x -> write_int16 x
    | V.Uint16 x -> write_int16 x
    | V.Int32 x -> write_int32_from_int32 x
    | V.Uint32 x -> write_int32_from_int32 x
    | V.Int64 x -> write_int64_from_int64 x
    | V.Uint64 x -> write_int64_from_int64  x
    | V.Double x -> write_double x
    | V.String x -> write_string x
    | V.Signature x -> write_signature x
    | V.Object_path x -> write_object_path x

  let rec write_array elt_writer fold v i =
    (* A marshaled array correspond to the length in bytes of the
       marshaled array followed by the marshaled array contents *)
    let i = pad4 i in
    let j = i + 4 in
    let k = fold (fun v i -> elt_writer v i) j v in
    let len = k - j in
      if len > max_array_size
      then raise (Write_error "array too big!")
      else begin
        ignore(write_int32 len i);
        k
      end

  let write_dict key_writer val_writer fold v i =
    let i = pad4 i in
    let j = pad8 (i+4) in
    let k = fold
      (fun k v i -> val_writer v (key_writer k (pad8 i))) j v in
    let len = k - j in
      if len > max_array_size
      then raise (Write_error "array too big!")
      else begin
        ignore(write_int32 len i);
        k
      end

  let write_structure writer v i = writer v (pad8 i)

  let rec write_variant v i =
    let j = i + 1 in
    let k = write_single_type (V.type_of_single v) j in
      buffer.[i] <- char_of_int (k-j);
      write_single_value v k

  and write_single_value = function
      | V.Basic x ->
          write_basic_value x
      | V.Array(_, v) ->
          write_array write_single_value
            (fun f x l -> List.fold_left (fun i v -> f v i) x l) v
      | V.Dict(_, _, v) ->
          write_dict write_basic_value write_single_value
            (fun f x l -> List.fold_left (fun i (k, v) -> f k v i) x l) v
      | V.Structure(v) ->
          write_structure write_value v
      | V.Variant(v) ->
          write_variant v

  and write_value v i =
    List.fold_left (fun i t -> write_single_value t i) i v

  let write_variant = write_single_value

  let write_fixed_variant signature writer v i =
    writer v (write_signature_from_string signature i)
end

type 'a cookie = int

type bus = {
  fd : Unix.file_descr;
  buffer : string;
  mutable next_serial : int32;
}

open Header

module WriteH(BO : ByteOrder)(Buffer : BufferType) =
struct
  module W = Writer(BO)(Buffer)
  let write header i =
    let i = W.write_byte begin match header.byte_order with
      | LittleEndian -> 'l'
      | BigEndian -> 'B'
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
    let j = W.write_byte (char_of_int header.protocol_version) i in
    let i = W.write_uint32 header.length j in
    let i = W.write_uint32_from_int32 header.serial i in
    let i = W.write_array begin fun v i ->
      W.write_structure begin fun v i -> match v with
        | Path(x) -> let i = W.write_byte '\x01' i in W.write_fixed_variant "o" W.write_object_path x i
        | Interface(x) -> let i = W.write_byte '\x02' i in W.write_fixed_variant "s" W.write_string x i
        | Member(x) -> let i = W.write_byte '\x03' i in W.write_fixed_variant "s" W.write_string x i
        | Error_name(x) -> let i = W.write_byte '\x04' i in W.write_fixed_variant "s" W.write_string x i
        | Reply_serial(x) -> let i = W.write_byte '\x05' i in W.write_fixed_variant "u" W.write_int32_from_int32 x i
        | Destination(x) -> let i = W.write_byte '\x06' i in W.write_fixed_variant "s" W.write_string x i
        | Sender(x) -> let i = W.write_byte '\x07' i in W.write_fixed_variant "s" W.write_string x i
        | Signature(x) -> let i = W.write_byte '\x08' i in W.write_fixed_variant "g" W.write_signature_from_string x i
      end v i
    end (fun f x l -> List.fold_left (fun acc v -> f v acc) x l) header.fields i
    in
      ((fun length -> ignore (W.write_uint32 length j)), W.pad8 i)
end

module WriteHLE = WriteH(LittleEndian)
module WriteHBE = WriteH(BigEndian)

let write_header buffer header i =
  let module Buffer = struct let buffer = buffer end in
    match header.byte_order with
      | LittleEndian -> let module W = WriteHLE(Buffer) in
          W.write header i
      | BigEndian -> let module W = WriteHBE(Buffer) in
          W.write header i

module ReadH(BO : ByteOrder)(Buffer : BufferType) =
struct
  module R = Reader(BO)(Buffer)
  let read i =
    let i, byte_order = R.read_byte i in
    let i, message_type = R.read_byte i in
    let i, flags = R.read_byte i in
    let i, protocol_version = R.read_byte i in
    let i, length = R.read_uint32 i in
    let i, serial = R.read_uint32_as_int32 i in
    let i, fields = R.read_array begin fun i ->
      R.read_structure begin fun i ->
        let i, code = R.read_byte i in
          match code with
            | '\x01' -> let i, x = R.read_fixed_variant "o" R.read_object_path i in (i, Path(x))
            | '\x02' -> let i, x = R.read_fixed_variant "s" R.read_string i in (i, Interface(x))
            | '\x03' -> let i, x = R.read_fixed_variant "s" R.read_string i in (i, Member(x))
            | '\x04' -> let i, x = R.read_fixed_variant "s" R.read_string i in (i, Error_name(x))
            | '\x05' -> let i, x = R.read_fixed_variant "u" R.read_uint32_as_int32 i in (i, Reply_serial(x))
            | '\x06' -> let i, x = R.read_fixed_variant "s" R.read_string i in (i, Destination(x))
            | '\x07' -> let i, x = R.read_fixed_variant "s" R.read_string i in (i, Sender(x))
            | '\x08' -> let i, x = R.read_fixed_variant "g" R.read_signature_as_string i in (i, Signature(x))
            | code when code < '\x01' || code > '\x08' -> failwith "unknown header field code"
            | _ -> let _, x = R.read_variant i in
                failwith (Printf.sprintf "malformed header field(code = %d, value = %s)"
                            (int_of_char code)
                            (V.string_of_single x))
      end i
    end [] (fun e l -> e :: l) i in
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
         fields = fields })
end

module ReadHLE = ReadH(LittleEndian)
module ReadHBE = ReadH(BigEndian)

let read_header buffer i =
  let module Buffer = struct let buffer = buffer end in
    match buffer.[i] with
      | 'l' -> let module R = ReadHLE(Buffer) in
          R.read i
      | 'B' -> let module R = ReadHBE(Buffer) in
          R.read i
      | _ -> failwith (Printf.sprintf "invalid byte order %c" buffer.[i])

module H = Header

let send_message bus hfields msig rsig writer reader =
  let id = bus.next_serial in
    bus.next_serial <- Int32.succ id;
    String.fill bus.buffer 0 (String.length bus.buffer) '\x00';
    let f, i = write_header bus.buffer
      { H.byte_order = H.LittleEndian;
        H.typ = H.Method_call;
        H.flags = [ReplyExpected; AutoStart];
        H.protocol_version = 1;
        H.length = 0;
        H.serial = id;
        H.fields = hfields } 0 in
    let j = writer LittleEndian bus.buffer i in
      f (j - i);
      let h = open_out "/tmp/toto" in
        output_string h (String.sub bus.buffer 0 j);
        close_out h;
      Printf.printf "send = %d\n" (Unix.write bus.fd bus.buffer 0 j);
      let c = (Unix.read bus.fd bus.buffer 0 5000) in
      Printf.printf "recv = %d\n" c;
      let h = open_out "/tmp/titi" in
        output_string h (String.sub bus.buffer 0 c);
        close_out h;
      let i, header = read_header bus.buffer 0 in
        Printf.printf "length = %d\n" header.length;
      let i, v = reader header.H.byte_order bus.buffer i in
        v

open Unix

let rec open_bus = function
  | [] -> failwith "cannot connect"
  | (name, params) :: tl -> match name with
      | "unix" ->
          begin match match (Util.assoc "abstract" params,
		             Util.assoc "path" params) with
	    | None, None
	    | Some _, Some _ -> None
	    | Some abstract, None -> Some("\x00" ^ abstract)
	    | None, Some path -> Some(path)
          with
            | Some(addr) -> begin try
                let fd = socket PF_UNIX SOCK_STREAM 0 in
                  connect fd (ADDR_UNIX(addr));
                  match Auth.do_auth fd with
                    | Auth.Success -> { fd = fd; buffer = String.create 65536; next_serial = Int32.one }
                    | Auth.Failure _ -> open_bus tl
              with
                  _ -> open_bus tl
              end
            | None -> open_bus tl
          end
      | _ -> open_bus tl
