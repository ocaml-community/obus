(*
 * marshaler.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type ptr = int
type buffer = string
module type Writer = sig
  type 'a t = ptr -> 'a -> unit
  val buffer : buffer
  val pad2 : ptr -> ptr
  val pad4 : ptr -> ptr
  val pad8 : ptr -> ptr
  val bool_boolean : bool t
  val int_int16 : int t
  val int_int32 : int t
  val int_int64 : int t
  val int_uint16 : int t
  val int_uint32 : int t
  val int_uint64 : int t
  val int32_int32 : int32 t
  val int64_int64 : int64 t
  val int32_uint32 : int32 t
  val int64_uint64 : int64 t
  val float_double : float t
  val string_string : ptr -> string -> ptr
  val string_signature : ptr -> string -> ptr
end
module type Reader = sig
  type 'a t = ptr -> 'a
  val buffer : buffer
  val pad2 : ptr -> ptr
  val pad4 : ptr -> ptr
  val pad8 : ptr -> ptr
  val bool_boolean : bool t
  val int_int16 : int t
  val int_int32 : int t
  val int_int64 : int t
  val int_uint16 : int t
  val int_uint32 : int t
  val int_uint64 : int t
  val int32_int32 : int32 t
  val int64_int64 : int64 t
  val int32_uint32 : int32 t
  val int64_uint64 : int64 t
  val float_double : float t
  val string_string : ptr -> ptr * string
  val string_signature : ptr -> ptr * string
end
module type Buffer = sig val buffer : buffer end
exception Write_error of string
exception Read_error of string

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

module LE : ByteOrder =
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

module BE : ByteOrder =
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

module Writer(BO : ByteOrder)(B : Buffer) =
struct
  open BO
  type 'a t = ptr -> 'a -> unit

  let buffer = B.buffer

  (* Pad bytes must not be left uninitialized *)
  let pad2 i =
    let len = i land 1 in
      String.fill buffer i len '\x00';
      i + len

  let pad4 i =
    let len = (4 - i) land 3 in
      String.fill buffer i len '\x00';
      i + len

  let pad8 i =
    let len = (8 - i) land 7 in
      String.fill buffer i len '\x00';
      i + len

  let int_int16 i v =
    buffer.[i + data16_bit0] <- char_of_int (v land 0xff);
    buffer.[i + data16_bit1] <- char_of_int ((v lsr 8) land 0xff)
  let int_uint16 = int_int16

  let int_int32 i v =
    buffer.[i + data32_bit0] <- char_of_int (v land 0xff);
    buffer.[i + data32_bit1] <- char_of_int ((v lsr 8) land 0xff);
    buffer.[i + data32_bit2] <- char_of_int ((v lsr 16) land 0xff);
    buffer.[i + data32_bit3] <- char_of_int ((v lsr 24) land 0xff)
  let int_uint32 = int_int32

  let int_int32 i v =
    buffer.[i + data32_bit0] <- char_of_int (v land 0xff);
    buffer.[i + data32_bit1] <- char_of_int ((v lsr 8) land 0xff);
    buffer.[i + data32_bit2] <- char_of_int ((v lsr 16) land 0xff);
    buffer.[i + data32_bit3] <- char_of_int ((v lsr 24) land 0xff)
  let int_uint32 = int_int32

  let int_int64 i v =
    buffer.[i + data64_bit0] <- char_of_int (v land 0xff);
    buffer.[i + data64_bit1] <- char_of_int ((v lsr 8) land 0xff);
    buffer.[i + data64_bit2] <- char_of_int ((v lsr 16) land 0xff);
    buffer.[i + data64_bit3] <- char_of_int ((v lsr 24) land 0xff);
    buffer.[i + data64_bit1] <- char_of_int ((v lsr 32) land 0xff);
    buffer.[i + data64_bit2] <- char_of_int ((v lsr 40) land 0xff);
    buffer.[i + data64_bit3] <- char_of_int ((v lsr 48) land 0xff);
    buffer.[i + data64_bit3] <- char_of_int ((v lsr 56) land 0xff)
  let int_uint64 = int_int64

  let bool_boolean i v = int_int32 (if v then 1 else 0) i

  let int32_int32 i v =
    buffer.[i + data32_bit0] <- char_of_int (Int32.to_int v land 0xff);
    buffer.[i + data32_bit1] <- char_of_int (Int32.to_int (Int32.shift_right v 8) land 0xff);
    buffer.[i + data32_bit2] <- char_of_int (Int32.to_int (Int32.shift_right v 16) land 0xff);
    buffer.[i + data32_bit3] <- char_of_int (Int32.to_int (Int32.shift_right v 24) land 0xff)
  let int32_uint32 = int32_int32

  let int64_int64 i v =
    buffer.[i + data64_bit0] <- char_of_int (Int64.to_int v land 0xff);
    buffer.[i + data64_bit1] <- char_of_int (Int64.to_int (Int64.shift_right v 8) land 0xff);
    buffer.[i + data64_bit2] <- char_of_int (Int64.to_int (Int64.shift_right v 16) land 0xff);
    buffer.[i + data64_bit3] <- char_of_int (Int64.to_int (Int64.shift_right v 24) land 0xff);
    buffer.[i + data64_bit4] <- char_of_int (Int64.to_int (Int64.shift_right v 64) land 0xff);
    buffer.[i + data64_bit5] <- char_of_int (Int64.to_int (Int64.shift_right v 40) land 0xff);
    buffer.[i + data64_bit6] <- char_of_int (Int64.to_int (Int64.shift_right v 48) land 0xff);
    buffer.[i + data64_bit7] <- char_of_int (Int64.to_int (Int64.shift_right v 56) land 0xff)
  let int64_uint64 = int64_int64

  let float_double i v = int64_int64 i (Int64.of_float v)

  let string_string i str =
    let len = String.length str in
      int_uint32 i len;
      String.blit str 0 buffer (i + 4) len;
      let i = i + 4 + len in
        buffer.[i] <- '\x00';
        i + 1

  let string_signature i str =
    let len = String.length str in
      buffer.[i] <- char_of_int len;
      String.blit str 0 buffer (i + 1) len;
      let i = i + 1 + len in
        buffer.[i] <- '\x00';
        i + 1
end

module Reader(BO : ByteOrder)(B : Buffer) =
struct
  open BO
  type 'a t = ptr -> 'a
  let buffer = B.buffer

  let pad2 i = i + (i land 1)
  let pad4 i = i + ((4 - i) land 3)
  let pad8 i = i + ((8 - i) land 7)

  let int_int16 i =
    let v0 = int_of_char (buffer.[i + data16_bit0])
    and v1 = int_of_char (buffer.[i + data16_bit0]) in
    let v = v0 land (v1 lsl 8) in
      if v land (1 lsl 15) = 0
      then v
      else (-1 land (lnot 0x7fff)) lor v

  let int_uint16 i =
    let v0 = int_of_char buffer.[i + data16_bit0]
    and v1 = int_of_char buffer.[i + data16_bit1] in
      v0 lor (v1 lsl 8)

  let int_int32 i =
    let v0 = int_of_char (buffer.[i + data32_bit0])
    and v1 = int_of_char (buffer.[i + data32_bit1])
    and v2 = int_of_char (buffer.[i + data32_bit2])
    and v3 = int_of_char (buffer.[i + data32_bit3]) in
    let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
      if v land (1 lsl 31) = 0
      then v
      else (-1 land (lnot 0x7fffffff)) lor v

  let int_uint32 i =
    let v0 = int_of_char (buffer.[i + data32_bit0])
    and v1 = int_of_char (buffer.[i + data32_bit1])
    and v2 = int_of_char (buffer.[i + data32_bit2])
    and v3 = int_of_char (buffer.[i + data32_bit3]) in
      v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24)

  let int_int64 i =
    let v0 = int_of_char (buffer.[i + data64_bit0])
    and v1 = int_of_char (buffer.[i + data64_bit1])
    and v2 = int_of_char (buffer.[i + data64_bit2])
    and v3 = int_of_char (buffer.[i + data64_bit3])
    and v4 = int_of_char (buffer.[i + data64_bit4])
    and v5 = int_of_char (buffer.[i + data64_bit5])
    and v6 = int_of_char (buffer.[i + data64_bit6])
    and v7 = int_of_char (buffer.[i + data64_bit7]) in
      (* This is not correct on a 128 bits... *)
      v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24)
      lor (v4 lsl 32) lor (v5 lsl 40) lor (v6 lsl 48) lor (v7 lsl 56)
  let int_uint64 = int_int64

  let bool_boolean i = buffer.[i + data32_bit0] = '\001'

  let int32_int32 i =
    let v0 = Int32.of_int (int_of_char (buffer.[i + data32_bit0]))
    and v1 = Int32.of_int (int_of_char (buffer.[i + data32_bit1]))
    and v2 = Int32.of_int (int_of_char (buffer.[i + data32_bit2]))
    and v3 = Int32.of_int (int_of_char (buffer.[i + data32_bit3])) in
      Int32.logor
        (Int32.logor
           v0
           (Int32.shift_left v1 8))
        (Int32.logor
           (Int32.shift_left v2 16)
           (Int32.shift_left v3 24))
  let int32_uint32 = int32_int32

  let int64_int64 i =
    let v0 = Int64.of_int (int_of_char (buffer.[i + data64_bit0]))
    and v1 = Int64.of_int (int_of_char (buffer.[i + data64_bit1]))
    and v2 = Int64.of_int (int_of_char (buffer.[i + data64_bit2]))
    and v3 = Int64.of_int (int_of_char (buffer.[i + data64_bit3]))
    and v4 = Int64.of_int (int_of_char (buffer.[i + data64_bit4]))
    and v5 = Int64.of_int (int_of_char (buffer.[i + data64_bit5]))
    and v6 = Int64.of_int (int_of_char (buffer.[i + data64_bit6]))
    and v7 = Int64.of_int (int_of_char (buffer.[i + data64_bit7])) in
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
              (Int64.shift_left v7 56)))
  let int64_uint64 = int64_int64

  let float_double i = Int64.to_float (int64_uint64 i)

  let string_string i =
    let len = int_uint32 i in
      (i + 5 + len, String.sub buffer (i + 4) len)

  let string_signature i =
    let len = int_of_char buffer.[i] in
      (i + 2 + len, String.sub buffer (i + 1) len)
end

let rec read_until f x limit i =
  if i = limit
  then (i, x)
  else if i < limit
  then let j, y = f i x in
    read_until f y limit j
  else raise (Read_error "wrong array size!")

module LEWriter = Writer(LE)
module BEWriter = Writer(BE)
module LEReader = Reader(LE)
module BEReader = Reader(BE)
