(*
 * wire.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Low-level, unsafe serialization/deserialization *)

exception Out_of_bounds
exception Reading_error of string
exception Writing_error of string

let out_of_bounds _ = raise Out_of_bounds

let write_check_array_len len =
  if len < 0 || len > OBus_info.max_array_size
  then raise (Writing_error (Printf.sprintf "array too big to be send: %d" len))

let read_check_array_len len =
  if len < 0 || len > OBus_info.max_array_size
  then raise (Reading_error (Printf.sprintf "array size exceed the limit: %d" len))

module type Byte_order =
sig
  val byte_order_char : char
  val byte_order : OBus_info.byte_order
  val data16bit0 : int
  val data16bit1 : int
  val data32bit0 : int
  val data32bit1 : int
  val data32bit2 : int
  val data32bit3 : int
  val data64bit0 : int
  val data64bit1 : int
  val data64bit2 : int
  val data64bit3 : int
  val data64bit4 : int
  val data64bit5 : int
  val data64bit6 : int
  val data64bit7 : int
end

module Little_endian =
struct
  let byte_order_char = 'l'
  let byte_order = OBus_info.Little_endian
  let data16bit0 = 0
  let data16bit1 = 1
  let data32bit0 = 0
  let data32bit1 = 1
  let data32bit2 = 2
  let data32bit3 = 3
  let data64bit0 = 0
  let data64bit1 = 1
  let data64bit2 = 2
  let data64bit3 = 3
  let data64bit4 = 4
  let data64bit5 = 5
  let data64bit6 = 6
  let data64bit7 = 7
end

module Big_endian =
struct
  let byte_order_char = 'B'
  let byte_order = OBus_info.Big_endian
  let data16bit0 = 1
  let data16bit1 = 0
  let data32bit0 = 3
  let data32bit1 = 2
  let data32bit2 = 1
  let data32bit3 = 0
  let data64bit0 = 7
  let data64bit1 = 6
  let data64bit2 = 5
  let data64bit3 = 4
  let data64bit4 = 3
  let data64bit5 = 2
  let data64bit6 = 1
  let data64bit7 = 0
end

let unsafe_write_char_as_byte v buffer i =
  String.unsafe_set buffer i v

let unsafe_write_int_as_byte v buffer i =
  String.unsafe_set buffer i (Char.unsafe_chr v)

module Make_unsafe_writer(BO : Byte_order) =
struct
  open BO

  let unsafe_write_int_as_int16 v buffer i =
    String.unsafe_set buffer (i + data16bit0) (Char.unsafe_chr v);
    String.unsafe_set buffer (i + data16bit1) (Char.unsafe_chr (v lsr 8))
  let unsafe_write_int_as_uint16 = unsafe_write_int_as_int16

  let unsafe_write_int_as_int32 v buffer i =
    String.unsafe_set buffer (i + data32bit0) (Char.unsafe_chr v);
    String.unsafe_set buffer (i + data32bit1) (Char.unsafe_chr (v lsr 8));
    String.unsafe_set buffer (i + data32bit2) (Char.unsafe_chr (v lsr 16));
    String.unsafe_set buffer (i + data32bit3) (Char.unsafe_chr (v asr 24))
  let unsafe_write_int_as_uint32 = unsafe_write_int_as_int32

  let unsafe_write_int_as_int64 v buffer i =
    String.unsafe_set buffer (i + data64bit0) (Char.unsafe_chr v);
    String.unsafe_set buffer (i + data64bit1) (Char.unsafe_chr (v lsr 8));
    String.unsafe_set buffer (i + data64bit2) (Char.unsafe_chr (v lsr 16));
    String.unsafe_set buffer (i + data64bit3) (Char.unsafe_chr (v asr 24));
    String.unsafe_set buffer (i + data64bit4) (Char.unsafe_chr (v asr 32));
    String.unsafe_set buffer (i + data64bit5) (Char.unsafe_chr (v asr 40));
    String.unsafe_set buffer (i + data64bit6) (Char.unsafe_chr (v asr 48));
    String.unsafe_set buffer (i + data64bit7) (Char.unsafe_chr (v asr 56))
  let unsafe_write_int_as_uint64 = unsafe_write_int_as_int64

  let unsafe_write_int32_as_int32 v buffer i =
    String.unsafe_set buffer (i + data32bit0) (Char.unsafe_chr (Int32.to_int v));
    String.unsafe_set buffer (i + data32bit1) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
    String.unsafe_set buffer (i + data32bit2) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
    String.unsafe_set buffer (i + data32bit3) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24)))
  let unsafe_write_int32_as_uint32 = unsafe_write_int32_as_int32

  let unsafe_write_int64_as_int64 v buffer i =
    String.unsafe_set buffer (i + data64bit0) (Char.unsafe_chr (Int64.to_int v));
    String.unsafe_set buffer (i + data64bit1) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8)));
    String.unsafe_set buffer (i + data64bit2) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16)));
    String.unsafe_set buffer (i + data64bit3) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24)));
    String.unsafe_set buffer (i + data64bit4) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32)));
    String.unsafe_set buffer (i + data64bit5) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40)));
    String.unsafe_set buffer (i + data64bit6) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48)));
    String.unsafe_set buffer (i + data64bit7) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56)))
  let unsafe_write_int64_as_uint64 = unsafe_write_int64_as_int64
end

let unsafe_read_byte_as_char buffer i =
  String.unsafe_get buffer i

let unsafe_read_byte_as_int buffer i =
  Char.code (String.unsafe_get buffer i)

module Make_unsafe_reader(BO : Byte_order) =
struct
  open BO

  let unsafe_read_int16_as_int buffer i =
    let v0 = Char.code (String.unsafe_get buffer (i + data16bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data16bit0)) in
    let v = v0 land (v1 lsl 8) in
      if v land (1 lsl 15) = 0
      then v
      else (-1 land (lnot 0x7fff)) lor v

  let unsafe_read_uint16_as_int buffer i =
    let v0 = Char.code (String.unsafe_get buffer (i + data16bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data16bit1)) in
      v0 lor (v1 lsl 8)

  let unsafe_read_int32_as_int buffer i =
    let v0 = Char.code (String.unsafe_get buffer (i + data32bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data32bit1))
    and v2 = Char.code (String.unsafe_get buffer (i + data32bit2))
    and v3 = Char.code (String.unsafe_get buffer (i + data32bit3)) in
    let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
      if v land (1 lsl 31) = 0
      then v
      else (-1 land (lnot 0x7fffffff)) lor v

  let unsafe_read_uint32_as_int buffer i =
    let v0 = Char.code (String.unsafe_get buffer (i + data32bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data32bit1))
    and v2 = Char.code (String.unsafe_get buffer (i + data32bit2))
    and v3 = Char.code (String.unsafe_get buffer (i + data32bit3)) in
      v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24)

  let unsafe_read_int64_as_int buffer i =
    let v0 = Char.code (String.unsafe_get buffer (i + data64bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data64bit1))
    and v2 = Char.code (String.unsafe_get buffer (i + data64bit2))
    and v3 = Char.code (String.unsafe_get buffer (i + data64bit3))
    and v4 = Char.code (String.unsafe_get buffer (i + data64bit4))
    and v5 = Char.code (String.unsafe_get buffer (i + data64bit5))
    and v6 = Char.code (String.unsafe_get buffer (i + data64bit6))
    and v7 = Char.code (String.unsafe_get buffer (i + data64bit7)) in
      (* This is not correct on a 128 bits... *)
      v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24)
      lor (v4 lsl 32) lor (v5 lsl 40) lor (v6 lsl 48) lor (v7 lsl 56)
  let unsafe_read_uint64_as_int = unsafe_read_int64_as_int

  let unsafe_read_int32_as_int32 buffer i =
    let v0 = Int32.of_int (Char.code (String.unsafe_get buffer (i + data32bit0)))
    and v1 = Int32.of_int (Char.code (String.unsafe_get buffer (i + data32bit1)))
    and v2 = Int32.of_int (Char.code (String.unsafe_get buffer (i + data32bit2)))
    and v3 = Int32.of_int (Char.code (String.unsafe_get buffer (i + data32bit3))) in
      Int32.logor
        (Int32.logor
           v0
           (Int32.shift_left v1 8))
        (Int32.logor
           (Int32.shift_left v2 16)
           (Int32.shift_left v3 24))
  let unsafe_read_uint32_as_int32 = unsafe_read_int32_as_int32

  let unsafe_read_int64_as_int64 buffer i =
    let v0 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit0)))
    and v1 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit1)))
    and v2 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit2)))
    and v3 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit3)))
    and v4 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit4)))
    and v5 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit5)))
    and v6 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit6)))
    and v7 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit7))) in
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
  let unsafe_read_uint64_as_int64 = unsafe_read_int64_as_int64
end

module LEW = Make_unsafe_writer(Little_endian)
module BEW = Make_unsafe_writer(Big_endian)
module LER = Make_unsafe_reader(Little_endian)
module BER = Make_unsafe_reader(Big_endian)

let read_until reader empty len ctx i =
  let limit = i + len in
  let rec aux (i, acc) =
    if i < limit
    then aux (reader acc ctx i)
    else
      if i > limit
      then raise (Reading_error "invalid array size")
      else (i, acc)
  in
    aux (i, empty)

