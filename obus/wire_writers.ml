(*
 * wire_writers.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a t = string -> int -> 'a -> unit

let int_int16 buffer i v =
  String.unsafe_set buffer (i + (BIT(16, 0))) (Char.unsafe_chr (v land 0xff));
  String.unsafe_set buffer (i + (BIT(16, 1))) (Char.unsafe_chr ((v lsr 8) land 0xff))
let int_uint16 = int_int16

let int_int32 buffer i v =
  String.unsafe_set buffer (i + (BIT(32, 0))) (Char.unsafe_chr (v land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 1))) (Char.unsafe_chr ((v lsr 8) land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 2))) (Char.unsafe_chr ((v lsr 16) land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 3))) (Char.unsafe_chr ((v lsr 24) land 0xff))
let int_uint32 = int_int32

let int_int32 buffer i v =
  String.unsafe_set buffer (i + (BIT(32, 0))) (Char.unsafe_chr (v land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 1))) (Char.unsafe_chr ((v lsr 8) land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 2))) (Char.unsafe_chr ((v lsr 16) land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 3))) (Char.unsafe_chr ((v lsr 24) land 0xff))
let int_uint32 = int_int32

let int_int64 buffer i v =
  String.unsafe_set buffer (i + (BIT(64, 0))) (Char.unsafe_chr (v land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 1))) (Char.unsafe_chr ((v lsr 8) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 2))) (Char.unsafe_chr ((v lsr 16) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 3))) (Char.unsafe_chr ((v lsr 24) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 4))) (Char.unsafe_chr ((v lsr 32) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 5))) (Char.unsafe_chr ((v lsr 40) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 6))) (Char.unsafe_chr ((v lsr 48) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 7))) (Char.unsafe_chr ((v lsr 56) land 0xff))
let int_uint64 = int_int64

let int32_int32 buffer i v =
  String.unsafe_set buffer (i + (BIT(32, 0))) (Char.unsafe_chr (Int32.to_int v land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 1))) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8) land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 2))) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16) land 0xff));
  String.unsafe_set buffer (i + (BIT(32, 3))) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24) land 0xff))
let int32_uint32 = int32_int32

let int64_int64 buffer i v =
  String.unsafe_set buffer (i + (BIT(64, 0))) (Char.unsafe_chr (Int64.to_int v land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 1))) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 2))) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 3))) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 4))) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 64) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 5))) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 6))) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48) land 0xff));
  String.unsafe_set buffer (i + (BIT(64, 7))) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56) land 0xff))
let int64_uint64 = int64_int64

let float_double buffer i v = int64_int64 buffer i (Int64.of_float v)
