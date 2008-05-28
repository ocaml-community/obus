(*
 * wire_readers.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a t = string -> int -> 'a

let int_int16 buffer i =
  let v0 = Char.code (String.unsafe_get buffer (i + (BIT(16, 0))))
  and v1 = Char.code (String.unsafe_get buffer (i + (BIT(16, 0)))) in
  let v = v0 land (v1 lsl 8) in
    if v land (1 lsl 15) = 0
    then v
    else (-1 land (lnot 0x7fff)) lor v

let int_uint16 buffer i =
  let v0 = Char.code (String.unsafe_get buffer (i + (BIT(16, 0))))
  and v1 = Char.code (String.unsafe_get buffer (i + (BIT(16, 1)))) in
    v0 lor (v1 lsl 8)

let int_int32 buffer i =
  let v0 = Char.code (String.unsafe_get buffer (i + (BIT(32, 0))))
  and v1 = Char.code (String.unsafe_get buffer (i + (BIT(32, 1))))
  and v2 = Char.code (String.unsafe_get buffer (i + (BIT(32, 2))))
  and v3 = Char.code (String.unsafe_get buffer (i + (BIT(32, 3)))) in
  let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
    if v land (1 lsl 31) = 0
    then v
    else (-1 land (lnot 0x7fffffff)) lor v

let int_uint32 buffer i =
  let v0 = Char.code (String.unsafe_get buffer (i + (BIT(32, 0))))
  and v1 = Char.code (String.unsafe_get buffer (i + (BIT(32, 1))))
  and v2 = Char.code (String.unsafe_get buffer (i + (BIT(32, 2))))
  and v3 = Char.code (String.unsafe_get buffer (i + (BIT(32, 3)))) in
    v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24)

let int_int64 buffer i =
  let v0 = Char.code (String.unsafe_get buffer (i + (BIT(64, 0))))
  and v1 = Char.code (String.unsafe_get buffer (i + (BIT(64, 1))))
  and v2 = Char.code (String.unsafe_get buffer (i + (BIT(64, 2))))
  and v3 = Char.code (String.unsafe_get buffer (i + (BIT(64, 3))))
  and v4 = Char.code (String.unsafe_get buffer (i + (BIT(64, 4))))
  and v5 = Char.code (String.unsafe_get buffer (i + (BIT(64, 5))))
  and v6 = Char.code (String.unsafe_get buffer (i + (BIT(64, 6))))
  and v7 = Char.code (String.unsafe_get buffer (i + (BIT(64, 7)))) in
    (* This is not correct on a 128 bits... *)
    v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24)
    lor (v4 lsl 32) lor (v5 lsl 40) lor (v6 lsl 48) lor (v7 lsl 56)
let int_uint64 = int_int64

let int32_int32 buffer i =
  let v0 = Int32.of_int (Char.code (String.unsafe_get buffer (i + (BIT(32, 0)))))
  and v1 = Int32.of_int (Char.code (String.unsafe_get buffer (i + (BIT(32, 1)))))
  and v2 = Int32.of_int (Char.code (String.unsafe_get buffer (i + (BIT(32, 2)))))
  and v3 = Int32.of_int (Char.code (String.unsafe_get buffer (i + (BIT(32, 3))))) in
    Int32.logor
      (Int32.logor
         v0
         (Int32.shift_left v1 8))
      (Int32.logor
         (Int32.shift_left v2 16)
         (Int32.shift_left v3 24))
let int32_uint32 = int32_int32

let int64_int64 buffer i =
  let v0 = Int64.of_int (Char.code (String.unsafe_get buffer (i + (BIT(64, 0)))))
  and v1 = Int64.of_int (Char.code (String.unsafe_get buffer (i + (BIT(64, 1)))))
  and v2 = Int64.of_int (Char.code (String.unsafe_get buffer (i + (BIT(64, 2)))))
  and v3 = Int64.of_int (Char.code (String.unsafe_get buffer (i + (BIT(64, 3)))))
  and v4 = Int64.of_int (Char.code (String.unsafe_get buffer (i + (BIT(64, 4)))))
  and v5 = Int64.of_int (Char.code (String.unsafe_get buffer (i + (BIT(64, 5)))))
  and v6 = Int64.of_int (Char.code (String.unsafe_get buffer (i + (BIT(64, 6)))))
  and v7 = Int64.of_int (Char.code (String.unsafe_get buffer (i + (BIT(64, 7))))) in
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

let float_double buffer i = Int64.to_float (int64_uint64 buffer i)
