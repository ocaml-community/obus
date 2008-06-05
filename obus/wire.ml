(*
 * wire.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type ptr = int
type buffer = string
type byte_order = Little_endian | Big_endian
exception Out_of_bounds
exception Content_error of string
exception Convertion_failed of exn
exception Reading_error of string
exception Writing_error of string
type 'a writer = buffer -> ptr -> 'a -> ptr
type 'a reader = buffer -> ptr -> ptr * 'a
module type Writer = sig
  val byte_order : byte_order
  val write_int_int16 : int writer
  val write_int_int32 : int writer
  val write_int_int64 : int writer
  val write_int_uint16 : int writer
  val write_int_uint32 : int writer
  val write_int_uint64 : int writer
  val write_int32_int32 : int32 writer
  val write_int64_int64 : int64 writer
  val write_int32_uint32 : int32 writer
  val write_int64_uint64 : int64 writer
  val write_float_double : float writer
  val write_bool_boolean : bool writer
  val write_string_string : string writer
  val write_string_object_path : string writer
  val write_array : 'a writer -> 'a writer
  val write_array8 : 'a writer -> 'a writer
end
module type Reader = sig
  val byte_order : byte_order
  val read_int_int16 : int reader
  val read_int_int32 : int reader
  val read_int_int64 : int reader
  val read_int_uint16 : int reader
  val read_int_uint32 : int reader
  val read_int_uint64 : int reader
  val read_int32_int32 : int32 reader
  val read_int64_int64 : int64 reader
  val read_int32_uint32 : int32 reader
  val read_int64_uint64 : int64 reader
  val read_float_double : float reader
  val read_bool_boolean : bool reader
  val read_string_string : string reader
  val read_string_object_path : string reader
  val read_array : (ptr -> buffer -> ptr -> 'a) -> 'a reader
  val read_array8 : (ptr -> buffer -> ptr -> 'a) -> 'a reader
end

open Printf

let out_of_bounds _ = raise Out_of_bounds
let write_array_too_big len =
  raise (Writing_error (sprintf "array too big to be send: %d" len))
let read_array_too_big len =
  raise (Reading_error (sprintf "array size exceed the limit: %d" len))

(* Padding functions.

   Since the size of the buffer is always a multiple of 8, adding
   null bytes for padding will never reach the end of the buffer.

   Note about the implementation: these function explicitly writes
   each padding bytes, and do not use [String.unsafe_fill]. After some
   tests it seems to be 4 times more efficient to do this that way.
*)

let wpad2 buffer i = match i land 1 with
  | 1 ->
      String.unsafe_set buffer i '\x00';
      i + 1
  | _ -> i

let wpad4 buffer i = match i land 3 with
  | 3 ->
      String.unsafe_set buffer i '\x00';
      i + 1
  | 2 ->
      String.unsafe_set buffer i '\x00';
      String.unsafe_set buffer (i+1) '\x00';
      i + 2
  | 1 ->
      String.unsafe_set buffer i '\x00';
      String.unsafe_set buffer (i+1) '\x00';
      String.unsafe_set buffer (i+2) '\x00';
      i + 3
  | _ -> i

let wpad8 buffer i = match i land 7 with
  | 7 ->
      String.unsafe_set buffer i '\x00';
      i + 1
  | 6 ->
      String.unsafe_set buffer i '\x00';
      String.unsafe_set buffer (i+1) '\x00';
      i + 2
  | 5 ->
      String.unsafe_set buffer i '\x00';
      String.unsafe_set buffer (i+1) '\x00';
      String.unsafe_set buffer (i+2) '\x00';
      i + 3
  | 4 ->
      String.unsafe_set buffer i '\x00';
      String.unsafe_set buffer (i+1) '\x00';
      String.unsafe_set buffer (i+2) '\x00';
      String.unsafe_set buffer (i+3) '\x00';
      i + 4
  | 3 ->
      String.unsafe_set buffer i '\x00';
      String.unsafe_set buffer (i+1) '\x00';
      String.unsafe_set buffer (i+2) '\x00';
      String.unsafe_set buffer (i+3) '\x00';
      String.unsafe_set buffer (i+4) '\x00';
      i + 5
  | 2 ->
      String.unsafe_set buffer i '\x00';
      String.unsafe_set buffer (i+1) '\x00';
      String.unsafe_set buffer (i+2) '\x00';
      String.unsafe_set buffer (i+3) '\x00';
      String.unsafe_set buffer (i+4) '\x00';
      String.unsafe_set buffer (i+5) '\x00';
      i + 6
  | 1 ->
      String.unsafe_set buffer i '\x00';
      String.unsafe_set buffer (i+1) '\x00';
      String.unsafe_set buffer (i+2) '\x00';
      String.unsafe_set buffer (i+3) '\x00';
      String.unsafe_set buffer (i+4) '\x00';
      String.unsafe_set buffer (i+5) '\x00';
      String.unsafe_set buffer (i+6) '\x00';
      i + 7
  | _ -> i

let rpad2 i = i + (i land 1)
let rpad4 i = i + ((4 - i) land 3)
let rpad8 i = i + ((8 - i) land 7)

(* The following function align the pointer to the boundary of the
   element to read/write and check that there is enough space in the
   buffer. This way there is only one check even if 2, 4 or 8 bytes
   are written. *)

let wprepare2 buffer i =
  let i = wpad2 buffer i in
    if i + 2 > String.length buffer then out_of_bounds ();
    i

let wprepare4 buffer i =
  let i = wpad4 buffer i in
    if i + 4 > String.length buffer then out_of_bounds ();
    i

let wprepare8 buffer i =
  let i = wpad8 buffer i in
    if i + 8 > String.length buffer then out_of_bounds ();
    i

let rprepare2 buffer i =
  let i = rpad2 i in
    if i + 2 > String.length buffer then out_of_bounds ();
    i

let rprepare4 buffer i =
  let i = rpad4 i in
    if i + 4 > String.length buffer then out_of_bounds ();
    i

let rprepare8 buffer i =
  let i = rpad8 i in
    if i + 8 > String.length buffer then out_of_bounds ();
    i

module type ByteOrder =
sig
  val byte_order : byte_order
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

module LittleEndian =
struct
  let byte_order = Little_endian
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

module BigEndian =
struct
  let byte_order = Big_endian
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

let write_char_byte buffer i v =
  if i >= String.length buffer then out_of_bounds ();
  String.unsafe_set buffer i v;
  i + 1

let write_int_byte buffer i v =
  if i >= String.length buffer then out_of_bounds ();
  String.unsafe_set buffer i (Char.unsafe_chr v);
  i + 1

let write_string buffer i str =
  let len = String.length str in
    if i + len > String.length buffer then out_of_bounds ();
    String.unsafe_set buffer (i + len) '\x00';
    match len with
      | 0 ->
          i + 1
      | 1 -> (* same thing here *)
          String.unsafe_set buffer i (String.unsafe_get str 0);
          i + 2
      | _ ->
          String.unsafe_blit str 0 buffer i len;
          i + len + 1

let write_string_signature buffer i v =
  write_string buffer (write_int_byte buffer i (String.length v)) v

module MakeWriter(BO : ByteOrder) =
struct
  let byte_order = BO.byte_order
  open BO

  let write_int_int16 buffer i v =
    let i = wprepare2 buffer i in
      String.unsafe_set buffer (i + data16bit0) (Char.unsafe_chr (v land 0xff));
      String.unsafe_set buffer (i + data16bit1) (Char.unsafe_chr ((v lsr 8) land 0xff));
      i + 2
  let write_int_uint16 = write_int_int16

  let write_int_int32 buffer i v =
    let i = wprepare4 buffer i in
      String.unsafe_set buffer (i + data32bit0) (Char.unsafe_chr (v land 0xff));
      String.unsafe_set buffer (i + data32bit1) (Char.unsafe_chr ((v lsr 8) land 0xff));
      String.unsafe_set buffer (i + data32bit2) (Char.unsafe_chr ((v lsr 16) land 0xff));
      String.unsafe_set buffer (i + data32bit3) (Char.unsafe_chr ((v lsr 24) land 0xff));
      i + 4
  let write_int_uint32 = write_int_int32

  let write_int_int32 buffer i v =
    let i = wprepare4 buffer i in
      String.unsafe_set buffer (i + data32bit0) (Char.unsafe_chr (v land 0xff));
      String.unsafe_set buffer (i + data32bit1) (Char.unsafe_chr ((v lsr 8) land 0xff));
      String.unsafe_set buffer (i + data32bit2) (Char.unsafe_chr ((v lsr 16) land 0xff));
      String.unsafe_set buffer (i + data32bit3) (Char.unsafe_chr ((v lsr 24) land 0xff));
      i + 4
  let write_int_uint32 = write_int_int32

  let write_int_int64 buffer i v =
    let i = wprepare8 buffer i in
      String.unsafe_set buffer (i + data64bit0) (Char.unsafe_chr (v land 0xff));
      String.unsafe_set buffer (i + data64bit1) (Char.unsafe_chr ((v lsr 8) land 0xff));
      String.unsafe_set buffer (i + data64bit2) (Char.unsafe_chr ((v lsr 16) land 0xff));
      String.unsafe_set buffer (i + data64bit3) (Char.unsafe_chr ((v lsr 24) land 0xff));
      String.unsafe_set buffer (i + data64bit4) (Char.unsafe_chr ((v lsr 32) land 0xff));
      String.unsafe_set buffer (i + data64bit5) (Char.unsafe_chr ((v lsr 40) land 0xff));
      String.unsafe_set buffer (i + data64bit6) (Char.unsafe_chr ((v lsr 48) land 0xff));
      String.unsafe_set buffer (i + data64bit7) (Char.unsafe_chr ((v lsr 56) land 0xff));
      i + 8
  let write_int_uint64 = write_int_int64

  let write_int32_int32 buffer i v =
    let i = wprepare4 buffer i in
      String.unsafe_set buffer (i + data32bit0) (Char.unsafe_chr (Int32.to_int v land 0xff));
      String.unsafe_set buffer (i + data32bit1) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8) land 0xff));
      String.unsafe_set buffer (i + data32bit2) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16) land 0xff));
      String.unsafe_set buffer (i + data32bit3) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24) land 0xff));
      i + 4
  let write_int32_uint32 = write_int32_int32

  let write_int64_int64 buffer i v =
    let i = wprepare8 buffer i in
      String.unsafe_set buffer (i + data64bit0) (Char.unsafe_chr (Int64.to_int v land 0xff));
      String.unsafe_set buffer (i + data64bit1) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8) land 0xff));
      String.unsafe_set buffer (i + data64bit2) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16) land 0xff));
      String.unsafe_set buffer (i + data64bit3) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24) land 0xff));
      String.unsafe_set buffer (i + data64bit4) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32) land 0xff));
      String.unsafe_set buffer (i + data64bit5) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40) land 0xff));
      String.unsafe_set buffer (i + data64bit6) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48) land 0xff));
      String.unsafe_set buffer (i + data64bit7) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56) land 0xff));
      i + 8
  let write_int64_uint64 = write_int64_int64

  let write_float_double buffer i v = write_int64_int64 buffer i (Int64.of_float v)

  let write_bool_boolean buffer i v = match v with
    | false -> write_int_uint32 buffer i 0
    | true -> write_int_uint32 buffer i 1

  let write_string_string buffer i v =
    write_string buffer (write_int_uint32 buffer i (String.length v)) v
  let write_string_object_path = write_string_string

  let write_array writer buffer i v =
    let i = wpad4 buffer i in
    let j = writer buffer (i + 4) v in
    let len = j - i - 4 in
      if len > Constant.max_array_size then write_array_too_big len;
      ignore (write_int_uint32 buffer i len);
      j

  let write_array8 writer buffer i v =
    let i = wpad4 buffer i in
    let j = wpad8 buffer (i + 4) in
    let k = writer buffer j v in
    let len = k - j in
      if len > Constant.max_array_size then write_array_too_big len;
      ignore (write_int_uint32 buffer i len);
      k
end

let read_char_byte buffer i =
  if i >= String.length buffer then out_of_bounds ();
  (i + 1, String.unsafe_get buffer i)

let read_int_byte buffer i =
  if i >= String.length buffer then out_of_bounds ();
  (i + 1, Char.code (String.unsafe_get buffer i))

let read_string buffer i len =
  if len < 0 || i + len > String.length buffer then out_of_bounds ();
  if String.unsafe_get buffer (i + len) <> '\x00' then raise (Reading_error "terminating null byte missing");
  let str = String.create len in
    match len with
      | 0 ->
          (i + 1, str)
      | 1 -> (* This happen often for variant *)
          String.unsafe_set str 0 (String.unsafe_get buffer i);
          (i + 2, str)
      | _ ->
          String.unsafe_blit buffer i str 0 len;
          (i + len + 1, str)

let read_string_signature buffer i =
  let (i, len) = read_int_byte buffer i in
    read_string buffer i len

module MakeReader(BO : ByteOrder) =
struct
  let byte_order = BO.byte_order
  open BO

  let read_int_int16 buffer i =
    let i = rprepare2 buffer i in
    let v0 = Char.code (String.unsafe_get buffer (i + data16bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data16bit0)) in
    let v = v0 land (v1 lsl 8) in
      (i + 2,
       if v land (1 lsl 15) = 0
       then v
       else (-1 land (lnot 0x7fff)) lor v)

  let read_int_uint16 buffer i =
    let i = rprepare2 buffer i in
    let v0 = Char.code (String.unsafe_get buffer (i + data16bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data16bit1)) in
      (i + 2,
       v0 lor (v1 lsl 8))

  let read_int_int32 buffer i =
    let i = rprepare4 buffer i in
    let v0 = Char.code (String.unsafe_get buffer (i + data32bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data32bit1))
    and v2 = Char.code (String.unsafe_get buffer (i + data32bit2))
    and v3 = Char.code (String.unsafe_get buffer (i + data32bit3)) in
    let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
      (i + 4,
       if v land (1 lsl 31) = 0
       then v
       else (-1 land (lnot 0x7fffffff)) lor v)

  let read_int_uint32 buffer i =
    let i = rprepare4 buffer i in
    let v0 = Char.code (String.unsafe_get buffer (i + data32bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data32bit1))
    and v2 = Char.code (String.unsafe_get buffer (i + data32bit2))
    and v3 = Char.code (String.unsafe_get buffer (i + data32bit3)) in
      (i + 4,
       v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24))

  let read_int_int64 buffer i =
    let i = rprepare8 buffer i in
    let v0 = Char.code (String.unsafe_get buffer (i + data64bit0))
    and v1 = Char.code (String.unsafe_get buffer (i + data64bit1))
    and v2 = Char.code (String.unsafe_get buffer (i + data64bit2))
    and v3 = Char.code (String.unsafe_get buffer (i + data64bit3))
    and v4 = Char.code (String.unsafe_get buffer (i + data64bit4))
    and v5 = Char.code (String.unsafe_get buffer (i + data64bit5))
    and v6 = Char.code (String.unsafe_get buffer (i + data64bit6))
    and v7 = Char.code (String.unsafe_get buffer (i + data64bit7)) in
      (* This is not correct on a 128 bits... *)
      (i + 8,
       v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24)
       lor (v4 lsl 32) lor (v5 lsl 40) lor (v6 lsl 48) lor (v7 lsl 56))
  let read_int_uint64 = read_int_int64

  let read_int32_int32 buffer i =
    let i = rprepare4 buffer i in
    let v0 = Int32.of_int (Char.code (String.unsafe_get buffer (i + data32bit0)))
    and v1 = Int32.of_int (Char.code (String.unsafe_get buffer (i + data32bit1)))
    and v2 = Int32.of_int (Char.code (String.unsafe_get buffer (i + data32bit2)))
    and v3 = Int32.of_int (Char.code (String.unsafe_get buffer (i + data32bit3))) in
      (i + 4,
       Int32.logor
         (Int32.logor
            v0
            (Int32.shift_left v1 8))
         (Int32.logor
            (Int32.shift_left v2 16)
            (Int32.shift_left v3 24)))
  let read_int32_uint32 = read_int32_int32

  let read_int64_int64 buffer i =
    let i = rprepare8 buffer i in
    let v0 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit0)))
    and v1 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit1)))
    and v2 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit2)))
    and v3 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit3)))
    and v4 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit4)))
    and v5 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit5)))
    and v6 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit6)))
    and v7 = Int64.of_int (Char.code (String.unsafe_get buffer (i + data64bit7))) in
      (i + 8,
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
  let read_int64_uint64 = read_int64_int64

  let read_float_double buffer i =
    let i, v = read_int64_uint64 buffer i in
      (i, Int64.to_float v)

  let read_bool_boolean buffer i =
    let (i, v) = read_int_uint32 buffer i in
      match v with
        | 0 -> (i, false)
        | 1 -> (i, true)
        | n -> raise (Failure ("invalid boolean value: " ^ string_of_int n))

  let read_string_string buffer i =
    let (i, len) = read_int_uint32 buffer i in
      read_string buffer i len
  let read_string_object_path = read_string_string

  let read_array reader buffer i =
    let (i, len) = read_int_uint32 buffer i in
      if len > Constant.max_array_size then read_array_too_big len;
      let limit = i + len in
        (limit, reader limit buffer i)

  let read_array8 reader buffer i =
    let (i, len) = read_int_uint32 buffer i in
      if len > Constant.max_array_size then read_array_too_big len;
      let i = rpad8 i in
      let limit = i + len in
        (limit, reader limit buffer i)
end

module LEWriter = MakeWriter(LittleEndian)
module BEWriter = MakeWriter(BigEndian)
module LEReader = MakeReader(LittleEndian)
module BEReader = MakeReader(BigEndian)

let check_signature buffer i str =
  let i, len = read_int_byte buffer i in
  let fail _ =
    raise (Content_error
             (sprintf "unexpected signature, expected: %s, got: %s"
                str (String.sub buffer i len)))
  in
  let rec aux = function
    | -1 -> ()
    | j when String.unsafe_get buffer (i + j) = String.unsafe_get str j ->
        aux (j - 1)
    | _ ->
        fail ()
  in
    if len < 0 || i + len > String.length buffer then out_of_bounds ();
    if String.unsafe_get buffer (i + len) <> '\x00'
    then raise (Reading_error "signature does not end with a null byte");
    let len' = String.length str in
      if len <> len'
      then fail ();
      aux (len' - 1)

let read_until reader empty i limit =
  let rec aux i acc =
    if i < limit
    then reader i acc aux
    else if i > limit
    then raise (Reading_error "invalid array size")
    else acc
  in
    aux i empty

let read_until_rev reader empty i limit =
  let rec aux i =
    if i < limit
    then reader i aux
    else if i > limit
    then raise (Reading_error "invalid array size")
    else empty
  in
    aux i
