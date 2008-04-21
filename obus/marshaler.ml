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
module type Basic = sig
  type 'a t
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
end
module type BasicWriter = Basic with type 'a t = ptr -> 'a -> unit
module type BasicReader = Basic with type 'a t = ptr -> 'a
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
end

module Reader(BO : ByteOrder)(B : Buffer) =
struct
  open BO
  type 'a t = ptr -> 'a
  let buffer = B.buffer

  let pad2 i = i + (i mod 2)
  let pad4 i = i + (3 - (i-1) mod 4)
  let pad8 i = i + (7 - (i-1) mod 8)

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
end

module T = Types_internal
module V = Values_internal

module SigW = Common.SignatureWriter(T)(struct exception Fail = Write_error end)
module SigR = Common.SignatureReader(T)(struct exception Fail = Read_error end)

module SingleWriter(B : BasicWriter) =
struct
  let write_single_t i t =
    let j = SigW.write_single B.buffer (i + 1) t in
      B.buffer.[i] <- char_of_int (j - i);
      B.buffer.[j] <- '\x00';
      i + 1

  let write_t i t =
    let j = SigW.write_t B.buffer i t in
      B.buffer.[i] <- char_of_int (j - i);
      B.buffer.[j] <- '\x00';
      i + 1

  let write_string i str =
    let i = B.pad4 i in
    let len = String.length str in
      B.int_uint32 (String.length str) i;
      String.blit str 0 B.buffer (i + 4) len;
      let i = i + 4 + len in
        B.buffer.[i] <- '\x00';
        i + 1

  let write_basic_v i = function
    | V.Byte(v) -> B.buffer.[i] <- v; i + 1
    | V.Boolean(v) -> let i = B.pad4 i in B.bool_boolean i v; i + 4
    | V.Int16(v) -> let i = B.pad2 i in B.int_int16 i v; i + 2
    | V.Int32(v) -> let i = B.pad4 i in B.int32_int32 i v; i + 4
    | V.Int64(v) -> let i = B.pad8 i in B.int64_int64 i v; i + 8
    | V.Uint16(v) -> let i = B.pad2 i in B.int_uint16 i v; i + 2
    | V.Uint32(v) -> let i = B.pad4 i in B.int32_uint32 i v; i + 4
    | V.Uint64(v) -> let i = B.pad8 i in B.int64_uint64 i v; i + 8
    | V.Double(v) -> let i = B.pad8 i in B.float_double i v; i + 8
    | V.String(v) -> write_string i v
    | V.Signature(v) -> write_t i v
    | V.Object_path(v) -> write_string i v

  let rec write_array f i =
    let i = B.pad4 i in
    let j = i + 4 in
    let k = f j in
    let len = k - j in
      if len > Common.max_array_size
      then raise (Write_error "array too big!")
      else begin
        B.int_int32 len i;
        k
      end

  let rec write_single_v i = function
    | V.Basic(v) -> write_basic_v i v
    | V.Array(_, v) -> write_array (fun i -> List.fold_left write_single_v i v) i
    | V.Dict(_, _, v) -> write_array (fun i -> List.fold_left (fun i (k, v) -> write_single_v (write_basic_v (B.pad8 i) k) v) i v) i
    | V.Structure(v) -> List.fold_left write_single_v (B.pad8 i) v
    | V.Variant(v) -> let i = write_single_t i (V.type_of_single v) in write_single_v i v

  let write_v = List.fold_left write_single_v
end

module SingleReader(B : BasicReader) =
struct
  let read_single_t = SigR.read_single B.buffer
  let read_t = SigR.read_t B.buffer

  let read_basic_v i = function
    | T.Byte -> (i + 1, V.Byte(B.buffer.[i]))
    | T.Boolean -> let i = B.pad4 i in (i + 4, V.Boolean(B.bool_boolean i))
    | T.Int16 -> let i = B.pad2 i in (i + 2, V.Int16(B.int_int16 i))
    | T.Int32 -> let i = B.pad4 i in (i + 4, V.Int32(B.int32_int32 i))
    | T.Int64 -> let i = B.pad8 i in (i + 8, V.Int64(B.int64_int64 i))
    | T.Uint16 -> let i = B.pad2 i in (i + 2, V.Uint16(B.int_uint16 i))
    | T.Uint32 -> let i = B.pad4 i in (i + 4, V.Uint32(B.int32_uint32 i))
    | T.Uint64 -> let i = B.pad8 i in (i + 8, V.Uint64(B.int64_uint64 i))
    | T.Double -> let i = B.pad8 i in (i + 8, V.Double(B.float_double i))
    | T.String -> let i = B.pad4 i in let len = B.int_uint32 i in (i + 4 + len, V.String(String.sub B.buffer (i + 4) len))
    | T.Signature -> let len = int_of_char B.buffer.[i] in (i + 1 + len, V.Signature(snd (SigR.read_t B.buffer (i + 1))))
    | T.Object_path -> let i = B.pad4 i in let len = B.int_uint32 i in (i + 4 + len, V.Object_path(String.sub B.buffer (i + 4) len))


  let rec read_until f x limit i =
    if i = limit
    then (i, x)
    else if i < limit
    then let j, y = f x i in
      read_until f y limit j
    else raise (Read_error "wrong array size!")

  let rec read_single_v i = function
    | T.Basic(t) ->
        let i, x = read_basic_v i t in
          (i, V.Basic x)
    | T.Array(t) ->
        let i = B.pad4 i in
        let len = B.int_uint32 i in
          if len > Common.max_array_size
          then raise (Read_error "array too big!")
          else
            let i, v = read_until begin fun acc i ->
              let i, v = read_single_v i t in
                (i, v :: acc)
            end [] (i + len) i in
              (i, V.Array(t, List.rev v))
    | T.Dict(tk, tv) ->
        let i = B.pad4 i in
        let len = B.int_uint32 i in
          if len > Common.max_array_size
          then raise (Read_error "array too big!")
          else
            let i = B.pad8 i in
            let i, v = read_until begin fun acc i ->
              let i, k = read_basic_v (B.pad8 i) tk in
              let i, v = read_single_v i tv in
                (i, (k, v) :: acc)
            end [] (i + len) i in
              (i, V.Dict(tk, tv, List.rev v))
    | T.Structure(t) ->
        let i , v = List.fold_left begin fun (i, acc) t ->
          let i, v = read_single_v i t in
            (i, v :: acc)
        end (B.pad8 i, []) t in
          (i, V.Structure(List.rev v))
    | T.Variant ->
        let len = int_of_char B.buffer.[i] in
        let _, t = SigR.read_single B.buffer (i + 1) in
        let i, v = read_single_v (i + 2 + len) t in
          (i, V.Variant(v))

  let read_v i t =
    let i, v = List.fold_left begin fun (i,acc) t ->
      let i, v = read_single_v i t in
        (i, v :: acc)
    end (i, []) t in
      (i, List.rev v)
end

module LEWriter = Writer(LE)
module BEWriter = Writer(BE)
module LEReader = Reader(LE)
module BEReader = Reader(BE)
