(*
 * wire.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(***** Low-level, unsafe serialization/deserialization *****)

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

(***** Safe functions *****)

open Printf
open OBus_value
open OBus_info
open OBus_internals

exception Out_of_bounds
exception Reading_error of string
exception Writing_error of string

let wcheck_array_len len =
  if len < 0 || len > OBus_info.max_array_size
  then raise (Writing_error (Printf.sprintf "array too big to be send: %d" len))

let rcheck_array_len len =
  if len < 0 || len > OBus_info.max_array_size
  then raise (Reading_error (Printf.sprintf "array size exceed the limit: %d" len))

(*** Padding functions ***)

let pad2 i = i land 1
let pad4 i = (4 - i) land 3
let pad8 i = (8 - i) land 7

let wpadn f ctx i =
  let count = f i in
  if i + count > String.length ctx.buffer then raise Out_of_bounds;
  for j = 0 to count - 1 do
    String.unsafe_set ctx.buffer (i + j) '\x00'
  done;
  i + count

let wpad2 = wpadn pad2
let wpad4 = wpadn pad4
let wpad8 = wpadn pad8

let rpadn f ctx i =
  let count = f i in
  if i + count > String.length ctx.buffer then raise Out_of_bounds;
  for j = 0 to count - 1 do
    if String.unsafe_get ctx.buffer (i + j) <> '\x00'
    then raise (Reading_error "unitialized padding bytes")
  done;
  i + count

let rpad2 = rpadn pad2
let rpad4 = rpadn pad4
let rpad8 = rpadn pad8

let pad8_p = function
  | Tdict_entry _
  | Tsingle (Tstruct _)
  | Tsingle (Tbasic Tint64)
  | Tsingle (Tbasic Tuint64)
  | Tsingle (Tbasic Tdouble) -> true
  | _ -> false

(*** Writing ***)

let write1 unsafe_writer v ctx i =
  if i + 1 > String.length ctx.buffer then raise Out_of_bounds;
  unsafe_writer v ctx.buffer i;
  i + 1

let writen padding size unsafe_le_writer unsafe_be_writer v ctx i =
  let count = padding i in
  let end_ptr = i + count + size in
  if end_ptr > String.length ctx.buffer then raise Out_of_bounds;
  for j = 0 to count - 1 do
    String.unsafe_set ctx.buffer (i + j) '\x00'
  done;
  (match ctx.byte_order with
     | Little_endian -> unsafe_le_writer v ctx.buffer (i + count)
     | Big_endian -> unsafe_be_writer v ctx.buffer (i + count));
  end_ptr

let write2 le be = writen pad2 2 le be
let write4 le be = writen pad4 4 le be
let write8 le be = writen pad8 8 le be

let wbyte = write1 unsafe_write_char_as_byte
let wint8 v = wbyte & Char.unsafe_chr & v land 0xff
let wint16 = write2 LEW.unsafe_write_int_as_int16 BEW.unsafe_write_int_as_int16
let wint32 = write4 LEW.unsafe_write_int32_as_int32 BEW.unsafe_write_int32_as_int32
let wint64 = write8 LEW.unsafe_write_int64_as_int64 BEW.unsafe_write_int64_as_int64
let wint = write4 LEW.unsafe_write_int_as_int32 BEW.unsafe_write_int_as_int32
let wuint8 = wint8
let wuint16 = write2 LEW.unsafe_write_int_as_uint16 BEW.unsafe_write_int_as_uint16
let wuint32 = write4 LEW.unsafe_write_int32_as_uint32 BEW.unsafe_write_int32_as_uint32
let wuint64 = write8 LEW.unsafe_write_int64_as_uint64 BEW.unsafe_write_int64_as_uint64
let wuint = write4 LEW.unsafe_write_int_as_uint32 BEW.unsafe_write_int_as_uint32
let wdouble v = wuint64 & Int64.of_float v
let wboolean = function
  | false -> wuint 0
  | true -> wuint 1
let wstring str ctx i =
  let len = String.length str in
  let i = wuint len ctx i in
  if i + len + 1 > String.length ctx.buffer then raise Out_of_bounds;
  String.unsafe_blit str 0 ctx.buffer i len;
  String.unsafe_set ctx.buffer (i + len) '\000';
  i + len + 1
let wobject_path = wstring

module Types_writer = Types_rw.Make_writer(OBus_value)

let wsignature tl ctx i =
  let len = Types_writer.signature_size tl in
  let i = wuint8 len ctx i in
  if i + len + 1 > String.length ctx.buffer then raise Out_of_bounds;
  let i = Types_writer.write_sequence ctx.buffer i tl in
  String.unsafe_set ctx.buffer i '\000';
  i + 1

let wtype t ctx i =
  let len = Types_writer.single_signature_size t in
  let i = wuint8 len ctx i in
  if i + len + 1 > String.length ctx.buffer then raise Out_of_bounds;
  let i = Types_writer.write_single ctx.buffer i t in
  String.unsafe_set ctx.buffer i '\000';
  i + 1

let wstruct writer v ctx i = writer v ctx (wpad8 ctx i)

let warray typ writer fold v ctx i =
  let i = wpad4 ctx i in
  let j = if pad8_p typ then wpad8 ctx (i + 4) else i + 4 in
  let k = fold (fun x -> writer x ctx) v j in
  let len = k - j in
  wcheck_array_len len;
  (match ctx.byte_order with
     | Little_endian -> LEW.unsafe_write_int_as_uint32 len ctx.buffer i
     | Big_endian -> BEW.unsafe_write_int_as_uint32 len ctx.buffer i);
  k

let wdict_entry kwriter vwriter (k, v) ctx i = vwriter v ctx (kwriter k ctx (wpad8 ctx i))

let rec fold_list f l acc = match l with
  | [] -> acc
  | x :: l -> fold_list f l (f x acc)

let wlist typ writer = warray typ writer fold_list

let wbyte_array str ctx i =
  let len = String.length str in
  let i = wuint len ctx i in
  wcheck_array_len len;
  String.unsafe_blit str 0 ctx.buffer i len;
  i + len

let wfixed typ writer v ctx i =
  let i = wtype typ ctx i in
  writer v ctx i

let wbasic = function
  | Byte x -> wbyte x
  | Boolean x -> wboolean x
  | Int16 x -> wint16 x
  | Int32 x -> wint32 x
  | Int64 x -> wint64 x
  | Uint16 x -> wuint16 x
  | Uint32 x -> wuint32 x
  | Uint64 x -> wuint64 x
  | Double x -> wdouble x
  | String x -> wstring x
  | Signature x -> wsignature x
  | Object_path x -> wobject_path x

let rec wsingle = function
  | Basic x -> wbasic x
  | Array(t, l) -> wlist t welement l
  | Struct l -> wstruct wsequence l
  | Variant v -> wvariant v

and welement = function
  | Dict_entry(k, v) ->
      wdict_entry wbasic wsingle (k, v)
  | Single x ->
      wsingle x

and wvariant v ctx i =
  let i = wtype (type_of_single v) ctx i in
  wsingle v ctx i

and wsequence v ctx i = match v with
  | [] -> i
  | x :: l -> wsequence l ctx (wsingle x ctx i)

(*** Reading ***)

let read1 unsafe_reader ctx i =
  if i + 1 > String.length ctx.buffer then raise Out_of_bounds;
  let v = unsafe_reader ctx.buffer i in
  (i + 1, v)

let readn padding size unsafe_le_reader unsafe_be_reader ctx i =
  let count = padding i in
  let end_ptr = i + count + size in
  if end_ptr > String.length ctx.buffer then raise Out_of_bounds;
  for j = 0 to count - 1 do
    if String.unsafe_get ctx.buffer (i + j) <> '\x00'
    then raise (Reading_error "unitialized padding bytes")
  done;
  match ctx.byte_order with
    | Little_endian -> end_ptr, unsafe_le_reader ctx.buffer (i + count)
    | Big_endian -> end_ptr, unsafe_be_reader ctx.buffer (i + count)

let read2 le be = readn pad2 2 le be
let read4 le be = readn pad4 4 le be
let read8 le be = readn pad8 8 le be

let rwrap reader f ctx i = let i, v = reader ctx i in (i, f v)

let rbyte = read1 unsafe_read_byte_as_char
let rint8 = rwrap rbyte (fun v ->
                           let v = int_of_char v in
                           if v >= 128 then v - 256 else v)
let rint16 = read2 LER.unsafe_read_int16_as_int BER.unsafe_read_int16_as_int
let rint32 = read4 LER.unsafe_read_int32_as_int32 BER.unsafe_read_int32_as_int32
let rint64 = read8 LER.unsafe_read_int64_as_int64 BER.unsafe_read_int64_as_int64
let rint = read4 LER.unsafe_read_int32_as_int BER.unsafe_read_int32_as_int
let ruint8 = rwrap rbyte int_of_char
let ruint16 = read2 LER.unsafe_read_uint16_as_int BER.unsafe_read_uint16_as_int
let ruint32 = read4 LER.unsafe_read_uint32_as_int32 BER.unsafe_read_uint32_as_int32
let ruint64 = read8 LER.unsafe_read_uint64_as_int64 BER.unsafe_read_uint64_as_int64
let ruint = read4 LER.unsafe_read_uint32_as_int BER.unsafe_read_uint32_as_int
let rdouble = rwrap ruint64 Int64.to_float
let rboolean = rwrap ruint
  (function
     | 0 -> false
     | 1 -> true
     | n -> raise & Reading_error ("invalid boolean value: " ^ string_of_int n))
let rstring ctx i =
  let i, len = ruint ctx i in
  let end_ptr = i + len + 1 in
  if len < 0 || end_ptr > String.length ctx.buffer then raise Out_of_bounds;
  let str = String.create len in
  String.unsafe_blit ctx.buffer i str 0 len;
  match String.unsafe_get ctx.buffer (i + len) with
    | '\x00' -> (end_ptr, str)
    | _ -> raise & Reading_error "terminating null byte missing"
let robject_path = rstring

module Types_reader = Types_rw.Make_reader(OBus_value)
  (struct
     let get = String.unsafe_get
     let terminated str i = String.unsafe_get str i = '\x00'
   end)

let rsignature ctx i =
  let i, len = ruint8 ctx i in
  if len < 0 || i + len + 1 > String.length ctx.buffer then raise Out_of_bounds;
  if String.unsafe_get ctx.buffer (i + len) <> '\x00'
  then raise & Reading_error "signature does not end with a null byte";
  try
    Types_reader.read_sequence ctx.buffer i
  with
      Types_rw.Parse_failure(j, msg) ->
        raise & Reading_error
          (sprintf "invalid signature %S, at position %d: %s"
             (String.sub ctx.buffer i len) (j - i) msg)

let rtype ctx i = rwrap rsignature
  (function
     | [t] -> t
     | [] -> raise & Reading_error "empty variant signature"
     | _ -> raise & Reading_error "variant signature contain more than one single type")
  ctx i

let rstruct reader ctx i = reader ctx (rpad8 ctx i)

let read_until reader empty limit ctx i =
  let rec aux (i, acc) =
    if i < limit
    then aux (reader acc ctx i)
    else
      if i > limit
      then raise & Reading_error "invalid array size"
      else (i, acc)
  in
  aux (i, empty)

let rarray typ reader acc ctx i =
  let i, len = ruint ctx i in
  rcheck_array_len len;
  let i = if pad8_p typ then rpad8 ctx i else i in
  read_until reader acc (i + len) ctx i

let rdict_entry kreader vreader ctx i =
  let i = rpad8 ctx i in
  let i, k = kreader ctx i in
  let i, v = vreader ctx i in
  (i, (k, v))

let rlist typ reader ctx i =
  let i, len = ruint ctx i in
  rcheck_array_len len;
  let i = if pad8_p typ then rpad8 ctx i else i in
  let limit = i + len in
  let rec aux i =
    if i < limit
    then
      let i, v = reader ctx i in
      v :: aux i
    else
      if i > limit
      then raise (Reading_error "invalid array size")
      else []
  in
  (limit, aux i)

let rset typ reader = rarray typ (fun acc ctx i ->
                                    let i, v = reader ctx i in
                                    (i, v :: acc)) []

let rbyte_array ctx i =
  let i, len = ruint ctx i in
  rcheck_array_len len;
  let str = String.create len in
  String.unsafe_blit ctx.buffer i str 0 len;
  (i + len, str)

let rfixed typ reader ctx i =
  let i, typ' = rtype ctx i in
  if typ = typ'
  then reader ctx i
  else failwith
    (sprintf "invalid variant signature, expected '%s', got '%s'"
       (string_of_signature [typ])
       (string_of_signature [typ]))

let rbasic = function
  | Tbyte -> rwrap rbyte vbyte
  | Tboolean -> rwrap rboolean vboolean
  | Tint16 -> rwrap rint16 vint16
  | Tint32 -> rwrap rint32 vint32
  | Tint64 -> rwrap rint64 vint64
  | Tuint16 -> rwrap ruint16 vuint16
  | Tuint32 -> rwrap ruint32 vuint32
  | Tuint64 -> rwrap ruint64 vuint64
  | Tdouble -> rwrap rdouble vdouble
  | Tstring -> rwrap rstring vstring
  | Tsignature -> rwrap rsignature vsignature
  | Tobject_path -> rwrap robject_path vobject_path

let rec rsingle = function
  | Tbasic t -> rwrap (rbasic t) vbasic
  | Tarray t -> rwrap (match t with
                         | Tdict_entry(tk, tv) ->
                             let kreader = rbasic tk
                             and vreader = rsingle tv in
                             rset t (fun ctx i ->
                                       let i = rpad8 ctx i in
                                       let i, k = kreader ctx i in
                                       let i, v = vreader ctx i in
                                       (i, Dict_entry(k, v)))
                         | Tsingle t' ->
                             rlist t (rwrap (rsingle t') vsingle)) (varray t)
  | Tstruct tl -> rwrap (rstruct (rsequence tl)) vstruct
  | Tvariant -> rvariant

and rvariant ctx i =
  let i, t = rtype ctx i in
  let i, v = rsingle t ctx i in
  (i, vvariant v)

and rsequence t ctx i = match t with
  | [] -> (i, [])
  | t :: tl ->
      let i, x = rsingle t ctx i in
      let i, l = rsequence tl ctx i in
      (i, x :: l)
