(*
 * wire.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open OBus_value
open OBus_info

let (&) a b = a b

exception Out_of_bounds
exception Reading_error of string
exception Writing_error of string

module type Byte_order =
sig
  val byte_order_char : char
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

module Little_endian =
struct
  let byte_order_char = 'l'
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

module Big_endian =
struct
  let byte_order_char = 'B'
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

let validate_string fail s =
  let null_byte i = raise & fail & sprintf "invalid string %S, at position %d: null byte" s i
  and malformed_code i = raise & fail & sprintf "invalid string %S, at position %d: malformed utf8 code" s i in
  let rec trail c i a =
    if c = 0 then a else
      if i >= String.length s
      then malformed_code i
      else
        let n = Char.code (String.unsafe_get s i) in
        if n = 0
        then null_byte i
        else
          if n < 0x80 || n >= 0xc0
          then malformed_code i
          else trail (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
  let rec main i =
    if i >= String.length s then () else
      let n = Char.code (String.unsafe_get s i) in
      if n = 0 then null_byte i else
        if n < 0x80 then main (i + 1) else
          if n < 0xc2 then malformed_code i else
            if n <= 0xdf then
              if trail 1 (i + 1) (n - 0xc0) < 0x80 then malformed_code i else
                main (i + 2)
            else if n <= 0xef then
              if trail 2 (i + 1) (n - 0xe0) < 0x800 then malformed_code i else
                main (i + 3)
            else if n <= 0xf7 then
              if trail 3 (i + 1) (n - 0xf0) < 0x10000 then malformed_code i else
                main (i + 4)
            else if n <= 0xfb then
              if trail 4 (i + 1) (n - 0xf8) < 0x200000 then malformed_code i else
                main (i + 5)
            else if n <= 0xfd then
              let n = trail 5 (i + 1) (n - 0xfc) in
              if n lsr 16 < 0x400 then malformed_code i else
                main (i + 6)
            else malformed_code i in
  main 0


let pad2 i = i land 1
let pad4 i = (4 - i) land 3
let pad8 i = (8 - i) land 7

let single_pad8_p = function
  | Tstruct _
  | Tbasic Tint64
  | Tbasic Tuint64
  | Tbasic Tdouble -> true
  | _ -> false

let pad8_p = function
  | Tdict_entry _ -> true
  | Tsingle t -> single_pad8_p t

module Make_unsafe_writer(BO : Byte_order) =
struct
  open BO

  let unsafe_write_char_as_byte buffer i v =
    String.unsafe_set buffer i v

  let unsafe_write_int_as_byte buffer i v =
    String.unsafe_set buffer i (Char.unsafe_chr v)

  let unsafe_write_int_as_int16 buffer i v =
    String.unsafe_set buffer (i + data16bit0) (Char.unsafe_chr v);
    String.unsafe_set buffer (i + data16bit1) (Char.unsafe_chr (v lsr 8))
  let unsafe_write_int_as_uint16 = unsafe_write_int_as_int16

  let unsafe_write_int_as_int32 buffer i v =
    String.unsafe_set buffer (i + data32bit0) (Char.unsafe_chr v);
    String.unsafe_set buffer (i + data32bit1) (Char.unsafe_chr (v lsr 8));
    String.unsafe_set buffer (i + data32bit2) (Char.unsafe_chr (v lsr 16));
    String.unsafe_set buffer (i + data32bit3) (Char.unsafe_chr (v asr 24))
  let unsafe_write_int_as_uint32 = unsafe_write_int_as_int32

  let unsafe_write_int_as_int64 buffer i v =
    String.unsafe_set buffer (i + data64bit0) (Char.unsafe_chr v);
    String.unsafe_set buffer (i + data64bit1) (Char.unsafe_chr (v lsr 8));
    String.unsafe_set buffer (i + data64bit2) (Char.unsafe_chr (v lsr 16));
    String.unsafe_set buffer (i + data64bit3) (Char.unsafe_chr (v asr 24));
    String.unsafe_set buffer (i + data64bit4) (Char.unsafe_chr (v asr 32));
    String.unsafe_set buffer (i + data64bit5) (Char.unsafe_chr (v asr 40));
    String.unsafe_set buffer (i + data64bit6) (Char.unsafe_chr (v asr 48));
    String.unsafe_set buffer (i + data64bit7) (Char.unsafe_chr (v asr 56))
  let unsafe_write_int_as_uint64 = unsafe_write_int_as_int64

  let unsafe_write_int32_as_int32 buffer i v =
    String.unsafe_set buffer (i + data32bit0) (Char.unsafe_chr (Int32.to_int v));
    String.unsafe_set buffer (i + data32bit1) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
    String.unsafe_set buffer (i + data32bit2) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
    String.unsafe_set buffer (i + data32bit3) (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24)))
  let unsafe_write_int32_as_uint32 = unsafe_write_int32_as_int32

  let unsafe_write_int64_as_int64 buffer i v =
    String.unsafe_set buffer (i + data64bit0) (Char.unsafe_chr (Int64.to_int v));
    String.unsafe_set buffer (i + data64bit1) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8)));
    String.unsafe_set buffer (i + data64bit2) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16)));
    String.unsafe_set buffer (i + data64bit3) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24)));
    String.unsafe_set buffer (i + data64bit4) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32)));
    String.unsafe_set buffer (i + data64bit5) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40)));
    String.unsafe_set buffer (i + data64bit6) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48)));
    String.unsafe_set buffer (i + data64bit7) (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56)))
  let unsafe_write_int64_as_uint64 = unsafe_write_int64_as_int64

  let wcheck_array_len len =
    if len < 0 || len > max_array_size
    then raise & Writing_error (Printf.sprintf "array too big to be send: %d" len)

  let wpadn f buffer i =
    let count = f i in
    if i + count > String.length buffer then raise Out_of_bounds;
    for j = 0 to count - 1 do
      String.unsafe_set buffer (i + j) '\x00'
    done;
    i + count

  let wpad2 = wpadn pad2
  let wpad4 = wpadn pad4
  let wpad8 = wpadn pad8

  let write1 unsafe_writer buffer i v =
    if i + 1 > String.length buffer then raise Out_of_bounds;
    unsafe_writer buffer i v;
    i + 1

  let writen padding size unsafe_writer buffer i v =
    let count = padding i in
    let end_ptr = i + count + size in
    if end_ptr > String.length buffer then raise Out_of_bounds;
    for j = 0 to count - 1 do
      String.unsafe_set buffer (i + j) '\x00'
    done;
    unsafe_writer buffer (i + count) v;
    end_ptr

  let write2 f = writen pad2 2 f
  let write4 f = writen pad4 4 f
  let write8 f = writen pad8 8 f

  let wuint8 = write1 unsafe_write_int_as_byte
  let wuint = write4 unsafe_write_int_as_uint32
  let wsig_len buffer i x =
    if x > 255 then raise & Writing_error "signature too big";
    wuint8 buffer i x
  let wlen = wuint

  let wbyte = write1 unsafe_write_char_as_byte
  let wint16 = write2 unsafe_write_int_as_int16
  let wint32 = write4 unsafe_write_int32_as_int32
  let wint64 = write8 unsafe_write_int64_as_int64
  let wuint16 = write2 unsafe_write_int_as_uint16
  let wuint32 = write4 unsafe_write_int32_as_uint32
  let wuint64 = write8 unsafe_write_int64_as_uint64
  let wdouble buffer i v = wuint64 buffer i (Int64.of_float v)
  let wboolean buffer i v = wuint buffer i (match v with false -> 0 | true -> 1)

  let wunsafe_string buffer i str =
    let len = String.length str in
    let i = wlen buffer i len in
    if i + len + 1 > String.length buffer then raise Out_of_bounds;
    String.unsafe_blit str 0 buffer i len;
    String.unsafe_set buffer (i + len) '\000';
    i + len + 1

  let wstring buffer i str =
    validate_string (fun exn -> Writing_error exn) str;
    wunsafe_string buffer i str

  let wobject_path buffer i = function
    | [] -> wstring buffer i "/"
    | path ->
        let rec aux i = function
          | [] ->
              if i + 1 > String.length buffer then raise Out_of_bounds;
              String.unsafe_set buffer i '\000';
              i
          | element :: rest ->
              OBus_path.validate_element element;
              let len = String.length element in
              if i + len + 1 > String.length buffer then raise Out_of_bounds;
              String.unsafe_set buffer i '/';
              String.unsafe_blit element 0 buffer (i + 1) len;
              aux (i + len + 1) rest
        in
        let i = wpad4 buffer i in
        let j = i + 4 in
        let k = aux j path in
        unsafe_write_int_as_uint32 buffer i (k - j);
        k + 1

  module Types_writer = Types_rw.Make_writer(OBus_value)

  let wsignature buffer i tl =
    let len = Types_writer.signature_size tl in
    let i = wsig_len buffer i len in
    if i + len + 1 > String.length buffer then raise Out_of_bounds;
    let i = Types_writer.write_sequence buffer i tl in
    String.unsafe_set buffer i '\000';
    i + 1

  let wtype buffer i t =
    let len = Types_writer.single_signature_size t in
    let i = wsig_len buffer i len in
    if i + len + 1 > String.length buffer then raise Out_of_bounds;
    let i = Types_writer.write_single buffer i t in
    String.unsafe_set buffer i '\000';
    i + 1

  let wbasic buffer i = function
    | Byte x -> wbyte buffer i x
    | Boolean x -> wboolean buffer i x
    | Int16 x -> wint16 buffer i x
    | Int32 x -> wint32 buffer i x
    | Int64 x -> wint64 buffer i x
    | Uint16 x -> wuint16 buffer i x
    | Uint32 x -> wuint32 buffer i x
    | Uint64 x -> wuint64 buffer i x
    | Double x -> wdouble buffer i x
    | String x -> wstring buffer i x
    | Signature x -> wsignature buffer i x
    | Object_path x -> wobject_path buffer i x

  let rec wsingle buffer i = function
    | Basic x -> wbasic buffer i x
    | Array(t, l) ->
        let i = wpad4 buffer i in
        let j = if pad8_p t then wpad8 buffer (i + 4) else i + 4 in
        let k = List.fold_left (welement buffer) j l in
        let len = k - j in
        wcheck_array_len len;
        unsafe_write_int_as_uint32 buffer i len;
        k
    | Struct l -> wsequence buffer (wpad8 buffer i) l
    | Variant v ->
        wvariant buffer i v

  and wvariant buffer i v =
    let i = wtype buffer i (type_of_single v) in
    wsingle buffer i v

  and welement buffer i = function
    | Dict_entry(k, v) ->
        wsingle buffer (wbasic buffer i k) v
    | Single x ->
        wsingle buffer i x

  and wsequence buffer i = function
    | [] -> i
    | x :: l -> wsequence buffer (wsingle buffer i x) l
end

module Make_unsafe_reader(BO : Byte_order) =
struct
  open BO

  let unsafe_read_byte_as_char buffer i =
    String.unsafe_get buffer i

  let unsafe_read_byte_as_int buffer i =
    Char.code (String.unsafe_get buffer i)

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

  let rcheck_array_len len =
    if len < 0 || len > max_array_size
    then raise & Reading_error (Printf.sprintf "array size exceed the limit: %d" len)

  let rpadn f buffer i =
    let count = f i in
    if i + count > String.length buffer then raise Out_of_bounds;
    for j = 0 to count - 1 do
      if String.unsafe_get buffer (i + j) <> '\x00'
      then raise (Reading_error "unitialized padding bytes")
    done;
    i + count

  let rpad2 = rpadn pad2
  let rpad4 = rpadn pad4
  let rpad8 = rpadn pad8

  let read1 unsafe_reader buffer i =
    if i + 1 > String.length buffer then raise Out_of_bounds;
    let v = unsafe_reader buffer i in
    (i + 1, v)

  let readn padding size unsafe_reader buffer i =
    let count = padding i in
    let end_ptr = i + count + size in
    if end_ptr > String.length buffer then raise Out_of_bounds;
    for j = 0 to count - 1 do
      if String.unsafe_get buffer (i + j) <> '\x00'
      then raise & Reading_error "unitialized padding bytes"
    done;
    (end_ptr, unsafe_reader buffer (i + count))

  let read2 f = readn pad2 2 f
  let read4 f = readn pad4 4 f
  let read8 f = readn pad8 8 f

  let rwrap reader f buffer i = let i, v = reader buffer i in (i, f v)

  let ruint8 = read1 unsafe_read_byte_as_int
  let ruint = read4 unsafe_read_uint32_as_int
  let rsig_len = ruint8
  let rlen = ruint


  let rbyte = read1 unsafe_read_byte_as_char
  let rint16 = read2 unsafe_read_int16_as_int
  let rint32 = read4 unsafe_read_int32_as_int32
  let rint64 = read8 unsafe_read_int64_as_int64
  let ruint16 = read2 unsafe_read_uint16_as_int
  let ruint32 = read4 unsafe_read_uint32_as_int32
  let ruint64 = read8 unsafe_read_uint64_as_int64
  let rdouble = rwrap ruint64 Int64.to_float
  let rboolean = rwrap ruint
    (function
       | 0 -> false
       | 1 -> true
       | n -> raise & Reading_error ("invalid boolean value: " ^ string_of_int n))

  let runsafe_string buffer i =
    let i, len = rlen buffer i in
    let end_ptr = i + len + 1 in
    if len < 0 || end_ptr > String.length buffer then raise Out_of_bounds;
    let str = String.create len in
    String.unsafe_blit buffer i str 0 len;
    match String.unsafe_get buffer (i + len) with
      | '\x00' -> (end_ptr, str)
      | _ -> raise & Reading_error "string terminal null byte missing"

  let robject_path buffer i =
    let i, len = rlen buffer i in
    if len < 0 || i + len + 1 > String.length buffer then raise Out_of_bounds;
    (* Check for the initial '/' *)
    if String.unsafe_get buffer i <> '/' then
      raise & Reading_error "object path do not start with a '/'";
    if String.unsafe_get buffer (i + len) <> '\x00' then
      raise & Reading_error "object path terminal null byte missing";
    let rec aux acc j =
      if j < i
      then (i + len + 1, acc)
      else begin
        (* rindex_from never fail here since we know there is a '/' at
           the begining *)
        let k = String.rindex_from buffer j '/' in
        let len = j - k in
        let elt = String.create len in
        String.unsafe_blit buffer (k + 1) elt 0 len;
        match OBus_path.test_element elt with
          | None -> aux (elt :: acc) (k - 1)
          | Some msg -> raise & Reading_error ("invalid object path: " ^ msg)
      end
    in
    aux [] (i + len - 1)

  let rstring = rwrap runsafe_string
    (fun str ->
       validate_string (fun exn -> Reading_error exn) str;
       str)

  module Types_reader = Types_rw.Make_reader(OBus_value)
    (struct
       let get = String.unsafe_get
       let terminated str i = String.unsafe_get str i = '\x00'
     end)

  let rsignature buffer i =
    let i, len = rsig_len buffer i in
    if len < 0 || i + len + 1 > String.length buffer then raise Out_of_bounds;
    if String.unsafe_get buffer (i + len) <> '\x00'
    then raise & Reading_error "signature does not end with a null byte";
    try
      Types_reader.read_sequence buffer i
    with
        Types_rw.Parse_failure(j, msg) ->
          raise & Reading_error
            (sprintf "invalid signature %S, at position %d: %s"
               (String.sub buffer i len) (j - i) msg)

  let rtype buffer i = rwrap rsignature
    (function
       | [t] -> t
       | [] -> raise & Reading_error "empty variant signature"
       | _ -> raise & Reading_error "variant signature contain more than one single type")
    buffer i

  let rstruct reader buffer i = reader buffer (rpad8 buffer i)

  let rlist typ reader buffer i =
    let i, len = ruint buffer i in
    rcheck_array_len len;
    let i = if pad8_p typ then rpad8 buffer i else i in
    let limit = i + len in
    let rec aux i =
      if i < limit
      then
        let i, v = reader buffer i in
        v :: aux i
      else
        if i > limit
        then raise (Reading_error "invalid array size")
        else []
    in
    (limit, aux i)

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
    | Tarray t ->
        begin match t with
          | Tdict_entry(tk, tv) ->
              let kreader = rbasic tk
              and vreader = rsingle tv in
              (fun buffer i ->
                 let i, len = ruint buffer i in
                 rcheck_array_len len;
                 let i = rpad8 buffer i in
                 let limit = i + len in
                 let rec aux i acc =
                   if i < limit
                   then begin
                     let i = rpad8 buffer i in
                     let i, k = kreader buffer i in
                     let i, v = vreader buffer i in
                     aux i (Dict_entry(k, v) :: acc)
                   end else
                     if i > limit
                     then raise & Reading_error "invalid array size"
                     else (i, varray t acc)
                 in
                 aux i [])
          | Tsingle t' ->
              let reader = rsingle t' in
              (fun buffer i ->
                 let i, len = ruint buffer i in
                 rcheck_array_len len;
                 let i = if single_pad8_p t' then rpad8 buffer i else i in
                 let limit = i + len in
                 let rec aux i =
                   if i < limit
                   then begin
                     let i, x = reader buffer i in
                     Single x :: aux i
                   end else
                     if i > limit
                     then raise & Reading_error "invalid array size"
                     else []
                 in
                 limit, varray t (aux i))
        end
    | Tstruct tl -> rwrap (rstruct (rsequence tl)) vstruct
    | Tvariant -> rvariant

  and rvariant buffer i =
    let i, t = rtype buffer i in
    let i, v = rsingle t buffer i in
    (i, vvariant v)

  and rsequence t buffer i = match t with
    | [] -> (i, [])
    | t :: tl ->
        let i, x = rsingle t buffer i in
        let i, l = rsequence tl buffer i in
        (i, x :: l)
end
