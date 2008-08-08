(*
 * oBus_wire.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open OBus_types
open OBus_value
open OBus_intern
open OBus_info
open Wire

let (|>) f g x = g (f x)
let ($) a b = a b

let padded_on_8 = function
  | Tstruct _
  | Tbasic Tint64
  | Tbasic Tuint64
  | Tbasic Tdouble -> true
  | _ -> false

type accu = int
type reader
type writer
type ('a, +'b, +'c, 'typ) t = context -> int -> int * 'a
type ('a, +'b, +'c, 'typ) one = ('a, 'b, 'c * 'b, 'typ) t
type ('a, +'b, 'typ) null = ('a, 'b, 'b, 'typ) t
type ('a, 'b, 'typ) basic_p = ('a, unit, 'b, 'typ) one
constraint 'b = [< abasic ]
type ('a, 'b, 'typ) single_p = ('a, unit, 'b, 'typ) one
type ('a, 'b, 'typ) sequence_p = ('a, unit, 'b, 'typ) t

let bind m f ctx ptr =
  let ptr, v = m ctx ptr in
    f v ctx ptr
let return v ctx ptr = (ptr, v)
let failwith msg ctx ptr = Pervasives.failwith msg
let (>>=) = bind
let (>>) a b = a >>= (fun _ -> b)
let run m con bus_name bo buf ptr = m { connection = con;
                                        bus_name = bus_name;
                                        byte_order = bo;
                                        buffer = buf } ptr

let throw exn ctx ptr = raise exn

let check len ctx ptr =
    if len < 0 || ptr + len > String.length ctx.buffer
    then out_of_bounds ()
    else (ptr, ())

let wrap count le be ctx ptr =
  if ptr + count > String.length ctx.buffer then out_of_bounds ();
  let result = (match ctx.byte_order with | Little_endian -> le | Big_endian -> be) ctx.buffer ptr in
    (ptr + count, result)

(***** Writing *****)

let wconnection ctx ptr = (ptr, ctx.connection)
let wdestination ctx ptr = (ptr, ctx.bus_name)

let wcheck_array len ctx ptr = (ptr, write_check_array_len len)

let wblit str ctx ptr =
  let len = String.length str in
    if ptr + len > String.length ctx.buffer then out_of_bounds ();
    String.unsafe_blit str 0 ctx.buffer ptr len;
    (ptr + len, ())

module Types_writer = Types_rw.Make_writer(OBus_types)

let unsafe_write_types x ctx ptr = (Types_writer.write_sequence ctx.buffer ptr x, ())

let write1 w x = wrap 1 (w x) (w x)
let write2 le be x = wpad2 >> wrap 2 (le x) (be x)
let write4 le be x = wpad4 >> wrap 4 (le x) (be x)
let write8 le be x = wpad8 >> wrap 8 (le x) (be x)

let wbyte = write1 unsafe_write_char_as_byte
let wchar = wbyte
let wint8 v = wbyte (Char.unsafe_chr v)
let wint16 = write2 LEW.unsafe_write_int_as_int16 BEW.unsafe_write_int_as_int16
let wint32 = write4 LEW.unsafe_write_int32_as_int32 BEW.unsafe_write_int32_as_int32
let wint64 = write8 LEW.unsafe_write_int64_as_int64 BEW.unsafe_write_int64_as_int64
let wint = write4 LEW.unsafe_write_int_as_int32 BEW.unsafe_write_int_as_int32
let wuint8 = wint8
let wuint16 = write2 LEW.unsafe_write_int_as_uint16 BEW.unsafe_write_int_as_uint16
let wuint32 = write4 LEW.unsafe_write_int32_as_uint32 BEW.unsafe_write_int32_as_uint32
let wuint64 = write8 LEW.unsafe_write_int64_as_uint64 BEW.unsafe_write_int64_as_uint64
let wuint = write4 LEW.unsafe_write_int_as_uint32 BEW.unsafe_write_int_as_uint32
let wdouble v = wuint64 (Int64.of_float v)
let wfloat = wdouble
let wboolean = function
  | false -> wuint 0
  | true -> wuint 1
let wbool = wboolean
let wstring str = wuint (String.length str) >> wblit str >> wbyte '\x00'
let wobject_path = wstring
let wpath = wobject_path
let wsignature tl =
  let len = Types_writer.signature_size tl in
    wuint8 len >> check len >> unsafe_write_types tl >> wbyte '\x00'

let wstruct writer = wpad8 >> writer

let __warray on8 writer ctx ptr =
  let i = fst (wpad4 ctx ptr) in
  let j = if on8 then fst (wpad8 ctx (i + 4)) else i + 4 in
  let k = writer ctx j in
  let len = k - j in
    write_check_array_len len;
    (match ctx.byte_order with
       | Little_endian -> LEW.unsafe_write_int_as_uint32
       | Big_endian -> BEW.unsafe_write_int_as_uint32) len ctx.buffer i;
    (k, ())

let _warray on8 writer fold =
  __warray on8 (fun ctx -> fold (fun x ptr -> fst (writer x ctx ptr)))

let warray annot = _warray (padded_on_8 (single_of_annot annot))
let wdict writer = _warray true (fun x -> wpad8 >> writer x)

let rec fold_list f acc = function
  | [] -> acc
  | x :: l -> fold_list f (f x acc) l

let _wlist on8 elt_writer l = _warray on8 elt_writer (fun f acc -> fold_list f acc l)
let wlist annot = _wlist (padded_on_8 (single_of_annot annot))

let wassoc kwriter vwriter l =
  wdict
    (fun (x, y) -> kwriter x >> vwriter y)
    (fun f acc -> fold_list f acc l)

module Seq =
struct
  type 'a t = context -> int -> int

  let empty ctx ptr = ptr
  let append a b ctx ptr = b ctx (a ctx ptr)
  let concat l ctx ptr =
    List.fold_left (fun ptr m -> m ctx ptr) ptr l
  let one writer ctx ptr = fst (writer ctx ptr)
end

let warray_seq annot = __warray (padded_on_8 (single_of_annot annot))
let wdict_seq seq = __warray true seq

let wbyte_array str =
  let len = String.length str in
    wcheck_array len >> wuint len >> wblit str

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
  | Array(t, l) -> _wlist (padded_on_8 t) wsingle l
  | Dict(tk, tv, l) -> wassoc wbasic wsingle l
  | Struct l -> wstruct (wsequence l)
  | Variant v -> wvariant v

and wvariant v =
  wsignature [type_of_single v] >> wsingle v

and wsequence = function
  | [] -> return ()
  | x :: l -> wsingle x >> wsequence l

let wfixed annot writer =
  wsignature [single_of_annot annot] >> writer

(***** Reading *****)

let rconnection ctx ptr = (ptr, ctx.connection)
let rsender ctx ptr = (ptr, ctx.bus_name)

let rcheck_array len ctx ptr = (ptr, read_check_array_len len)

let rblit len ctx ptr =
  if len < 0 || ptr + len > String.length ctx.buffer then out_of_bounds ();
  let str = String.create len in
    String.unsafe_blit ctx.buffer ptr str 0 len;
    (ptr + len, str)

module Types_reader = Types_rw.Make_reader(OBus_types)
  (struct
     let get = String.unsafe_get
     let terminated str i = String.unsafe_get str i = '\x00'
   end)

let read1 r = wrap 1 r r
let read2 le be = rpad2 >> wrap 2 le be
let read4 le be = rpad4 >> wrap 4 le be
let read8 le be = rpad8 >> wrap 8 le be

let rbyte = read1 unsafe_read_byte_as_char
let rchar = rbyte
let rint8 = rbyte >>= (fun v -> let v = int_of_char v in
                         return $
                           if v <= 127
                           then v
                           else v - 256)
let rint16 = read2 LER.unsafe_read_int16_as_int BER.unsafe_read_int16_as_int
let rint32 = read4 LER.unsafe_read_int32_as_int32 BER.unsafe_read_int32_as_int32
let rint64 = read8 LER.unsafe_read_int64_as_int64 BER.unsafe_read_int64_as_int64
let rint = read4 LER.unsafe_read_int32_as_int BER.unsafe_read_int32_as_int
let ruint8 = rbyte >>= (int_of_char |> return)
let ruint16 = read2 LER.unsafe_read_uint16_as_int BER.unsafe_read_uint16_as_int
let ruint32 = read4 LER.unsafe_read_uint32_as_int32 BER.unsafe_read_uint32_as_int32
let ruint64 = read8 LER.unsafe_read_uint64_as_int64 BER.unsafe_read_uint64_as_int64
let ruint = read4 LER.unsafe_read_uint32_as_int BER.unsafe_read_uint32_as_int
let rdouble = ruint64 >>= (Int64.to_float |> return)
let rfloat = rdouble
let rboolean =
  ruint >>= function
    | 0 -> return false
    | 1 -> return true
    | n -> throw (Reading_error ("invalid boolean value: " ^ string_of_int n))
let rbool = rboolean
let rstring =
  (perform
     len <-- ruint;
     str <-- rblit len;
     rbyte >>= (function
                  | '\x00' -> return ();
                  | _ -> throw (Reading_error "terminating null byte missing"));
     return str)
let robject_path = rstring
let rpath = robject_path
let rsignature ctx i =
  let i, len = ruint8 ctx i in
    if len < 0 || i + len > String.length ctx.buffer then out_of_bounds ();
    if String.unsafe_get ctx.buffer (i + len) <> '\x00'
    then raise (Reading_error "signature does not end with a null byte");
    try
      Types_reader.read_sequence ctx.buffer i
    with
        Types_rw.Parse_failure(j, msg) ->
          raise (Reading_error
                   (sprintf "invalid signature %S, at position %d: %s"
                      (String.sub ctx.buffer i len) (j - i) msg))

let rstruct reader = rpad8 >> reader

let _rarray on8 reader acc =
  (perform
     len <-- ruint;
     rcheck_array len;
     if on8 then rpad8 else return ();
     read_until reader acc len)

let rarray annot = _rarray (padded_on_8 $ single_of_annot annot)
let rdict reader = _rarray true (fun acc -> rpad8 >> reader acc)

let _rlist on8 reader = _rarray on8
  (fun f -> reader >>= fun x -> return $ fun l -> f $ x :: l) (fun l -> l)
  >>= (fun f -> return $ f [])

let rlist annot = _rlist (padded_on_8 $ single_of_annot annot)

let rset annot reader = rarray annot
  (fun l -> reader >>= fun x -> return $ x :: l) []

let rassoc kreader vreader = rdict (fun l ->
                                      perform
                                        x <-- kreader;
                                        v <-- vreader;
                                        return $ (x, v) :: l) []
let rbyte_array =
  (perform
     len <-- ruint;
     rcheck_array len;
     rblit len)

let rvariant_type =
  rsignature >>= function
    | [t] -> return t
    | [] -> throw (Reading_error "empty variant signature")
    | _ -> throw (Reading_error "variant signature contain more than one single type")

let rbasic = function
  | Tbyte -> rbyte >>= (vbyte |> return)
  | Tboolean -> rboolean >>= (vboolean |> return)
  | Tint16 -> rint16 >>= (vint16 |> return)
  | Tint32 -> rint32 >>= (vint32 |> return)
  | Tint64 -> rint64 >>= (vint64 |> return)
  | Tuint16 -> ruint16 >>= (vuint16 |> return)
  | Tuint32 -> ruint32 >>= (vuint32 |> return)
  | Tuint64 -> ruint64 >>= (vuint64 |> return)
  | Tdouble -> rdouble >>= (vdouble |> return)
  | Tstring -> rstring >>= (vstring |> return)
  | Tsignature -> rsignature >>= (vsignature |> return)
  | Tobject_path -> robject_path >>= (vobject_path |> return)

let rec rsingle = function
  | Tbasic t -> rbasic t >>= (vbasic |> return)
  | Tarray t -> _rlist (padded_on_8 t) (rsingle t) >>= (varray t |> return)
  | Tdict(tk, tv) -> rassoc (rbasic tk) (rsingle tv) >>= (vdict tk tv |> return)
  | Tstruct tl -> rstruct (rsequence tl) >>= (vstruct |> return)
  | Tvariant -> rvariant >>= (vvariant |> return)

and rsequence = function
  | [] -> return []
  | t :: tl ->
      (perform
         x <-- rsingle t;
         l <-- rsequence tl;
         return $ x :: l)

and rvariant bo = (rvariant_type >>= rsingle) bo

let rfixed annot reader =
  let t = single_of_annot annot in
    rvariant_type
    >>= (fun t' ->
           if t <> t'
           then throw (Failure (sprintf "invalid variant signature, expected '%s', got '%s'"
                                  (string_of_signature [t])
                                  (string_of_signature [t'])))
           else reader)
