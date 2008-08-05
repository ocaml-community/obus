(*
 * oBus_types.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf

module T =
struct
  type basic =
    | Tbyte
    | Tboolean
    | Tint16
    | Tint32
    | Tint64
    | Tuint16
    | Tuint32
    | Tuint64
    | Tdouble
    | Tstring
    | Tsignature
    | Tobject_path
  type single =
    | Tbasic of basic
    | Tstruct of single list
    | Tarray of single
    | Tdict of basic * single
    | Tvariant
end

include T

type sequence = single list
type signature = sequence

let string_of_basic = function
  | Tbyte -> "Tbyte"
  | Tboolean -> "Tboolean"
  | Tint16 -> "Tint16"
  | Tint32 -> "Tint32"
  | Tint64 -> "Tint64"
  | Tuint16 -> "Tuint16"
  | Tuint32 -> "Tuint32"
  | Tuint64 -> "Tuint64"
  | Tdouble -> "Tdouble"
  | Tstring -> "Tstring"
  | Tsignature -> "Tsignature"
  | Tobject_path -> "Tobject_path"

let rec string_of_single = function
  | Tbasic t -> sprintf "Tbasic %s" (string_of_basic t)
  | Tarray t -> sprintf "Tarray(%s)" (string_of_single t)
  | Tdict(k, v) -> sprintf "Tdict(%s, %s)" (string_of_basic k) (string_of_single v)
  | Tstruct tl -> sprintf "Tstruct %s" (string_of_sequence tl)
  | Tvariant -> "Tvariant"

and string_of_sequence tl = sprintf "[%s]" (String.concat "; " (List.map string_of_single tl))

open Types_rw

module Parser_params =
struct
  let get str i =
    if i > String.length str
    then fail i "unterminated signature"
    else String.unsafe_get str i

  let terminated str i = i = String.length str
end

module R = Make_reader(T)(Parser_params)
module W = Make_writer(T)

let string_of_signature ts =
  let len = W.signature_size ts in
  let str = String.create len in
    ignore (W.write_sequence str 0 ts);
    str

let signature_of_string signature =
  try
    snd (R.read_sequence signature 0)
  with
      Parse_failure(i, msg) ->
        raise (Invalid_argument
                 (sprintf "signature_of_string: invalid signature %S, at position %d: %s" signature i msg))

type (+'a, +'b) annot = Annot of (sequence -> sequence)
type (+'a, +'b) one = ('a, 'b * 'a) annot

type dbasic_byte
type dbasic_boolean
type dbasic_int16
type dbasic_int32
type dbasic_int64
type dbasic_uint16
type dbasic_uint32
type dbasic_uint64
type dbasic_double
type dbasic_string
type dbasic_signature
type dbasic_object_path
type 'a dbasic
type dbyte = dbasic_byte dbasic
type dboolean = dbasic_boolean dbasic
type dint16 = dbasic_int16 dbasic
type dint32 = dbasic_int32 dbasic
type dint64 = dbasic_int64 dbasic
type duint16 = dbasic_uint16 dbasic
type duint32 = dbasic_uint32 dbasic
type duint64 = dbasic_uint64 dbasic
type ddouble = dbasic_double dbasic
type dstring = dbasic_string dbasic
type dsignature = dbasic_signature dbasic
type dobject_path = dbasic_object_path dbasic
type 'a dstruct
type 'a darray
type ('a, 'b) ddict
constraint 'a = _ dbasic
type dvariant
type 'a basic_p = (unit, 'a) one
constraint 'a = _ dbasic
type 'a single_p = (unit, 'a) one
type 'a sequence_p = (unit, 'a) annot

let basic_of_annot (Annot seq) = match seq [] with
  | [Tbasic t] -> t
  | _ -> assert false
      (* Phantom types assure that this will never happen *)
let single_of_annot (Annot seq) = match seq [] with
  | [t] -> t
  | _ -> assert false
let sequence_of_annot (Annot seq) = seq []

let one t = fun l -> t :: l
let one_basic t = one (Tbasic t)

let dbyte = Annot(one_basic Tbyte)
let dboolean = Annot(one_basic Tboolean)
let dint16 = Annot(one_basic Tint16)
let dint32 = Annot(one_basic Tint32)
let dint64 = Annot(one_basic Tint64)
let duint16 = Annot(one_basic Tuint16)
let duint32 = Annot(one_basic Tuint32)
let duint64 = Annot(one_basic Tuint64)
let ddouble = Annot(one_basic Tdouble)
let dstring = Annot(one_basic Tstring)
let dsignature = Annot(one_basic Tsignature)
let dobject_path = Annot(one_basic Tobject_path)
let dstruct (Annot tl) = Annot(one (Tstruct(tl [])))
let darray t = Annot(one (Tarray(single_of_annot t)))
let ddict tk tv = Annot(one (Tdict(basic_of_annot tk, single_of_annot tv)))
let dvariant = Annot(one Tvariant)
let dpair (Annot a) (Annot b) = Annot(fun l -> a (b l))
let (++) = dpair
let dnil = Annot(fun l -> l)
