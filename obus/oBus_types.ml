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

type tree =
  | Tcons of tree * tree
  | Tone of single
  | Tnil

type (+'a, +'b) annot = tree
type (+'a, +'b) one = ('a, 'b * 'a) annot

type abasic =
  [ `byte
  | `boolean
  | `int16
  | `int32
  | `int64
  | `uint16
  | `uint32
  | `uint64
  | `double
  | `string
  | `signature
  | `object_path ]

type 'a basic_p = (unit, 'a) one
constraint 'a = [< abasic ]
type 'a single_p = (unit, 'a) one
type 'a sequence_p = (unit, 'a) annot

let rec get_one fallback = function
  | Tone t -> t
  | Tcons(x, y) -> get_one (y :: fallback) x
  | Tnil -> match fallback with
      | [] -> assert false
      | x :: l -> get_one l x

let rec get acc = function
  | Tone t -> t :: acc
  | Tcons(x, y) -> get (get acc y) x
  | Tnil -> acc

let basic_of_annot t = match get_one [] t with
  | Tbasic t -> t
  | _ -> assert false
let single_of_annot = get_one []
let sequence_of_annot = get []

let one t = fun l -> t :: l
let one_basic t = one (Tbasic t)

let dbyte = Tone(Tbasic Tbyte)
let dboolean = Tone(Tbasic Tboolean)
let dint16 = Tone(Tbasic Tint16)
let dint32 = Tone(Tbasic Tint32)
let dint64 = Tone(Tbasic Tint64)
let duint16 = Tone(Tbasic Tuint16)
let duint32 = Tone(Tbasic Tuint32)
let duint64 = Tone(Tbasic Tuint64)
let ddouble = Tone(Tbasic Tdouble)
let dstring = Tone(Tbasic Tstring)
let dsignature = Tone(Tbasic Tsignature)
let dobject_path = Tone(Tbasic Tobject_path)
let dstruct tl = Tone(Tstruct(sequence_of_annot tl))
let darray t = Tone(Tarray(single_of_annot t))
let ddict tk tv = Tone(Tdict(basic_of_annot tk, single_of_annot tv))
let dvariant = Tone Tvariant
let dpair a b = Tcons(a, b)
let (++) = dpair
let dnil = Tnil
