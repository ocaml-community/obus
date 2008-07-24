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

and string_of_sequence tl = sprintf "[%s]" (String.concat " * " (List.map string_of_single tl))

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

(* Representation of list adapted to the construction *)
type seq =
  | Cons of seq * seq
  | One of single
  | Nil

type ('a, 'b) annot = seq
type ('a, 'b) sannot = seq

let rec get_one seq cont _ = match seq with
  | One x -> x
  | Cons(a, b) -> get_one a (get_one b cont) ()
  | Nil -> cont ()

let rec get_seq seq cont acc = match seq with
  | One x -> cont (x :: acc)
  | Cons(a, b) -> get_seq b (get_seq a cont) acc
  | Nil -> cont acc

(* Types ensure that there is exactly one value in the sequence *)
let basic_type_of_annot t = match get_one t (fun _ -> assert false) () with
  | Tbasic t -> t
  | _ -> assert false
let single_type_of_annot t = get_one t (fun _ -> assert false) ()

let sequence_type_of_annot t = get_seq t (fun x -> x) []

type dbyte = [`byte]
type dboolean = [`boolean]
type dint16 = [`int16]
type dint32 = [`int32]
type dint64 = [`int64]
type duint16 = [`uint16]
type duint32 = [`uint32]
type duint64 = [`uint64]
type ddouble = [`double]
type dstring = [`string]
type dsignature = [`signature]
type dobject_path = [`object_path]
type dbasic =
    [ dbyte
    | dboolean
    | dint16
    | dint32
    | dint64
    | duint16
    | duint32
    | duint64
    | ddouble
    | dstring
    | dsignature
    | dobject_path ]
type 'a dstruct
type 'a darray
type ('a, 'b) ddict
constraint 'a = [< dbasic ]
type dvariant

let dbyte = One(Tbasic Tbyte)
let dboolean = One(Tbasic Tboolean)
let dint16 = One(Tbasic Tint16)
let dint32 = One(Tbasic Tint32)
let dint64 = One(Tbasic Tint64)
let duint16 = One(Tbasic Tuint16)
let duint32 = One(Tbasic Tuint32)
let duint64 = One(Tbasic Tuint64)
let ddouble = One(Tbasic Tdouble)
let dstring = One(Tbasic Tstring)
let dsignature = One(Tbasic Tsignature)
let dobject_path = One(Tbasic Tobject_path)
let dstruct tl = One(Tstruct(sequence_type_of_annot tl))
let darray t = One(Tarray(single_type_of_annot t))
let ddict tk tv = One(Tdict(basic_type_of_annot tk, single_type_of_annot tv))
let dvariant = One(Tvariant)

let dpair a b = Cons(a, b)
let (@@) = dpair
let dnil = Nil
