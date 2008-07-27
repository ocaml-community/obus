(*
 * oBus_annot.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_types

type ext_basic =
  | Ta_byte
  | Ta_boolean
  | Ta_int8
  | Ta_uint8
  | Ta_int16
  | Ta_int32
  | Ta_int64
  | Ta_uint16
  | Ta_uint32
  | Ta_uint64
  | Ta_int
  | Ta_uint
  | Ta_double
  | Ta_string
  | Ta_signature
  | Ta_object_path
  | Ta_flag of ext_basic * string * string list
  | Ta_bitwise of ext_basic * string * string list

type ext_single =
  | Ta_basic of ext_basic
  | Ta_struct of ext_sequence
  | Ta_array of ext_single
  | Ta_dict of ext_basic * ext_single
  | Ta_byte_array
  | Ta_variant

and ext_sequence =
  | Ta_cons of ext_sequence * ext_sequence
  | Ta_one of ext_single
  | Ta_nil

type (+'a, +'b) t = Annot of ext_sequence
type (+'a, +'b) one = ('a, 'b * 'a) t
type 'a dbasic
type 'a basic_p = (unit, 'a) one
constraint 'a = _ dbasic
type 'a single_p = (unit, 'a) one
type 'a sequence_p = (unit, 'a) t

(* With GADTs, [annot] would be:

   type ('a, 'b) annot =
     | Ta_cons : ('a, 'b) annot -> ('b, 'c) annot -> ('a, 'c) annot
     | Ta_one : 'a annot_single -> ('b, 'a * 'b) annot
     | Ta_nil : ('a, 'a) annot
*)

(* This is emulated by the type of the [d*] functions, this is why the
   following function is correct (i.e. [assert false] is never
   reached): *)

let rec get_uniq seq =
  let rec aux seq cont _ = match seq with
    | Ta_one x -> x
    | Ta_cons(a, b) -> aux a (aux b cont) ()
    | Ta_nil -> cont ()
  in
    aux seq (fun _ -> assert false) ()

let map_ext_sequence map seq =
  let rec aux seq cont acc = match seq with
    | Ta_one x -> cont (map x :: acc)
    | Ta_cons(a, b) -> aux b (aux a cont) acc
    | Ta_nil -> cont acc
  in
    aux seq (fun l -> l) []

let ext_basic_of_annot (Annot seq) = match get_uniq seq with
  | Ta_basic t -> t
  | _ -> assert false (* Same reason here *)
let ext_single_of_annot (Annot seq) = get_uniq seq
let ext_sequence_of_annot (Annot seq) = seq

let dbyte = Annot(Ta_one(Ta_basic Ta_byte))
let dboolean = Annot(Ta_one(Ta_basic Ta_boolean))
let dint8 = Annot(Ta_one(Ta_basic Ta_int8))
let duint8 = Annot(Ta_one(Ta_basic Ta_uint8))
let dint16 = Annot(Ta_one(Ta_basic Ta_int16))
let dint32 = Annot(Ta_one(Ta_basic Ta_int32))
let dint64 = Annot(Ta_one(Ta_basic Ta_int64))
let duint16 = Annot(Ta_one(Ta_basic Ta_uint16))
let duint32 = Annot(Ta_one(Ta_basic Ta_uint32))
let duint64 = Annot(Ta_one(Ta_basic Ta_uint64))
let dint = Annot(Ta_one(Ta_basic Ta_int))
let duint = Annot(Ta_one(Ta_basic Ta_uint))
let ddouble = Annot(Ta_one(Ta_basic Ta_double))
let dstring = Annot(Ta_one(Ta_basic Ta_string))
let dsignature = Annot(Ta_one(Ta_basic Ta_signature))
let dobject_path = Annot(Ta_one(Ta_basic Ta_object_path))
let dflag t n l = Annot(Ta_one(Ta_basic(Ta_flag(ext_basic_of_annot t, n, l))))
let dbitwise t n l = Annot(Ta_one(Ta_basic(Ta_bitwise(ext_basic_of_annot t, n, l))))
let dstruct (Annot tl) = Annot(Ta_one(Ta_struct tl))
let darray t = Annot(Ta_one(Ta_array(ext_single_of_annot t)))
let ddict tk tv = Annot(Ta_one(Ta_dict(ext_basic_of_annot tk, ext_single_of_annot tv)))
let dbyte_array = Annot(Ta_one Ta_byte_array)
let dvariant = Annot(Ta_one(Ta_variant))
let dpair (Annot a) (Annot b) = Annot(Ta_cons(a, b))
let (++) = dpair
let dnil = Annot Ta_nil

let rec basic_type_of_ext = function
  | Ta_byte -> Tbyte
  | Ta_boolean -> Tboolean
  | Ta_int8 -> Tbyte
  | Ta_uint8 -> Tbyte
  | Ta_int16 -> Tint16
  | Ta_int32 -> Tint32
  | Ta_int64 -> Tint64
  | Ta_uint16 -> Tuint16
  | Ta_uint32 -> Tuint32
  | Ta_uint64 -> Tuint64
  | Ta_int -> Tint32
  | Ta_uint -> Tuint32
  | Ta_double -> Tdouble
  | Ta_string -> Tstring
  | Ta_signature -> Tsignature
  | Ta_object_path -> Tobject_path
  | Ta_flag(t, name, l) -> basic_type_of_ext t
  | Ta_bitwise(t, name, l) -> basic_type_of_ext t

let rec single_type_of_ext = function
  | Ta_basic t -> Tbasic(basic_type_of_ext t)
  | Ta_struct tl -> Tstruct(sequence_type_of_ext tl)
  | Ta_array t -> Tarray(single_type_of_ext t)
  | Ta_dict(tk, tv) -> Tdict(basic_type_of_ext tk, single_type_of_ext tv)
  | Ta_byte_array -> Tarray(Tbasic Tbyte)
  | Ta_variant -> Tvariant

and sequence_type_of_ext seq = map_ext_sequence single_type_of_ext seq

let default_ext_of_basic = function
  | Tbyte -> Ta_byte
  | Tboolean -> Ta_boolean
  | Tint16 -> Ta_int16
  | Tint32 -> Ta_int
  | Tint64 -> Ta_int64
  | Tuint16 -> Ta_uint16
  | Tuint32 -> Ta_uint
  | Tuint64 -> Ta_uint64
  | Tdouble -> Ta_double
  | Tstring -> Ta_string
  | Tsignature -> Ta_signature
  | Tobject_path -> Ta_object_path

let rec default_ext_of_single = function
  | Tbasic t -> Ta_basic(default_ext_of_basic t)
  | Tarray(Tbasic Tbyte) -> Ta_byte_array
  | Tarray t -> Ta_array(default_ext_of_single t)
  | Tdict(tk, tv) -> Ta_dict(default_ext_of_basic tk, default_ext_of_single tv)
  | Tstruct tl -> Ta_struct(default_ext_of_sequence tl)
  | Tvariant -> Ta_variant

and default_ext_of_sequence = function
  | [] -> Ta_nil
  | t :: tl -> Ta_cons(Ta_one(default_ext_of_single t),
                       default_ext_of_sequence tl)

let canonicalize_basic = function
  | Ta_flag(t, n, l) -> Ta_flag(t, n, List.sort compare l)
  | Ta_bitwise(t, n, l) -> Ta_bitwise(t, n, List.sort compare l)
  | t -> t

let rec canonicalize_single = function
  | Ta_basic t -> Ta_basic(canonicalize_basic t)
  | Ta_array t -> Ta_array(canonicalize_single t)
  | Ta_dict(tk, tv) -> Ta_dict(canonicalize_basic tk,
                               canonicalize_single tv)
  | Ta_struct tl -> Ta_struct(canonicalize_sequence tl)
  | Ta_byte_array -> Ta_byte_array
  | Ta_variant -> Ta_variant

and canonicalize_sequence tl =
  let rec aux = function
    | [] -> Ta_nil
    | x :: l -> Ta_cons(Ta_one x, aux l)
  in
    aux (map_ext_sequence canonicalize_single tl)

open Printf

let select ob comb normal = match ob with
  | true -> comb
  | false -> normal

let string_of_basic ob = function
  | Ta_byte -> "char"
  | Ta_boolean -> "bool"
  | Ta_int8 -> select ob "int8" "int"
  | Ta_uint8 -> select ob "uint8" "int"
  | Ta_int16 -> select ob "int16" "int"
  | Ta_int32 -> select ob "int32" "int32"
  | Ta_int64 -> select ob "int64" "int64"
  | Ta_uint16 -> select ob "uint16" "int"
  | Ta_uint32 -> select ob "uint32" "int32"
  | Ta_uint64 -> select ob "uint64" "int64"
  | Ta_int -> "int"
  | Ta_uint -> select ob "uint" "int"
  | Ta_double -> "float"
  | Ta_string -> "string"
  | Ta_signature -> select ob "signature" "OBus_types.signature"
  | Ta_object_path -> select ob "path" "OBus_path.t"
  | Ta_flag(_, name, _) -> name
  | Ta_bitwise(_, name, _) -> name

type pos =
  | In_tuple
  | In_params
  | The_param
  | At_top

let paren pos s = match pos with
  | The_param -> sprintf "(%s)" s
  | _ -> s

let rec _string_of_single ob pos = function
  | Ta_basic t -> string_of_basic ob t
  | Ta_struct tl ->
      if ob
      then sprintf "%s structure" (_string_of_sequence ob The_param tl)
      else _string_of_sequence ob pos tl
  | Ta_array t -> paren pos (sprintf "%s list" (_string_of_single ob The_param t))
  | Ta_dict(tk, tv) ->
      paren pos (sprintf "(%s, %s) %s"
                   (string_of_basic ob tk)
                   (_string_of_single ob In_params tv)
                   (select ob "assoc" "list"))
  | Ta_byte_array -> sprintf "byte_array"
  | Ta_variant -> sprintf "variant"

and _string_of_sequence ob pos seq =
  match map_ext_sequence (fun x -> x) seq with
    | [] -> "unit"
    | [t] -> _string_of_single ob pos t
    | l -> let s = String.concat " * " (List.map (_string_of_single ob In_tuple) l) in
        match pos with
          | The_param
          | In_tuple -> sprintf "(%s)" s
          | _ -> s

let string_of_single ob = _string_of_single ob At_top
let string_of_sequence ob = _string_of_sequence ob At_top

type dbasic_byte
type dbasic_boolean
type dbasic_int8
type dbasic_uint8
type dbasic_int16
type dbasic_int32
type dbasic_int64
type dbasic_uint16
type dbasic_uint32
type dbasic_uint64
type dbasic_int
type dbasic_uint
type dbasic_double
type dbasic_string
type dbasic_signature
type dbasic_object_path
type dbyte = dbasic_byte dbasic
type dboolean = dbasic_boolean dbasic
type dint8 = dbasic_int8 dbasic
type duint8 = dbasic_uint8 dbasic
type dint16 = dbasic_int16 dbasic
type dint32 = dbasic_int32 dbasic
type dint64 = dbasic_int64 dbasic
type duint16 = dbasic_uint16 dbasic
type duint32 = dbasic_uint32 dbasic
type duint64 = dbasic_uint64 dbasic
type dint = dbasic_int dbasic
type duint = dbasic_uint dbasic
type ddouble = dbasic_double dbasic
type dstring = dbasic_string dbasic
type dsignature = dbasic_signature dbasic
type dobject_path = dbasic_object_path dbasic
type 'a dstruct
type 'a darray
type dbyte_array
type ('a, 'b) ddict
constraint 'a = _ dbasic
type dvariant

type dunknown
let make_unknown t = (t :> (dunknown, dunknown) t)
