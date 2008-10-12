(*
 * oBus_value.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf

module T =
struct
  type tbasic =
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
  type tsingle =
    | Tbasic of tbasic
    | Tstruct of tsingle list
    | Tarray of telement
    | Tvariant
  and telement =
    | Tdict_entry of tbasic * tsingle
    | Tsingle of tsingle
end

include T

type tsequence = tsingle list
type signature = tsequence

let string_of_tbasic = function
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

let rec string_of_tsingle = function
  | Tbasic t -> sprintf "Tbasic %s" (string_of_tbasic t)
  | Tarray t -> sprintf "Tarray(%s)" (string_of_telement t)
  | Tstruct tl -> sprintf "Tstruct %s" (string_of_tsequence tl)
  | Tvariant -> "Tvariant"

and string_of_telement = function
  | Tdict_entry(tk, tv) -> sprintf "Tdict_entry(%s, %s)" (string_of_tbasic tk) (string_of_tsingle tv)
  | Tsingle t -> sprintf "Tsingle(%s)" (string_of_tsingle t)

and string_of_tsequence tl = sprintf "[%s]" (String.concat "; " (List.map string_of_tsingle tl))

open Types_rw

module Id =
struct
  type 'a t = 'a
  let bind m f = f m
  let return x = x
end

module Reader_params =
struct
  include Id

  let failwith fmt = ksprintf (fun msg -> raise (Failure ("invalid signature: " ^ msg))) fmt

  type input = string * int ref

  let get (str, i) =
    let p = !i in
    if p >= String.length str then
      failwith "unterminated signature"
    else begin
      i := p + 1;
      String.unsafe_get str p
    end

  let get_opt (str, i) =
    let p = !i in
    if p >= String.length str then
      None
    else begin
      i := p + 1;
      Some (String.unsafe_get str p)
    end
end

module Writer_params =
struct
  include Id
  type output = string * int ref

  let put (str, i) ch =
    String.unsafe_set str !i ch;
    incr i
end

module R = Make_reader(T)(Reader_params)
module W = Make_writer(T)(Writer_params)

let string_of_signature ts =
  let len = W.signature_size ts in
  let str = String.create len in
  W.write_sequence (str, ref 0) ts;
  str

let signature_of_string str =
  try
    R.read_sequence (str, ref 0)
  with
      Failure msg ->
        raise (Invalid_argument
                 (sprintf "signature_of_string: invalid signature %S: %s" str msg))

type basic =
  | Byte of char
  | Boolean of bool
  | Int16 of int
  | Int32 of int32
  | Int64 of int64
  | Uint16 of int
  | Uint32 of int32
  | Uint64 of int64
  | Double of float
  | String of string
  | Signature of signature
  | Object_path of OBus_path.t

type single =
  | Basic of basic
  | Array of telement * element list
  | Struct of single list
  | Variant of single

and element =
  | Dict_entry of basic * single
  | Single of single

type sequence = single list

let type_of_basic = function
  | Byte _ -> Tbyte
  | Boolean _ -> Tboolean
  | Int16 _ -> Tint16
  | Int32 _ -> Tint32
  | Int64 _ -> Tint64
  | Uint16 _ -> Tuint16
  | Uint32 _ -> Tuint32
  | Uint64 _ -> Tuint64
  | Double _ -> Tdouble
  | String _ -> Tstring
  | Signature _ -> Tsignature
  | Object_path _ -> Tobject_path

let rec type_of_single = function
  | Basic x -> Tbasic(type_of_basic x)
  | Array(t, x) -> Tarray t
  | Struct x -> Tstruct(List.map type_of_single x)
  | Variant _ -> Tvariant

let type_of_element = function
  | Dict_entry(k, v) -> Tdict_entry(type_of_basic k, type_of_single v)
  | Single x -> Tsingle(type_of_single x)

let type_of_sequence = List.map type_of_single

let vbyte x = Byte x
let vboolean x = Boolean x
let vint16 x = Int16 x
let vint32 x = Int32 x
let vint64 x = Int64 x
let vuint16 x = Uint16 x
let vuint32 x = Uint32 x
let vuint64 x = Uint64 x
let vdouble x = Double x
let vstring x = String x
let vsignature x = Signature x
let vobject_path x = Object_path x
let vbasic x = Basic x
let varray t l =
  List.iter (fun x ->
               if type_of_element x <> t
               then failwith "OBus_value.varray: unexpected type") l;
  Array(t, l)
let vstruct l = Struct l
let vvariant v = Variant v

let vdict_entry k v = Dict_entry(k, v)
let vsingle x = Single x

open Printf

let string_of_basic = function
  | Byte x -> sprintf "Byte %C" x
  | Boolean x -> sprintf "Boolean %B" x
  | Int16 x -> sprintf "Int16 %d" x
  | Int32 x -> sprintf "Int32 %ldl" x
  | Int64 x -> sprintf "Int64 %LdL" x
  | Uint16 x -> sprintf "Uint16 %d" x
  | Uint32 x -> sprintf "Uint32 %ldl" x
  | Uint64 x -> sprintf "Uint64 %LdL" x
  | Double x -> sprintf "Double %f" x
  | String x -> sprintf "String %S" x
  | Signature x -> sprintf "Signature(%s)" (string_of_tsequence x)
  | Object_path x -> sprintf "Object_path [%s]" (String.concat "; " (List.map (sprintf "%S") x))

let rec string_of_single = function
  | Basic v -> sprintf "Basic(%s)" (string_of_basic v)
  | Array(t, l) ->
      sprintf "Array(%s, [%s])"
        (string_of_telement t)
        (String.concat "; " (List.map string_of_element l))
  | Struct l -> sprintf "Structure %s" (string_of_sequence l)
  | Variant x -> sprintf "Variant(%s)" (string_of_single x)

and string_of_element = function
  | Dict_entry(k, v) -> sprintf "Dict_entry(%s, %s)" (string_of_basic k) (string_of_single v)
  | Single x -> sprintf "Single(%s)" (string_of_single x)

and string_of_sequence l = sprintf "[%s]" (String.concat "; " (List.map string_of_single l))
