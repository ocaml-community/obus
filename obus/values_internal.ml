(*
 * values_internal.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module T = Types_internal

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
  | Signature of T.t
  | Object_path of string

type single =
  | Basic of basic
  | Array of T.single * single list
  | Dict of T.basic * T.single * (basic * single) list
  | Structure of single list
  | Variant of single

type t = single list

let type_of_basic = function
  | Byte _ -> T.Byte
  | Boolean _ -> T.Boolean
  | Int16 _ -> T.Int16
  | Int32 _ -> T.Int32
  | Int64 _ -> T.Int64
  | Uint16 _ -> T.Uint16
  | Uint32 _ -> T.Uint32
  | Uint64 _ -> T.Uint64
  | Double _ -> T.Double
  | String _ -> T.String
  | Signature _ -> T.Signature
  | Object_path _ -> T.Object_path

let rec type_of_single = function
  | Basic v -> T.Basic(type_of_basic v)
  | Array(t, _) -> T.Array(t)
  | Dict(tk, tv, _) -> T.Dict(tk, tv)
  | Structure(l) -> T.Structure(type_of_t l)
  | Variant _ -> T.Variant

and type_of_t l = List.map type_of_single l

let string_of_basic = function
  | Byte(c) -> "Byte('" ^ Char.escaped c ^ "')"
  | Boolean(b) -> "Boolean(" ^ (if b then "true" else "false") ^ ")"
  | Int16(i) -> "Int16(" ^ string_of_int i ^ ")"
  | Int32(i) -> "Int32(" ^ Int32.to_string i ^ ")"
  | Int64(i) -> "Int64(" ^ Int64.to_string i ^ ")"
  | Uint16(i) -> "Uint16(" ^ string_of_int i ^ ")"
  | Uint32(i) -> "Uint32(" ^ Int32.to_string i ^ ")"
  | Uint64(i) -> "Uint64(" ^ Int64.to_string i ^ ")"
  | Double(d) -> "Double(" ^ string_of_float d ^ ")"
  | String(s) -> "String(\"" ^ s ^ "\")"
  | Signature(s) -> "Signature(" ^ T.string_of_t s ^ ")"
  | Object_path(s) -> "Object_path(" ^ s ^ ")"

let rec string_of_single = function
  | Basic(t) -> string_of_basic t
  | Array(t, vs) -> "Array(" ^ T.string_of_single t ^ ", " ^ string_of_t vs ^ ")"
  | Dict(tk, tv, vs) -> "Dict(" ^ T.string_of_basic tk ^ ", " ^ T.string_of_single tv ^
      ", " ^ "[" ^ String.concat "; "
        (List.map (fun (k, v) -> string_of_basic k ^ ", " ^ string_of_single v) vs) ^ "])"
  | Structure(t) -> "Structure(" ^  string_of_t t ^ ")"
  | Variant(x) -> "Variant(" ^ string_of_single x ^ ")"

and string_of_t t = "[" ^ String.concat "; " (List.map string_of_single t) ^ "]"

type 'a tbasic = basic
type 'a tsingle = single
type ('a, 'b) tlist = t

let byte x = Byte(x)
let boolean x = Boolean(x)
let int16 x = Int16(x)
let int32 x = Int32(x)
let int64 x = Int64(x)
let uint16 x = Uint16(x)
let uint32 x = Uint32(x)
let uint64 x = Uint64(x)
let double x = Double(x)
let string x = String(x)
let signature x = Signature(x)
let object_path x = Object_path(x)
let basic x = Basic(x)
let array t x = Array(t, x)
let dict tk tv vs = Dict(tk, tv, vs)
let structure x = Structure(x)
let variant x = Variant(x)
let cons x l = x :: l
let nil = []
let concat a b = a @ b

let basic_of_tbasic x = x
let single_of_tsingle x = x
let list_of_tlist x = x

let type_of_tbasic = type_of_basic
let type_of_tsingle = type_of_single
let type_of_tlist = type_of_t
