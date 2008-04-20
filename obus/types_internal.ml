(*
 * types_internal.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type basic =
  | Byte
  | Boolean
  | Int16
  | Int32
  | Int64
  | Uint16
  | Uint32
  | Uint64
  | Double
  | String
  | Signature
  | Object_path

type single =
  | Basic of basic
  | Array of single
  | Dict of basic * single
  | Structure of single list
  | Variant

type t = single list

let string_of_basic = function
  | Byte -> "Byte"
  | Boolean -> "Boolean"
  | Int16 -> "Int16"
  | Int32 -> "Int32"
  | Int64 -> "Int64"
  | Uint16 -> "Uint16"
  | Uint32 -> "Uint32"
  | Uint64 -> "Uint64"
  | Double -> "Double"
  | String -> "String"
  | Signature -> "Signature"
  | Object_path -> "Object_path"

let rec string_of_single = function
  | Basic(t) -> string_of_basic t
  | Array(t) -> "Array(" ^ string_of_single t ^ ")"
  | Dict(tk, tv) -> "Dict(" ^ string_of_basic tk ^ ", " ^ string_of_single tv ^ ")"
  | Structure(t) -> "Structure(" ^  string_of_t t ^ ")"
  | Variant -> "Variant"

and string_of_t t = "[" ^ String.concat "; " (List.map string_of_single t) ^ "]"

type byte
type boolean
type int16
type int32
type int64
type uint16
type uint32
type uint64
type double
type string
type signature
type object_path
type 'a array
type ('a, 'b) dict
type 'a structure
type variant
type nil

type 'a tbasic = basic
type 'a tsingle = single
type ('a, 'b) tlist = t

let byte = Byte
let boolean = Boolean
let int16 = Int16
let int32 = Int32
let int64 = Int64
let uint16 = Uint16
let uint32 = Uint32
let uint64 = Uint64
let double = Double
let string = String
let signature = Signature
let object_path = Object_path
let basic x = Basic(x)
let array t = Array(t)
let dict tk tv = Dict(tk, tv)
let structure t = Structure(t)
let variant = Variant
let cons x l = x :: l
let nil = []
let concat a b = a @ b

let basic_of_tbasic x = x
let single_of_tsingle x = x
let list_of_tlist x = x
