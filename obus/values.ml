(*
 * values.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

include Common

type value =
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
  | Signature of dtypes
  | Object_path of string
  | Array of dtype * value list
  | Dict of dtype * dtype * (value * value) list
  | Structure of value list
  | Variant of value
type values = value list

let rec dtype_of_value = function
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
  | Array(t, _) -> Tarray(t)
  | Dict(tk, tv, _) -> Tdict(tk, tv)
  | Structure(l) -> Tstructure(dtypes_of_values l)
  | Variant _ -> Tvariant
and dtypes_of_values values = List.map dtype_of_value values

let rec string_of_dtype = function
  | Tbyte -> "Byte"
  | Tboolean -> "Boolean"
  | Tint16 -> "Int16"
  | Tint32 -> "Int32"
  | Tint64 -> "Int64"
  | Tuint16 -> "Uint16"
  | Tuint32 -> "Uint32"
  | Tuint64 -> "Uint64"
  | Tdouble -> "Double"
  | Tstring -> "String"
  | Tsignature -> "Signature"
  | Tobject_path -> "Object_path"
  | Tarray(t) -> "Array(" ^ string_of_dtype t ^ ")"
  | Tdict(tk, tv) -> "Dict(" ^ string_of_dtype tk ^ ", " ^ string_of_dtype tv ^ ")"
  | Tstructure(t) -> "Structure(" ^ string_of_dtypes t ^ ")"
  | Tvariant -> "Variant"
and string_of_dtypes types = "[" ^ String.concat "; " (List.map string_of_dtype types) ^ "]"

let rec string_of_value = function
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
  | Signature(s) -> "Signature(" ^ string_of_dtypes s ^ "])"
  | Object_path(s) -> "Object_path(" ^ s ^ ")"
  | Array(t, vs) -> "Array(" ^ string_of_dtype t ^ ", " ^ string_of_values vs ^ ")"
  | Dict(tk, tv, vs) -> "Dict(" ^ string_of_dtype tk ^ ", " ^ string_of_dtype tv ^
      ", " ^ "[" ^ String.concat "; "
        (List.map (fun (k, v) -> string_of_value k ^ ", " ^ string_of_value v) vs) ^ "])"
  | Structure(t) -> "Structure(" ^ string_of_values t ^ ")"
  | Variant(x) -> "Variant(" ^ string_of_value x ^ ")"
and string_of_values values = "[" ^ String.concat "; " (List.map string_of_value values) ^ "]"

(** {6 DBus types/values construction} *)

type ('a, 'is_basic) cstr = dtype * ('a -> value) * (value -> 'a)
type yes
type no
type 'a seq_cstr = dtypes * ('a -> values) * (values -> 'a)

let invalid_type () = raise (Failure "invalid type")

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
let varray t x = Array(t, x)
let vdict tk tv x = Dict(tk, tv, x)
let vstructure x = Structure x
let vvariant x = Variant x

let byte = (Tbyte,
            vbyte,
            (function
               | Byte x -> x
               | _ -> invalid_type ()))
let boolean = (Tboolean,
               vboolean,
               (function
                  | Boolean x -> x
                  | _ -> invalid_type ()))
let int16 = (Tint16,
             vint16,
             (function
                | Int16 x -> x
                | _ -> invalid_type ()))
let int32 = (Tint32,
             vint32,
             (function
                | Int32 x -> x
                | _ -> invalid_type ()))
let int64 = (Tint64,
             vint64,
             (function
                | Int64 x -> x
                | _ -> invalid_type ()))
let uint16 = (Tuint16,
              vuint16,
              (function
                 | Uint16 x -> x
                 | _ -> invalid_type ()))
let uint32 = (Tuint32,
              vuint32,
              (function
                 | Uint32 x -> x
                 | _ -> invalid_type ()))
let uint64 = (Tuint64,
              vuint64,
              (function
                 | Uint64 x -> x
                 | _ -> invalid_type ()))
let double = (Tdouble,
              vdouble,
              (function
                 | Double x -> x
                 | _ -> invalid_type ()))
let string = (Tstring,
              vstring,
              (function
                 | String x -> x
                 | _ -> invalid_type ()))
let signature = (Tsignature,
                 vsignature,
                 (function
                    | Signature x -> x
                    | _ -> invalid_type ()))
let object_path = (Tobject_path,
                   vobject_path,
                   (function
                      | Object_path x -> x
                      | _ -> invalid_type ()))
let array (t, f, g) = (Tarray t,
                       (fun x -> Array (t, List.map f x)),
                       (function
                          | Array(_, x) -> List.map g x
                          | _ -> invalid_type ()))
let dict (tk, fk, gk) (tv, fv, gv) = (Tdict(tk, tv),
                                      (fun x -> Dict(tk, tv, List.map (fun (k, v) -> (fk k, fv v)) x)),
                                      (function
                                         | Dict(_, _, x) -> List.map (fun (k, v) -> (gk k, gv v)) x
                                         | _ -> invalid_type ()))
let structure (t, f, g) = (Tstructure t,
                           (fun x -> Structure(f x)),
                           (function
                              | Structure x -> g x
                              | _ -> invalid_type ()))
let variant = (Tvariant,
               vvariant,
               (function
                  | Variant x -> x
                  | _ -> invalid_type ()))
let cons (t, f, g) (tl, fl, gl) = (t :: tl,
                                   (fun (x,y) -> f x :: fl y),
                                   (function
                                      | x :: y -> (g x, gl y)
                                      | _ -> invalid_type ()))
let nil = ([],
           (fun () -> []),
           (function
              | [] -> ()
              | _ -> invalid_type ()))

let make_dtype (t, _, _) = t
let make_value (_, f, _) = f
let make_dtypes = make_dtype
let make_values = make_value
let get_value (_, _, g) = g
let get_values = get_value

open Wire

module type Reader = sig
  val read_value : buffer -> ptr -> dtype -> value
  val read_values : buffer -> ptr -> dtypes -> values
end

module type Writer = sig
  val write_value : buffer -> ptr -> value -> buffer * ptr
  val write_values : buffer -> ptr -> values -> buffer * ptr
end

;;
INCLUDE "obus/values-generated.ml"
