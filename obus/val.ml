(*
 * val.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type typ =
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
  | Tbasic of typ
  | Tarray of typ
  | Tdict of typ * typ
  | Tstructure of typ list
  | Tvariant

let rec string_of_type = function
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
  | Tarray(t) -> "Array(" ^ string_of_type t ^ ")"
  | Tdict(tk, tv) -> "Dict(" ^ string_of_type tk ^ ", " ^ string_of_type tv ^ ")"
  | Tstructure(t) -> "Structure([" ^ String.concat "; " (List.map string_of_type t) ^ "])"
  | Tvariant -> "Variant"

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
  | Signature of dtyp
  | Object_path of string
  | Basic of value
  | Array of typ * value list
  | Dict of typ * typ * (value * value) list
  | Structure of value list
  | Variant of value

let string_of_value = function
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
  | Signature(s) -> "Signature(" ^ string_of_type s ^ ")"
  | Object_path(s) -> "Object_path(" ^ s ^ ")"
  | Array(t, vs) -> "Array(" ^ string_of_type t ^ ", " ^ string_of_t vs ^ ")"
  | Dict(tk, tv, vs) -> "Dict(" ^ string_of_type tk ^ ", " ^ string_of_type tv ^
      ", " ^ "[" ^ String.concat "; "
        (List.map (fun (k, v) -> string_of_value k ^ ", " ^ string_of_value v) vs) ^ "])"
  | Structure(t) -> "Structure([" ^ String.concat "; " (List.map string_of_value t) ^ "])"
  | Variant(x) -> "Variant(" ^ string_of_value x ^ ")"

let type_of_value = function
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
  | Structure(l) -> Tstructure(type_of_value l)
  | Variant _ -> Tvariant

(** {6 DBus types/values construction} *)

type ('a, 'is_basic) cstr = typ * ('a -> value) * (value -> 'a)
type yes
type no
type 'a seq_cstr = typ list * ('a -> value list) * (value list -> 'a)

let invalid_type () = raise (Failure "invalid type")

let byte = (Tbyte,
            (fun x -> Byte x),
            (function
               | Byte x -> x
               | _ -> invalid_type ()))
let boolean = (Tboolean,
               (fun x -> Boolean x),
               (function
                  | Boolean x -> x
                  | _ -> invalid_type ()))
let int16 = (Tint16,
             (fun x -> Int16 x),
             (function
                | Int16 x -> x
                | _ -> invalid_type ()))
let int32 = (Tint32,
             (fun x -> Int32 x),
             (function
                | Int32 x -> x
                | _ -> invalid_type ()))
let int64 = (Tint64,
             (fun x -> Int64 x),
             (function
                | Int64 x -> x
                | _ -> invalid_type ()))
let uint16 = (Tuint16,
              (fun x -> Uint16 x),
              (function
                 | Uint16 x -> x
                 | _ -> invalid_type ()))
let uint32 = (Tuint32,
              (fun x -> Uint32 x),
              (function
                 | Uint32 x -> x
                 | _ -> invalid_type ()))
let uint64 = (Tuint64,
              (fun x -> Uint64 x),
              (function
                 | Uint64 x -> x
                 | _ -> invalid_type ()))
let double = (Tdouble,
              (fun x -> Double x),
              (function
                 | Double x -> x
                 | _ -> invalid_type ()))
let string = (Tstring,
              (fun x -> String x),
              (function
                 | String x -> x
                 | _ -> invalid_type ()))
let signature = (Tsignature,
                 (fun x -> Signature x),
                 (function
                    | Signature x -> x
                    | _ -> invalid_type ()))
let object_path = (Tobject_path,
                   (fun x -> Object_Path x),
                   (function
                      | Object_Path x -> x
                      | _ -> invalid_type ()))
let array (t, f, g) = (Tarray t,
                       (fun x -> Array (t, List.map f x)),
                       (function
                          | Array _ x -> List.map g x
                          | _ -> invalid_type ()))
let dict (tk, fk, gk) (tv, fv, gv) = (Tdict(tk, tv),
                                      (fun x -> Dict(tk, tv, List.map (fun (k, v) -> (fk k, fv v)) x)),
                                      (function
                                         | Dict _ _ x -> List.map (fun (k, v) -> (gk k, gv v)) x
                                         | _ -> invalid_type ()))
let structure (t, f, g) = (Tstruct t,
                           (fun x -> Structure(f x)),
                           (function
                              | Structure x -> x
                              | _ -> invalid_type ()))
let variant (t, f, g) = (Tvariant,
                         (fun x -> Variant(f x)),
                         (function
                            | Variant x -> x
                            | _ -> invalid_type ()))
let cons (t, f, g) (tl, fl, gl) = (t :: tl,
                                   (fun (x,y) -> (f x, fl y)),
                                   (function
                                      | x :: y -> (g x, gl y)
                                      | _ -> invalid_type ()))
let nil = ([],
           (fun () -> []),
           (function
              | [] -> ()
              | _ -> invalid_type ()))

let make_type (t, _, _) = t
let make_value (_, f, _) = f
let make_type_list = make_type
let make_value_list = make_value
let get (_, _, g) = g
let get_list = get
