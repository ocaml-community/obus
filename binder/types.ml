(*
 * types.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type ident = Camlp4.PreCast.Ast.ident
type expr = Camlp4.PreCast.Ast.expr
type patt = Camlp4.PreCast.Ast.patt

type mono
type poly =
  | Intern of int
  | UserVar of string

type ('t, 'var) term =
  | Type of 't * ('t, 'var) term list
  | Cons of ('t, 'var) term * ('t, 'var) term
  | Nil
  | Var of 'var

type 'a typ = ('a, mono) term
type 'a pattern = ('a, poly) term

let fresh =
  let c = ref 0 in
    fun () ->
      incr c; Var(Intern !c)

let typ id args = Type(id, args)
let v x = Var(UserVar x)
let cons x y = match y with
  | Cons _
  | Var _
  | Nil -> Cons(x, y)
  | _ -> raise (Invalid_argument "only Cons, Nil and variables are allowed in the right side of a cons")
let nil = Nil
let tuple l = List.fold_right (fun x acc -> Cons(x, acc)) l Nil
let rec list_of_tuple = function
  | Cons(x, y) -> x :: list_of_tuple y
  | Nil -> []
  | _ -> assert false


include Common

type dbus_id =
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
  | Array
  | Dict
  | Structure
  | Variant
type 'a dbus_type = (dbus_id, 'a) term

let dbyte = typ Byte []
let dboolean = typ Boolean []
let dint16 = typ Int16 []
let dint32 = typ Int32 []
let dint64 = typ Int64 []
let duint16 = typ Uint16 []
let duint32 = typ Uint32 []
let duint64 = typ Uint64 []
let ddouble = typ Double []
let dstring = typ String []
let dsignature = typ Signature []
let dobject_path = typ Object_path []
let darray t = typ Array [t]
let ddict k v = typ Dict [k; v]
let dstructure l = typ Structure [l]
let dvariant = typ Variant []

let rec dbus_type_of_dtype = function
  | Tbyte -> dbyte
  | Tboolean -> dboolean
  | Tint16 -> dint16
  | Tint32 -> dint32
  | Tint64 -> dint64
  | Tuint16 -> duint16
  | Tuint32 -> duint32
  | Tuint64 -> duint64
  | Tdouble -> ddouble
  | Tstring -> dstring
  | Tsignature -> dsignature
  | Tobject_path -> dobject_path
  | Tarray(t) -> darray (dbus_type_of_dtype t)
  | Tdict(k, v) -> ddict (dbus_type_of_dtype k) (dbus_type_of_dtype v)
  | Tstructure(l) -> dstructure (dbus_type_of_dtypes l)
  | Tvariant -> dvariant
and dbus_type_of_dtypes l = tuple (List.map dbus_type_of_dtype l)

type caml_id = string
type 'a caml_type = (caml_id, 'a) term

let int = typ "int" []
let int32 = typ "int32" []
let int64 = typ "int64" []
let float = typ "float" []
let bool = typ "bool" []
let char = typ "char" []
let string = typ "string" []
let list x = typ "list" [x]
let array x = typ "array" [x]
let dbus_value = typ "OBus.Values.value" []
let dbus_types = typ "OBus.Values.dtypes" []
