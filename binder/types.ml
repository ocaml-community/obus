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

type var = string

type typ =
  | Type of string * typ list
  | Cons of typ * typ
  | Nil
  | Var of var

let typ id args = Type(id, args)
let v x = Var(x)
let cons x = function
  | Nil -> x
  | y -> Cons(x, y)
let nil = Nil
let tuple = function
  | [t] -> t
  | l ->  List.fold_right (fun x acc -> Cons(x, acc)) l Nil
let rec list_of_tuple = function
  | Cons(x, y) -> x :: list_of_tuple y
  | Nil -> []
  | x -> [x]

let string_of_type =
  let at_top x = x
  and not_at_top x = "(" ^ x ^ ")" in
  let rec aux top = function
    | Type(t, []) -> t
    | Type(t, args) ->
        top (t ^ String.concat "" (List.map (fun t -> " " ^ aux not_at_top t) args))
    | Cons _ as l ->
        top (String.concat " * " (List.map (aux at_top) (list_of_tuple l)))
    | Nil -> "()"
    | Var(x) -> "'" ^ x
  in
    aux at_top

include Common

type dbus_type = dtype list

let dbus_type_of_signature = dtypes_of_signature
let signature_of_dbus_type = signature_of_dtypes

type caml_type = typ

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
