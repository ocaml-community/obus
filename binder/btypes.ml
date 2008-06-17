(*
 * btypes.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Helpers

let _loc = Loc.ghost

type ident = Ast.ident
type expr = Ast.expr
type patt = Ast.patt

type var = string

type caml_type =
  | Type of string * caml_type list
  | Tuple of caml_type list
  | Var of var

let typ id args = Type(id, args)
let v x = Var x
let tuple l = Tuple l
let list_of_tuple = function
  | Tuple l -> l
  | x -> [x]

let string_of_caml_type =
  let at_top x = x
  and not_at_top x = "(" ^ x ^ ")" in
  let rec aux top = function
    | Type(t, []) -> t
    | Type(t, [arg]) -> aux not_at_top arg ^ " " ^ t
    | Type(t, args) ->
        "(" ^ String.concat ", " (List.map (aux at_top) args) ^ ") " ^ t
    | Tuple [] -> "unit"
    | Tuple l ->
        top (String.concat " * " (List.map (aux not_at_top) l))
    | Var(x) -> "'" ^ x
  in
    aux at_top

let rec ctyp_of_caml_type = function
  | Type(t, args) ->
      List.fold_left (fun acc t -> <:ctyp< $ctyp_of_caml_type t$ $acc$ >>) (<:ctyp< $id:ident_of_string t$ >>) args
  | Tuple [] -> (<:ctyp< unit >>)
  | Tuple l -> Ast.TyTup(_loc, Ast.tySta_of_list (List.map ctyp_of_caml_type l))
  | Var x -> (<:ctyp< '$x$ >>)

let ctyp_func_of_caml_types types acc = List.fold_right (fun t acc -> <:ctyp< $ctyp_of_caml_type t$ -> $acc$ >>) types acc

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make
          (Camlp4.OCamlInitSyntax.Make(Ast)(Gram)(Quotation))))

let caml_type_of_string str =
  let fail () = failwith ("can not understand this caml type: " ^ str) in
  let parse_id id =
    String.concat "." (List.map (function
                                | Ast.IdLid(_, t) -> t
                                | Ast.IdUid(_, t) -> t
                                | _ -> fail ()) (Ast.list_of_ident id [])) in
  let rec parse_app acc = function
    | (<:ctyp< $id:t$ >>) -> (List.rev acc, parse_id t)
    | (<:ctyp< $a$ $b$ >>) -> parse_app (parse_type a :: acc) b
    | _ -> fail ()
  and parse_type t = match t with
    | Ast.TyTup(_, t) -> tuple (List.map parse_type (Ast.list_of_ctyp t []))
    | (<:ctyp< '$x$ >>) -> v x
    | (<:ctyp< $_$ -> $_$ >>) -> tuple (parse_fun t)
    | _ -> let args, id = parse_app [] t in if id = "unit" then Tuple [] else typ id args
  and parse_fun = function
    | (<:ctyp< $a$ -> $b$ >>) -> parse_type a :: parse_fun b
    | t -> [parse_type t]
  in
    parse_type (Caml.Gram.parse Caml.ctyp _loc (Stream.of_string str))

let unit = tuple []
let int = typ "int" []
let int32 = typ "int32" []
let int64 = typ "int64" []
let float = typ "float" []
let bool = typ "bool" []
let char = typ "char" []
let string = typ "string" []
let list x = typ "list" [x]
let path = typ "Path.t" []
let obus_value = typ "Values.t" []
let obus_types = list (typ "Types.t" [])

type dbus_basic_type =
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

type dbus_single_type =
    [ dbus_basic_type
    | `array of dbus_single_type
    | `dict of dbus_basic_type * dbus_single_type
    | `structure of dbus_single_type list
    | `variant ]

type dbus_type =
    [ dbus_single_type
    | `seq of dbus_single_type list ]

let dbus_type_of_signature str = `seq (CommonTypes.of_signature str)
let signature_of_dbus_type dt = CommonTypes.to_signature
  (match dt with
     | #dbus_single_type as t -> [t]
     | `seq tl -> tl)

let string_of_eqn (ct, dt) =
  string_of_caml_type ct ^ " = " ^ CommonTypes.to_string (match dt with
                                                            | `seq t -> `structure t
                                                            | #dbus_single_type as t -> t)
