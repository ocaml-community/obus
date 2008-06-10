(*
 * helpers.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast

let _loc = Loc.ghost

let ident_of_string name =
  Ast.idAcc_of_list
    (List.map
       (fun id ->
          if id <> "" && Char.uppercase id.[0] <> id.[0]
          then <:ident< $lid:id$ >>
          else <:ident< $uid:id$ >>)
       (StrUtil.split_dot name))

let expr_of_id id = (<:expr< $id:id$ >>)
let patt_of_id id = (<:patt< $id:id$ >>)
let expr_of_str x = (<:expr< $str:String.escaped x$ >>)
let patt_of_str x = (<:patt< $str:String.escaped x$ >>)
let expr_of_int x = (<:expr< $int:string_of_int x$ >>)
let patt_of_int x = (<:patt< $int:string_of_int x$ >>)
let expr_of_chr x = Ast.ExChr(_loc, String.escaped (String.make 1 x))
let patt_of_chr x = Ast.PaChr(_loc, String.escaped (String.make 1 x))

let idexpr_of_string name =
  if name <> "" && name.[0] = '`'
  then Ast.ExVrn(_loc, String.sub name 1 (String.length name - 1))
  else (expr_of_id (ident_of_string name))

let idpatt_of_string name =
  if name <> "" && name.[0] = '`'
  then Ast.PaVrn(_loc, String.sub name 1 (String.length name - 1))
  else (patt_of_id (ident_of_string name))

let bind_patt patt value expr = match patt, value with
  | (<:patt< $id:a$ >>, <:expr< $id:b$ >>) when a = b -> expr
  | _ -> (<:expr< let $patt$ = $value$ in $expr$ >>)
let bind id = bind_patt (patt_of_id id)
let seq exprs = Ast.exSem_of_list exprs
let app a b = match a with
  | (<:expr< fun $x$ -> $e$ >>) -> (<:expr< let $x$ = $b$ in $e$ >>)
  | Ast.ExFun(_, mc) -> (<:expr< match $b$ with $mc$ >>)
  | _ -> (<:expr< $a$ $b$ >>)

let expr_record desc =
  Ast.ExRec(_loc,
            Ast.rbSem_of_list
              (List.map
                 (fun (id, expr) ->
                    <:rec_binding< $id$ = $expr$ >>)
                 desc),
            Ast.ExNil _loc)

let patt_record desc =
  Ast.PaRec(_loc,
            Ast.paSem_of_list
              (List.map
                 (fun (id, expr) ->
                    <:patt< $id$ = $expr$ >>)
                 desc))

let appn expr args = List.fold_left app expr args

let func args expr =
  List.fold_right
    (fun id acc -> <:expr< fun $id:id$ -> $acc$ >>)
    args expr
