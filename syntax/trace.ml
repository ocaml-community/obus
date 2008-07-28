(*
 * trace.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Inspired of Camlp4Tracer.ml *)

open Camlp4.PreCast

let add_debug_expr name e =
  let _loc = Ast.loc_of_expr e in
  let msg = name ^ ": " ^ (Loc.to_string _loc) ^ "@." in
    (<:expr< prerr_endline $str:msg$; $e$ >>)

let rec map_match_case name = function
  | (<:match_case@_loc< $m1$ | $m2$ >>) ->
      (<:match_case< $map_match_case name m1$ | $map_match_case name m2$ >>)
  | (<:match_case@_loc< $p$ when $w$ -> $e$ >>) ->
      (<:match_case< $p$ when $w$ -> $add_debug_expr name e$ >>)
  | m -> m

and map_expr name = function
  | Ast.ExFun(_loc, m) -> Ast.ExFun (_loc, map_match_case name m)
  | x -> x

let rec map_str_item = function
  | Ast.StVal(_loc, Ast.BFalse,
              (Ast.BiEq (_, (Ast.PaId (_, (Ast.IdLid (_, f)))),
                         ((Ast.ExFun _ as e))))) ->
      (<:str_item< let $lid:f$ = $map_expr f e$ >>)
  | x -> x

let _ =
  AstFilters.register_str_item_filter (Ast.map_str_item map_str_item)#str_item
