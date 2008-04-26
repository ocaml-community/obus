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

let add_debug_expr e =
  let _loc = Ast.loc_of_expr e in
  let msg = "tracer: " ^ (Loc.to_string _loc) ^ "@." in
    (<:expr< prerr_endline $str:msg$; $e$ >>)

let rec map_match_case = function
  | (<:match_case@_loc< $m1$ | $m2$ >>) ->
      (<:match_case< $map_match_case m1$ | $map_match_case m2$ >>)
  | (<:match_case@_loc< $p$ when $w$ -> $e$ >>) ->
      (<:match_case< $p$ when $w$ -> $add_debug_expr e$ >>)
  | m -> m

and map_expr =
      function
      | Ast.ExFun (_loc, m) -> Ast.ExFun (_loc, map_match_case m)
      | x -> x

let _ =
  AstFilters.register_str_item_filter (Ast.map_expr map_expr)#str_item
