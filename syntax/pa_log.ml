(*
 * pa_debug.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast

let section file_name =
  let s = Filename.chop_extension (Filename.basename file_name) in
  let len = String.length s in
  if len >= 5 && String.sub s 0 5 = "oBus_" then
    String.sub s 5 (len - 5)
  else
    s

let rec insert section op =
  let rec aux = function
    | <:expr@_loc< $x$ $y$ >> -> <:expr< $aux x$ $y$ >>
    | x ->
        let _loc = Ast.loc_of_expr x in
        begin match section with
          | "util" -> <:expr< $lid:op$ None $x$ >>
          | _ -> <:expr< Util.$lid:op$ (Some $str:section$) $x$ >>
        end
  in
    aux

let var _loc v = function
  | "util" -> <:expr< $lid:v$ >>
  | _ -> <:expr< Util.$lid:v$ >>

let map_expr = function
  | <:expr@_loc< LOG($x$) >> ->
      let section = section (Loc.file_name _loc) in
      <:expr< if $var _loc "verbose" section$
                then $insert section "log" x$
                else () >>
  | <:expr@_loc< DEBUG($x$) >> ->
      let section = section (Loc.file_name _loc) in
      <:expr< if $var _loc "debug" section$
                then $insert section "log" x$
                else () >>
  | <:expr@_loc< ERROR($x$) >> ->
      let section = section (Loc.file_name _loc) in
      <:expr< $insert section "error" x$ >>
  | x -> x

let _ =
  AstFilters.register_str_item_filter (Ast.map_expr map_expr)#str_item;
