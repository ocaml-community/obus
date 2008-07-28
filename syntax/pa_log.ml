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
  match Filename.chop_extension (Filename.basename file_name) with
    | "auth" -> "authentification"
    | s -> s

let make_format loc section prefix fmt =
  let i = try String.index section '_' + 1 with _ -> 0 in
    Ast.ExStr(loc, "obus(" ^ String.sub section i (String.length section - i) ^ "):" ^ prefix ^ " " ^ fmt ^ "\\n%!")

let rec insert section prefix =
  let rec aux = function
    | <:expr@_loc< $x$ $y$ >> -> <:expr< $aux x$ $y$ >>
    | <:expr@_loc< $str:x$ >> -> <:expr< Printf.eprintf $make_format _loc section prefix x$ >>
    | _ -> assert false
  in
    aux

let map_expr = function
  | <:expr@_loc< LOG($x$) >> ->
    let section = section (Loc.file_name _loc) in
      <:expr< if OBus_info.verbose
      then $insert section "" x$
      else () >>
  | <:expr@_loc< DEBUG($x$) >> ->
    let section = section (Loc.file_name _loc) in
      <:expr< if OBus_info.debug
      then $insert section "" x$
      else () >>
  | <:expr@_loc< ERROR($x$) >> ->
    let section = section (Loc.file_name _loc) in
      <:expr< $insert section " error:" x$ >>
  | x -> x

let _ =
  AstFilters.register_str_item_filter (Ast.map_expr map_expr)#str_item;
