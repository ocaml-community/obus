(*
 * pa_gen_code.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Add common expressions for modules which generate caml code *)

open Camlp4.PreCast

let _loc = Loc.ghost

let _ =
  AstFilters.register_str_item_filter
    (fun str_item ->
       let _loc = Ast.loc_of_str_item str_item in
         (<:str_item<
          open Camlp4.PreCast;;
          let _loc = Loc.ghost;;
          $str_item$
          >>))
