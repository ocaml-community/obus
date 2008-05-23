(*
 * helpers.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types

val ident_of_string : string -> ident
val idexpr_of_string : string -> expr
val idpatt_of_string : string -> patt

val expr_of_id : ident -> expr
val patt_of_id : ident -> patt
val expr_of_str : string -> expr
val patt_of_str : string -> patt
val expr_of_int : int -> expr
val patt_of_int : int -> patt
val expr_of_chr : char -> expr
val patt_of_chr : char -> patt

val expr_record : (ident * expr) list -> expr
val patt_record : (ident * patt) list -> patt

val bind : ident -> expr -> expr -> expr
val bind_patt : patt -> expr -> expr -> expr
val seq : expr list -> expr
val app : expr -> expr -> expr
