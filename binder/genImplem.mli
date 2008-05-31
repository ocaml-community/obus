(*
 * genImplem.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Generate a module implementation *)

val gen : bool -> GenSerializer.rule list -> Introspect.module_tree -> Camlp4.PreCast.Ast.str_item

