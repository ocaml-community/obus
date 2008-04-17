(*
 * interface.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type 'a param = string * 'a

type 'a element =
  | Method of string * 'a param list * 'a param list
  | Signal of string * 'a param list

type 'a t = 'a element list

val from_instrospection : Lexing.lexbuf -> (string * DBus.typ t) list

val map : ('a -> 'b) -> 'a t -> 'b t
