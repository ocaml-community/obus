(*
 * util.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

val assoc : 'a -> ('a * 'b) list -> 'b option
val find_map : ('a -> 'b option) -> 'a list -> 'b option
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val try_all : ('a -> 'b option) list -> 'a -> 'b option
val select : 'a -> ('a -> 'b option) list -> 'b list
