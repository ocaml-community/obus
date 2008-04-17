(*
 * tree.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type 'a t = Node of 'a option * (string * 'a t) list

val empty : 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val add : string -> 'a -> 'a t -> 'a t
