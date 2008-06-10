(*
 * tree.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of a hierarchy of modules *)

type ('a, 'b) t = Node of 'a option * ('b * ('a, 'b) t) list

val empty : ('a, 'b) t

val insert : 'b list -> 'a -> ('a, 'b) t -> ('a, 'b) t
  (** [insert path x tree] *)

val map : ('b list -> 'a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
val fold : ('b list -> 'a -> 'c -> 'c) -> 'c -> ('a, 'b) t -> 'c
val fold_map : ('b list -> 'a -> 'c -> 'c * 'd) -> 'c -> ('a, 'b) t -> 'c * ('d, 'b) t
val flat : ('a option -> ('b * 'c) list -> 'c) -> ('a, 'b) t -> 'c
