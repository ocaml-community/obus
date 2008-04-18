(*
 * seq.mli
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of list of fixed size *)

type ('elt, 'arg, 'func, 'result) t
  (** A list of element of type ['elt]. The size of the list is encoded in ['func] *)

val nil : ('a, 'b, 'c, 'c) t
  (** [nil] an empty list *)

val cons : 'a -> ('a, 'b, 'c, 'd) t -> ('a, 'b, 'b -> 'c, 'd) t
  (** [cons x l] the list which head is [x] and tail is [l] *)

val apply : 'b -> ('a, 'a, 'b, 'c) t -> 'c
  (** [apply f l] apply the function [f] to the list [l]. [f] must
      take at least the size of [l] arguments *)

val concat : ('a, 'b, 'c, 'd) t -> ('a, 'b, 'd, 'e) t -> ('a, 'b, 'c, 'e) t
  (** [concat a b] concatenate two list *)

val hd : ('a, 'b, 'b -> 'c, 'd) t -> 'a
  (** [hd l] return the head of a non empty list *)

val tl : ('a, 'b, 'b -> 'c, 'd) t -> ('a, 'b, 'c, 'd) t
  (** [tl l] return the tail of a non empty list *)

val map : ('a -> 'b) -> ('a, 'c, 'd, 'e) t -> ('b, 'c, 'd, 'e) t
  (** [map f l] same as List.map *)

val to_list : ('a, 'b, 'c, 'd) t -> 'a list
  (** [to_list seq] Convert a sequence into a native list *)
