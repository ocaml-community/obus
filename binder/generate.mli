(*
 * generate.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type TermType =
sig
  type right
  type left
end

module type ValueType =
sig
  type t
  val flat : t list -> t
end

module Make (Term : TermType) (Value : ValueType) :
sig
  open Term
  open Type

  type ltype = left typ
  type rtype = right typ
  type lpattern = left pattern
  type rpattern = right pattern

  type value = Value.t list

  type ('a, 'b) args = ('a, value, 'b, value list -> value) Seq.t
  type dep = lpattern * rpattern

  val add_rule : lpattern -> rpattern -> (dep, 'a) args -> dep list -> 'a -> unit

  val generate : ltype -> rtype  -> value option
end
