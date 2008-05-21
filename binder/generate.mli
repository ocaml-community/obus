(*
 * generate.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type ValueType =
sig
  type t
  val flat : t list -> t
end

module Make (Value : ValueType) :
sig
  open Types

  type value = Value.t list

  type ('a, 'b) args = ('a, value, 'b, value list -> value) Seq.t
  type ('l, 'r) dep = 'l pattern * 'r pattern

  type ('l, 'r) rule

  val rule : 'l pattern -> 'r pattern -> (('l, 'r) dep, 'a) args -> ('l, 'r) dep list -> 'a -> ('l, 'r) rule

  val generate : ('l, 'r) rule list -> 'l typ -> 'r typ -> value option
end
