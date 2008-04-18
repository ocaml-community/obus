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
  type var
  type right
  type left
end

module type ValueType =
sig
  type t
end

module Make (Term : TermType) (Value : ValueType) :
sig
  type lterm = [ `LTerm of Term.left * lterm list ]
  type rterm = [ `RTerm of Term.right * rterm list ]
  type rpattern =
      [ `RTerm of Term.right * rpattern list
      | `Var of Term.var ]
  type pattern =
      [ `LTerm of Term.left * pattern list
      | `RTerm of Term.right * rpattern list
      | `Var of Term.var ]

  type generator

  val make_generator : pattern -> rpattern -> rpattern list -> (Value.t list -> Value.t) -> generator

  val generate : generator list -> lterm -> rterm  -> Value.t option
end
