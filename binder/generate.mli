(*
 * generate.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

module type TermType =
sig
  type var
  type t
end

module type ValueType =
sig
  type t
end

module Make (Term : TermType) (Value : ValueType) :
sig
  type term =
      [ `Term of Term.t * term list ]

  type pattern =
      [ `Term of Term.t * pattern list
      | `Var of Term.var ]

  type generator

  val make_generator : pattern -> pattern -> pattern list -> (Value.t list -> Value.t) -> generator

  val generate : generator list -> term -> term  -> Value.t option
end
