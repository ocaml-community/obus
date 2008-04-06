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
  type left
  type right
end

module type ValueType =
sig
  type t
end

module Make (Term : TermType) (Value : ValueType) :
sig
  type 'a term = Term of 'a * 'a term list

  type right_pattern =
    | RPVar of Term.var
    | RPTerm of Term.right * right_pattern list

  type either_pattern =
    | EPVar of Term.var
    | EPLeft of Term.left * either_pattern list
    | EPRight of right_pattern

  type generator

  val make_generator : either_pattern -> right_pattern -> right_pattern list -> (Value.t list -> Value.t) -> generator

  val generate : generator list -> Term.left term -> Term.right term  -> Value.t option
end
