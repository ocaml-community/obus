(*
 * type.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

type none

type section
type var = section * string

type ('t, 'var) term =
  | Type of 't * ('t, 'var) term list
  | Cons of ('t, 'var) term * ('t, 'var) term
  | Nil
  | Var of 'var

type 'a typ = ('a, none) term
type 'a pattern = ('a, var) term

val section : unit -> section

val v : string -> ('a, var) term
val cons : ('a, 'b) term -> ('a, 'b) term -> ('a, 'b) term
val nil : ('a, 'b) term
val tuple : ('a, 'b) term list -> ('a, 'b) term
val list_of_tuple : ('a, 'b) term -> ('a, 'b) term list
