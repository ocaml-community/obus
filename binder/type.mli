(*
 * type.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

type mono
  (** mono is an empty type *)
type poly

type (+'t, +'var) term =
    private
  | Type of 't * ('t, 'var) term list
  | Cons of ('t, 'var) term * ('t, 'var) term
  | Nil
  | Var of 'var

type 'a typ = ('a, mono) term
type 'a pattern = ('a, poly) term

val fresh : unit -> ('a, poly) term
  (** [fresh ()] create a fresh new variable *)

val v : string -> ('a, poly) term
  (** [v x] create a variable *)

val typ : 'a -> ('a, 'b) term list -> ('a, 'b) term
  (** [typ id args] create a new type *)

val cons : ('a, 'b) term -> ('a, 'b) term -> ('a, 'b) term
  (** [cons x y] create a cons containing [x] and [y]. [y] must be a
      variable of [nil] *)
val nil : ('a, 'b) term

val tuple : ('a, 'b) term list -> ('a, 'b) term
  (** [tuple l] create a tuple from a list of types *)

val list_of_tuple : ('a, 'b) term -> ('a, 'b) term list
  (** [list_of_tuple tup] return the list of types contained in
      [tup]. *)
