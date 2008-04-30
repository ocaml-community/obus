(*
 * type.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

type none

type ('t, 'var) term =
  | Type of 't * ('t, 'var) term list
  | Cons of ('t, 'var) term * ('t, 'var) term
  | Nil
  | Var of 'var

type section = int
type var = section * string

type 'a typ = ('a, none) term
type 'a pattern = ('a, var) term

let section =
  let c = ref 0 in
    fun () -> incr c; !c

let global = section ()

let v x = Var(global, x)
let cons x y = Cons(x, y)
let nil = Nil
let tuple l = List.fold_right (fun x acc -> Cons(x, acc)) l Nil
let rec list_of_tuple = function
  | Cons(x, y) -> x :: list_of_tuple y
  | Nil -> []
  | _ -> raise (Invalid_argument "malformed list")

