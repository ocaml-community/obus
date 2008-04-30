(*
 * type.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

type mono
type poly =
  | Intern of int
  | UserVar of string

type ('t, 'var) term =
  | Type of 't * ('t, 'var) term list
  | Cons of ('t, 'var) term * ('t, 'var) term
  | Nil
  | Var of 'var

type 'a typ = ('a, mono) term
type 'a pattern = ('a, poly) term

let fresh =
  let c = ref 0 in
    fun () ->
      incr c; Var(Intern !c)

let typ id args = Type(id, args)
let v x = Var(UserVar x)
let cons x y = match y with
  | Cons _
  | Var _
  | Nil -> Cons(x, y)
  | _ -> raise (Invalid_argument "only Cons, Nil and variables are allowed in the right side of a cons")
let nil = Nil
let tuple l = List.fold_right (fun x acc -> Cons(x, acc)) l Nil
let rec list_of_tuple = function
  | Cons(x, y) -> x :: list_of_tuple y
  | Nil -> []
  | _ -> assert false

