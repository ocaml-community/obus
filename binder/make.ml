(*
 * make.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)


module type Expr = sig type t val tuple_maker : t list -> t end
module type Lang = sig type t end
module type Var = sig type t end

module Make(L : Lang)(R : Lang)(Var : Var)(Expr : Expr) =
struct
  type none
  type ('left, 'right, 'var) term =
    | LType of 'left * ('left, 'right, 'var) term list
    | RType of 'right * (none, 'right, 'var) term list
    | LTuple of ('left, 'right, 'var) term list
    | RTuple of (none, 'right, 'var) term list
    | Var of 'var
  type ltype = (L.t, none, none) term
  type rtype = (none, R.t, none) term
  type lpattern = (L.t, R.t, Var.t) term
  type rpattern = (none, R.t, Var.t) term
  type expr = Expr.t list
  type 'a rpattern_seq = (rpattern, expr, 'a, expr list -> expr) Seq.t

  type 'a id =
    | Id of 'a
    | Cons
    | Nil
  type make_var =
    | UserVar of Var.t
    | TupleFirst
    | TupleNext

  module Gen = Generate.Make
    (struct
       type var = make_var
       type left = L.t id
       type right = R.t id
     end)
    (struct
       type t = expr
     end)

  let lterm id args = `LTerm(id, args)
  let rterm id args = `RTerm(id, args)
  let tuple make next args =  List.fold_right (fun x acc -> make Cons [x; acc]) args next
  let ltuple next args = tuple lterm next args
  let rtuple next args = tuple rterm next args
  let var v = `Var (UserVar v)
  let lnil = lterm Nil []
  let rnil = rterm Nil []

  let rec make_rterm : rtype -> Gen.rterm = function
    | LType _ -> assert false
    | RType(id, args) -> rterm (Id id) (List.map make_rterm args)
    | LTuple _ -> assert false
    | RTuple(args) -> rtuple rnil (List.map make_rterm args)
    | Var _ -> assert false

  let rec make_lterm : ltype -> Gen.lterm = function
    | LType(id, args) -> lterm (Id id) (List.map make_lterm args)
    | RType _ -> assert false
    | LTuple(args) -> ltuple lnil (List.map make_lterm args)
    | RTuple _ -> assert false
    | Var _ -> assert false

  let rec make_rpattern : rpattern -> Gen.rpattern = function
    | LType _ -> assert false
    | RType(id, args) -> rterm (Id id) (List.map make_rpattern args)
    | LTuple _ -> assert false
    | RTuple(args) -> rtuple rnil (List.map make_rpattern args)
    | Var v -> var v

  let rec make_lpattern : lpattern -> Gen.lpattern = function
    | LType(id, args) -> lterm (Id id) (List.map make_lpattern args)
    | RType(id, args) -> rterm (Id id) (List.map make_rpattern args)
    | LTuple(args) -> ltuple lnil (List.map make_lpattern args)
    | RTuple(args) -> (rtuple rnil (List.map make_rpattern args) : Gen.rpattern :> Gen.lpattern)
    | Var v -> `Var (UserVar v)

  let rules = ref
    [Gen.make_generator
       (lterm Cons [`Var TupleFirst; `Var TupleNext])
       (rterm Cons [`Var TupleFirst; `Var TupleNext])
       [< (`Var TupleFirst); (`Var TupleNext) >]
       []
       (fun args _ -> Seq.apply (fun first next _ -> Expr.tuple_maker first :: next) args []);
     Gen.make_generator
       (lterm Nil [])
       (rterm Nil [])
       [<>]
       []
       (fun args _ -> [])]

  let rec tuplify = function
    | LType(id, args) -> LTuple [LType(id, List.map tuplify args)]
    | RType(id, args) -> RTuple [RType(id, List.map tuplify args)]
    | LTuple(args) -> LTuple(List.map tuplify args)
    | RTuple(args) -> RTuple(List.map tuplify args)
    | Var v -> RTuple [Var v]

  let seq_args args = Seq.map make_rpattern args
  let lst_args args = List.map make_rpattern args
  let any_next = `Var TupleNext

  let add_rule lpattern rpattern args rest maker =
    rules := begin match lpattern with
      | LTuple pats ->
          Gen.make_generator
            (ltuple any_next (List.map make_lpattern pats))
            (rtuple any_next [make_rpattern rpattern])
            (Seq.cons any_next (seq_args args))
            (lst_args rest)
            (fun args rest -> (Seq.hd args) @ (Seq.apply maker (Seq.tl args) rest))
      | _ ->
          Gen.make_generator
            (make_lpattern lpattern)
            (make_rpattern rpattern)
            (seq_args args)
            (lst_args rest)
            (fun args rest -> Seq.apply maker args rest)
    end :: !rules

  let make ltype rtype = Gen.generate !rules (make_lterm (LTuple ltype)) (make_rterm (RTuple rtype))
end
