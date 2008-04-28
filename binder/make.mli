(*
 * make.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

(** Module used to define rules and generate code for a language *)

module type Expr = sig
  type t
    (** type of language expression *)
  val tuple_maker : t list -> t
    (** [tuple_maker l] is an expression that compute a tuple which
        component are the elements of [l] *)
end

module type Lang = sig
  type t
    (** name for types *)
end

module type Var = sig
  type t
    (** type of variables *)
end

module Make(L : Lang)(R : Lang)(Var : Var)(Expr : Expr) : sig
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
      (** We always use list of expression to handle tuple, because a
          tuple need to be build only when we know how to compute each
          element of it *)

  type 'a rpattern_seq = (rpattern, expr, 'a, expr list -> expr) Seq.t
      (** A sequence of right pattern, describing arguments of an
          expression maker *)

  val add_rule : lpattern -> rpattern -> 'a rpattern_seq -> rpattern list -> 'a -> unit
    (** [add_rule lpat rpat args rest maker] add a rule for producing
        an expression that compute values of types [rpat] from values
        of types [lpat].

        patterns of [args] and [rest] will be substituted by
        expression during the resolution and then [maker] will be
        applied on them to produce a new expression. *)

  val make : ltype list -> rtype list -> expr option
    (** [make ltypes rtypes] try to compute an expression that compute
        values of type [rtypes] from values of types [ltypes] *)
end
