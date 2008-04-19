(*
 * generate.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
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

module Make (Term : TermType) (Value : ValueType) =
struct
  open Term

  type lterm = [ `LTerm of Term.left * lterm list ]
  type rterm = [ `RTerm of Term.right * rterm list ]
  type term =
      [ `LTerm of Term.left * term list
      | `RTerm of Term.right * rterm list ]
  type rpattern =
      [ `RTerm of Term.right * rpattern list
      | `Var of Term.var ]
  type pattern =
      [ `LTerm of Term.left * pattern list
      | `RTerm of Term.right * rpattern list
      | `Var of Term.var ]

  type eqn = lterm * rterm

  type ('a, 'b) value_maker =
    | Seq of ('a, Value.t, 'b, Value.t) Seq.t * 'b
    | List of 'a list * (Value.t list -> Value.t)

  class maker maker = object
    val maker : (rterm, 'a) value_maker = maker
    method make mapping = match maker with
      | Seq(vars, f) -> Seq.apply f (Seq.map (fun var -> List.assoc var mapping) vars)
      | List(vars, f) -> f (List.map (fun var -> List.assoc var mapping) vars)
  end

  class generator left right maker = object
    method left : pattern = left
    method right : rpattern = right
    val maker : (rpattern, 'a) value_maker = maker
    method maker f = new maker (match maker with
                                  | Seq(vars, g) -> Seq(Seq.map f vars, g)
                                  | List(vars, g) -> List(List.map f vars, g))
  end

  let make_generator left right maker = new generator left right maker

  type sub = (var * rterm) list

  let rec substitute_right (sub : sub) : rpattern -> rterm = function
    | `Var v -> (List.assoc v sub)
    | `RTerm(name, args) -> `RTerm(name, List.map (substitute_right sub) args)

  let rec substitute (sub : sub) : pattern -> term = function
    | `Var v -> ((List.assoc v sub) : rterm :> term)
    | `LTerm(name, args) -> `LTerm(name, List.map (substitute sub) args)
    | `RTerm(name, args) -> `RTerm(name, List.map (substitute_right sub) args)

  exception Cannot_unify

  let unify pattern term =

    let add var term sub =
      try
        if List.assoc var sub = term
        then sub
        else raise Cannot_unify
      with
        | Not_found -> (var, term) :: sub
    in

    let rec aux sub pattern term =
      match (pattern, term) with
        | `Var v, t -> add v t sub
        | `RTerm(ta, arga), `RTerm(tb, argb) when ta = tb ->
            begin try
              List.fold_left2 aux sub arga argb
            with
              | Invalid_argument _ -> raise Cannot_unify
            end
        | _ -> raise Cannot_unify
    in
      aux [] pattern term

  exception Cannot_generate

  type equation =
    | Equation of eqn
    | Branches of system list

  and system = System of maker * (rterm * Value.t) list * (rterm * equation) list

  type 'a result =
    | Success of Value.t
    | Failure
    | Update of 'a

  let match_term (a : lterm) (b : term) : (rterm * equation) list =
    let rec aux (eqns : (rterm * equation) list) (a : lterm) (b : term) : (rterm * equation) list =
      match (a, b) with
        | `LTerm(ta, arga), `LTerm(tb, argb) when ta = tb -> begin try
            List.fold_left2 aux eqns arga argb
          with
            | Invalid_argument _ -> raise Cannot_unify
          end
        | `LTerm _, `LTerm _ -> raise Cannot_unify
        | a, (#rterm as b) -> (b, Equation(a, b)) :: eqns
    in
      aux [] a b

  let generate generators =

    let gen_branches (a, b) =
      let rec aux acc = function
        | generator :: generators -> begin
            try
              let sub = unify generator#right b in
              let left = substitute sub generator#left in
                match match_term a left with
                  | [] -> Success((generator#maker (substitute_right []))#make [])
                  | eqns -> aux
                      (System(generator#maker (substitute_right sub),
                              [], eqns) :: acc) generators
            with
              | Cannot_unify -> aux acc generators
          end
        | [] -> begin match acc with
            | [] -> Failure
            | _ -> Update(acc)
          end
      in
        aux [] generators
    in

    let rec one_step_system (System(maker, mapping, eqns)) =
      let rec aux acc mapping = function
        | (t, eqn) :: eqns -> begin match one_step_equation eqn with
            | Success(v) -> aux acc ((t, v) :: mapping) eqns
            | Failure -> Failure
            | Update(eqn) -> aux ((t, eqn) :: acc) mapping eqns
          end
        | [] -> begin match acc with
            | [] -> Success(maker#make mapping)
            | _ -> Update(System(maker, mapping, acc))
          end
      in
        aux [] mapping eqns

    and one_step_equation = function
      | Equation(eqn) -> begin match gen_branches eqn with
          | Success(v) -> Success(v)
          | Failure -> Failure
          | Update(branches) -> Update(Branches(branches))
        end
      | Branches(branches) ->
          let rec aux acc = function
            | system :: branches -> begin match one_step_system system with
                | Success(v) -> Success(v)
                | Failure -> aux acc branches
                | Update(system) -> aux (system :: acc) branches
              end
            | [] -> begin match acc with
                | [] -> Failure
                | _ -> Update(Branches(acc))
              end
          in
           aux [] branches
    in

    let rec find equation =
      match one_step_equation equation with
        | Success(v) -> Some(v)
        | Failure -> None
        | Update(equation) -> find equation
    in

      fun a b -> find (Equation(a, b))
end
