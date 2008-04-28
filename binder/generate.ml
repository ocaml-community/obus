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
  type lpattern =
      [ `LTerm of Term.left * lpattern list
      | `RTerm of Term.right * rpattern list
      | `Var of Term.var ]

  type eqn = lterm * rterm

  type ('a, 'b) args = ('a, Value.t, 'b, Value.t list -> Value.t) Seq.t
  type maker = (rterm -> Value.t) -> Value.t

  type generator = {
    left : lpattern;
    right : rpattern;
    maker : (rpattern -> rterm) -> maker
  }

  let make_generator left right args rest maker  = {
    left = left;
    right = right;
    maker = fun f g ->
      let h x = g (f x) in
        maker (Seq.map h args) (List.map h rest)
  }

  type sub = (var * rterm) list

  let rec substitute_right (sub : sub) : rpattern -> rterm = function
    | `Var v -> (List.assoc v sub)
    | `RTerm(name, args) -> `RTerm(name, List.map (substitute_right sub) args)

  let rec substitute (sub : sub) : lpattern -> term = function
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
              let sub = unify generator.right b in
              let left = substitute sub generator.left in
                match match_term a left with
                  | [] -> Success(generator.maker (substitute_right []) (fun _ -> assert false))
                  | eqns -> aux
                      (System(generator.maker (substitute_right sub),
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
            | [] -> Success(maker (fun x -> List.assoc x mapping))
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
