(*
 * generate.ml
 * -----------
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

module Make (Term : TermType) (Value : ValueType) =
struct
  open Term
  type 'a term = Term of 'a * 'a term list
  type eqn = left term * right term
  type either_term =
    | ETLeft of Term.left * either_term list
    | ETRight of Term.right term
  type right_pattern =
    | RPVar of Term.var
    | RPTerm of Term.right * right_pattern list
  type either_pattern =
    | EPVar of Term.var
    | EPLeft of Term.left * either_pattern list
    | EPRight of right_pattern
  type maker = Value.t list -> Value.t
  type generator = {
    gen_either : either_pattern;
    gen_right : right_pattern;
    gen_vars : right_pattern list;
    gen_maker : maker;
  }

  let make_generator either_pattern right_pattern vars maker =
    { gen_either = either_pattern;
      gen_right = right_pattern;
      gen_vars = vars;
      gen_maker = maker }

  type sub = (var * right term) list

  let rec substitute_right (sub : sub) : right_pattern -> right term = function
      | RPVar v -> List.assoc v sub
      | RPTerm(name, args) -> Term(name, List.map (substitute_right sub) args)

  let rec substitute_either (sub : sub) : either_pattern -> either_term = function
      | EPVar v -> ETRight(List.assoc v sub)
      | EPLeft(name, args) -> ETLeft(name, List.map (substitute_either sub) args)
      | EPRight(t) -> ETRight(substitute_right sub t)

  exception Cannot_unify

  let unify_right (pattern : right_pattern) (term : right term) =

    let add (var : var) (value : right term) (sub : sub) : sub =
      try
        if List.assoc var sub = value
        then sub
        else raise Cannot_unify
      with
        | Not_found -> (var, value) :: sub
    in

    let rec aux (sub : sub) (pattern : right_pattern) (term : right term)
        : sub =
      match (pattern, term) with
        | RPVar v, t -> add v t sub
        | RPTerm(ta, arga), Term(tb, argb) when ta = tb ->
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

  and system = System of maker * right term list * (right term * Value.t) list * (right term * equation) list

  type 'a result =
    | Success of Value.t
    | Failure
    | Update of 'a

  let unify_left (left : left term) (term : either_term) : (right term * equation) list =
    let rec aux eqns left term =
      match (left, term) with
        | Term(ta, arga), ETLeft(tb, argb) ->
            if ta = tb
            then begin try
              List.fold_left2 aux eqns arga argb
            with
              | Invalid_argument _ -> raise Cannot_unify
            end else raise Cannot_unify
        | x, ETRight(y) -> (y, Equation(x, y)) :: eqns
    in
      aux [] left term

  let generate (generators : generator list) : left term -> right term -> Value.t option =

    let gen_branches ((a, b) : eqn) : system list result =
      let rec aux acc = function
        | generator :: generators -> begin
            try
              let sub = unify_right generator.gen_right b in
              let either = substitute_either sub generator.gen_either in
                match unify_left a either with
                  | [] -> Success(generator.gen_maker [])
                  | eqns -> aux
                      (System(generator.gen_maker,
                              List.map (substitute_right sub) generator.gen_vars,
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

    let rec one_step_system (System(maker, vars, mapping, eqns)) : system result =
      let rec aux acc mapping = function
        | (t, eqn) :: eqns -> begin match one_step_equation eqn with
            | Success(v) -> aux acc ((t, v) :: mapping) eqns
            | Failure -> Failure
            | Update(eqn) -> aux ((t, eqn) :: acc) mapping eqns
          end
        | [] -> begin match acc with
            | [] -> Success(maker (List.map (fun var -> List.assoc var mapping) vars))
            | _ -> Update(System(maker, vars, mapping, acc))
          end
      in
        aux [] mapping eqns

    and one_step_equation : equation -> equation result = function
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

    let rec find (equation : equation) : Value.t option =
      match one_step_equation equation with
        | Success(v) -> Some(v)
        | Failure -> None
        | Update(equation) -> find equation
    in

      fun a b -> find (Equation(a, b))
end
