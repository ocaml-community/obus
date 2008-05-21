(*
 * generate.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type ValueType =
sig
  type t
  val flat : t list -> t
end

module Make (Term : TermType) (Value : ValueType) =
struct
  open Types

  type ('a, 'b) args = ('a, value, 'b, value list -> value) Seq.t
  type ('l, 'r) dep = 'l pattern * 'r pattern

  let first = fresh ()
  let tail = fresh ()
  let first2 = (first, first)
  let tail2 = (tail, tail)

  type ('l, 'r) eqn = 'l typ * 'r typ
  type ('l, 'r) maker = (('l, 'r) eqn -> value) -> value

  let rec map f =
    let rec aux = function
      | Type(id, args) -> typ id (List.map aux args)
      | Cons(x, y) -> cons (aux x) (aux y)
      | Nil -> nil
      | Var v -> f v
    in aux

  type ('l, 'r) generator = {
    left : 'l pattern;
    right : 'r pattern;
    deps : ('l pattern * 'r pattern) list;
    maker : ('l pattern * 'r pattern -> ('l, 'r) eqn) -> ('l, 'r) maker
  }

  type ('l, 'r) rule = ('l, 'r) generator list

  let rec insert_tail = function
    | Cons(x, y) -> cons x (insert_tail y)
    | Nil -> tail
    | _ -> assert false

  let default_generators =
    [ { left = cons first nil;
        right = first;
        deps = [first2];
        maker = fun f g ->
          let h x = g (f x) in
            [Value.flat (h first2)] };
      { left = cons first tail;
        right = cons first tail;
        deps = [first2; tail2];
        maker = fun f g ->
          let h x = g (f x) in
            Value.flat (h first2) :: h tail2 };
      { left = nil;
        right = nil;
        deps = [];
        maker = fun _ _ -> [] } ]

  let rule left right args rest maker =
    let rec aux left right args rest maker =
      {
        left = left;
        right = right;
        deps = Seq.to_list args @ rest;
        maker = maker;
      } in
    let normal = [aux left right args rest
                    (fun f g ->
                       let h x = g (f x) in
                         (Seq.apply maker (Seq.map h args)) (List.map h rest))] in
      match left with
        | Cons _ ->
            aux (insert_tail left) (cons right tail)
              (Seq.cons tail2 args) rest
              (fun f g ->
                 let h x = g (f x) in
                   (Seq.apply maker (Seq.map h args)) (List.map h rest)
                   @ h tail2) :: normal
        | _ -> normal

  type 'a sub = (poly * 'a typ) list

  let substitute sub = map (fun v -> List.assoc v sub)

  let match_term t =
    let add var term sub =
      try
        if List.assoc var sub = term
        then sub
        else raise Exit
      with
        | Not_found -> (var, term) :: sub
    in

    let rec aux sub pattern term =
      match (pattern, term) with
        | Var v, t -> add v t sub
        | Type(ta, arga), Type(tb, argb) when ta = tb -> List.fold_left2 aux sub arga argb
        | Cons(xa, ya), Cons(xb, yb) -> aux (aux sub xa xb) ya yb
        | Nil, Nil -> sub
        | _ -> raise Exit
    in
      aux [] t

  type ('l, 'r) equation =
    | Equation of ('l, 'r) eqn
    | Branches of ('l, 'r) system list

  and ('l, 'r) system = System of ('l, 'r) maker * (('l, 'r) eqn * value) list * (('l, 'r) eqn * ('l, 'r) equation) list

  type 'a result =
    | Success of value
    | Failure
    | Update of 'a

  let generate rules =

    let generators = List.flatten (default_generators :: rules) in

    let gen_branches (a, b) =
      let rec aux acc = function
        | generator :: generators -> begin
            try
              let subr = substitute (match_term generator.right b) in
              let subl = substitute (match_term generator.left a) in
                match List.map (fun (a, b) -> (subl a, subr b)) generator.deps with
                  | [] ->
                      let f _ = assert false in
                        Success(generator.maker f f)
                  | eqns -> aux
                      (System(generator.maker (fun (x, y) -> (subl x, subr y)),
                              [], List.map (fun x -> (x, Equation x)) eqns) :: acc) generators
            with
                _ -> aux acc generators
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
        | (id, eqn) :: eqns -> begin match one_step_equation eqn with
            | Success(v) -> aux acc ((id, v) :: mapping) eqns
            | Failure -> Failure
            | Update(eqn) -> aux ((id, eqn) :: acc) mapping eqns
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
