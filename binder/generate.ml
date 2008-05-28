(*
 * generate.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types

type ('a, 'b, 'v) args = ('a, 'v, 'b, 'v list -> 'v) Seq.t
type dependency = typ * typ

type eqn = typ * typ
type 'v maker = (eqn -> 'v) -> 'v

let rec map f =
  let rec aux = function
    | Type(id, args) -> typ id (List.map aux args)
    | Cons(x, y) -> cons (aux x) (aux y)
    | Nil -> nil
    | Var v -> f v
  in aux

type 'v rule = {
  left : typ;
  right : typ;
  deps : (typ * typ) list;
  maker : (typ * typ -> eqn) -> 'v maker
}

let rule left right args rest maker = {
  left = left;
  right = right;
  deps = Seq.to_list args @ rest;
  maker = (fun f g ->
             let h x = g (f x) in
               (Seq.apply maker (Seq.map h args)) (List.map h rest));
}

type sub = (var * typ) list

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

type 'v equation =
  | Equation of eqn
  | Branches of 'v system list

and 'v system = System of 'v maker * (eqn * 'v) list * (eqn * 'v equation) list

let rec print_equation indent = function
  | Equation(a, b) ->
      Printf.printf "%s%s = %s\n"
        indent
        (string_of_type "()" a)
        (string_of_type "()" b)
  | Branches(systems) ->
      List.iter
        (function
           | System(_, solved, non_solved) ->
               Printf.printf "%s/-----\n" indent;
               List.iter
                 (fun ((a, b), _) ->
                    Printf.printf "%s| %s = %s (solved)\n"
                      indent
                      (string_of_type "()" a)
                      (string_of_type "()" b))
                 solved;
               List.iter
                 (fun ((a, b), br) ->
                    Printf.printf "%s| %s = %s\n"
                      indent
                      (string_of_type "()" a)
                      (string_of_type "()" b);
                    print_equation (indent ^ "|-> ") br)
                 non_solved;
               Printf.printf "%s\\-----\n" indent)
        systems

type ('a, 'v) result =
  | Success of 'v
  | Failure
  | Update of 'a

let generate ?(trace=false) rules =

  let trace = match trace with
    | true -> (fun equation ->
                 Printf.printf "==================\n";
                 print_equation "" equation)
    | false -> (fun _ -> ()) in

  let gen_branches (a, b) =
    let rec aux acc = function
      | rule :: rules -> begin
          try
            let subr = substitute (match_term rule.right b) in
            let subl = substitute (match_term rule.left a) in
              match List.map (fun (a, b) -> (subl a, subr b)) rule.deps with
                | [] ->
                    let f _ = assert false in
                      Success(rule.maker f f)
                | eqns -> aux
                    (System(rule.maker (fun (x, y) -> (subl x, subr y)),
                            [], List.map (fun x -> (x, Equation x)) eqns) :: acc) rules
          with
              _ -> aux acc rules
        end
      | [] -> begin match acc with
          | [] -> Failure
          | _ -> Update(acc)
        end
    in
      aux [] rules
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
    trace equation;
    match one_step_equation equation with
      | Success(v) -> Some(v)
      | Failure -> None
      | Update(equation) -> find equation
  in

    fun a b -> find (Equation(a, b))
