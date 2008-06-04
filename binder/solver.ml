(*
 * solver.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type ('eqn, 'sol) builder = ('eqn * 'sol) list -> 'sol

type ('eqn, 'sol) result =
  | R_success of 'sol
  | R_deps of 'eqn list * ('eqn, 'sol) builder
  | R_fail
type ('eqn, 'sol) rule = 'eqn -> ('eqn, 'sol) result

let fail = R_fail
let success x = R_success x
let dep deps f =
  R_deps(Seq.to_list deps,
         fun mapping ->
           Seq.apply f (Seq.map (fun x -> List.assoc x mapping) deps))

type ('eqn, 'sol) equation =
  | Equation of 'eqn
  | Branches of ('eqn, 'sol) system list

and ('eqn, 'sol) system = System of ('eqn, 'sol) builder * ('eqn * 'sol) list * ('eqn * ('eqn, 'sol) equation) list

open Printf

let print_equation string_of_eqn =
  let rec aux indent = function
    | Equation(eqn) ->
        printf "%s%s\n" indent (string_of_eqn eqn)
    | Branches(systems) ->
        List.iter
          (function
             | System(_, solved, non_solved) ->
                 printf "%s/-----\n" indent;
                 List.iter
                   (fun (eqn, _) ->
                      printf "%s| %s (solved)\n"
                        indent
                        (string_of_eqn eqn))
                   solved;
                 List.iter
                   (fun (eqn, br) ->
                      printf "%s| %s\n"
                        indent
                        (string_of_eqn eqn);
                      aux (indent ^ "|-> ") br)
                   non_solved;
                 printf "%s\\-----\n" indent)
          systems
  in
    aux ""

type ('a, 'b) one_step_result =
  | Success of 'a
  | Failure
  | Update of 'b

let solve ?printer rules =

  let trace = match printer with
    | Some f -> (fun equation ->
                   printf "==================\n";
                   print_equation f equation)
    | None -> (fun _ -> ()) in

  let gen_branches eqn =
    let rec aux acc = function
      | rule :: rules -> begin
          match rule eqn with
            | R_success v ->
                Success(v)
            | R_deps(deps, builder) ->
                aux (System(builder, [],
                            List.map (fun x -> (x, Equation x)) deps)
                     :: acc) rules
            | R_fail -> aux acc rules
        end
      | [] -> begin match acc with
          | [] -> Failure
          | _ -> Update(acc)
        end
    in
      aux [] rules
  in

  let rec one_step_system (System(builder, mapping, eqns)) =
    let rec aux acc mapping = function
      | (id, eqn) :: eqns -> begin match one_step_equation eqn with
          | Success(v) -> aux acc ((id, v) :: mapping) eqns
          | Failure -> Failure
          | Update(eqn) -> aux ((id, eqn) :: acc) mapping eqns
        end
      | [] -> begin match acc with
          | [] -> Success(builder mapping)
          | _ -> Update(System(builder, mapping, acc))
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

    fun eqn -> find (Equation eqn)
