(*
 * compile.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Types
open Helpers
open Instruction

let _loc = Loc.ghost

type env = (expr * ident) list

let empty_env = []
let dump_env l = List.rev (List.map (fun (a, b) -> (b, a)) l)

let lookup expr env =
  match Util.assoc expr env with
    | Some id -> (id, env)
    | None -> let id = (<:ident< $lid:"__intern" ^ string_of_int (List.length env)$ >>) in
        (id, (expr, id) :: env)

let padding_of_type = function
  | Tbyte -> 1
  | Tboolean -> 4
  | Tint16 -> 2
  | Tint32 -> 4
  | Tint64 -> 8
  | Tuint16 -> 2
  | Tuint32 -> 4
  | Tuint64 -> 8
  | Tdouble -> 8
  | Tstring -> 4
  | Tsignature -> 1
  | Tobject_path -> 4
  | Tarray _ -> 4
  | Tdict _ -> 4
  | Tstructure _ -> 8
  | Tvariant -> 1

let var_id n = (<:ident< $lid:"v" ^ string_of_int n$ >>)
let var_ids n count = List.map var_id (Util.gen_list (fun x -> x) n count)

let var_patt n = patt_of_id (var_id n)
let var_patts n count = List.map patt_of_id (var_ids n count)
let var_expr n = expr_of_id (var_id n)
let var_exprs n count = List.map expr_of_id (var_ids n count)

let compile_reader instrs return env =
  let rec aux instrs env n ret cont = match instrs with
    | [] -> let expr, env = ret env n in (cont expr, env)
    | instr :: instrs ->
        let next env count f =
          aux instrs env (n + count) ret
            (fun expr -> cont (f expr)) in
          match instr with
            | Istructure ->
                next env 0
                  (fun expr ->
                     <:expr<
                       let i = rpad8 i in
                         $expr$
                         >>)
            | Iarray(mkexpr, etyp) ->
                let expr, env = mkexpr env in
                let id, env = lookup expr env in
                let array_func = match padding_of_type etyp with
                  | 8 -> "read_array8"
                  | _ -> "read_array"
                in
                  next env 1
                    (fun expr ->
                       <:expr<
                         let i, $var_patt n$ = $lid:array_func$ $id:id$ buffer i in
                           $expr$
                           >>)
            | Iaction func ->
                next env 1
                  (fun expr ->
                     <:expr<
                       let i, $var_patt n$ = $lid:"read_" ^ func$ buffer i in
                         $expr$
                         >>)
            | Iconvert func ->
                next env 0
                  (fun expr ->
                     <:expr<
                       let $var_patt (n - 1)$ = $func$ $var_expr (n - 1)$ in
                         $expr$
                         >>)
            | Ipack(unpacker, packer, instrs) ->
                aux instrs env n
                  (fun env m ->
                     next env 1
                       (fun expr ->
                          <:expr<
                            let $var_patt n$ = $packer (var_exprs n (m - n))$ in
                              $expr$
                              >>))
                  (fun x -> x)
  in
    aux instrs env 0 (fun env n -> (return (var_exprs 0 n), env)) (fun x -> x)

let compile_writer instrs return env =
  let rec aux instrs env n ret cont = match instrs with
    | [] -> let patts, expr, env = ret env n in (patts, cont expr n, env)
    | instr :: instrs ->
        let next env count f =
          aux instrs env (n + count) ret
            (fun expr n -> cont (f expr) n) in
          match instr with
            | Istructure ->
                next env 0
                  (fun expr ->
                     <:expr<
                       let i = wpad8 buffer i in
                         $expr$
                         >>)
            | Iarray(mkexpr, etyp) ->
                let expr, env = mkexpr env in
                let id, env = lookup expr env in
                let array_func = match padding_of_type etyp with
                  | 8 -> "write_array8"
                  | _ -> "write_array"
                in
                  next env 1
                    (fun expr ->
                       <:expr<
                         let i = $lid:array_func$ $id:id$ buffer i $var_expr n$ in
                           $expr$
                           >>)
            | Iaction func ->
                next env 1
                  (fun expr ->
                     <:expr<
                       let i = $lid:"write_" ^ func$ buffer i $var_expr n$ in
                         $expr$
                         >>)
            | Iconvert func ->
                next env 0
                  (fun expr ->
                     <:expr<
                       let $var_patt n$ = $func$ $var_expr n$ in
                         $expr$
                         >>)
            | Ipack(unpacker, packer, instrs) ->
                aux instrs env n
                  (fun env _ -> next env 1 (fun x -> x))
                  (fun expr m ->
                     <:expr<
                       let $unpacker (var_patts n (m - n))$ = $var_expr n$ in
                         $expr$
                         >>)
  in
    aux instrs env 0 (fun env n -> (var_patts 0 n, return, env)) (fun x y -> x)
