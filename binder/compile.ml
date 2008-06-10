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

type env = (expr * string) list

let empty_env = []
let dump_env l = List.rev (List.map (fun (a, b) -> (b, a)) l)

let lookup expr env =
  match Util.assoc expr env with
    | Some id -> (id, env)
    | None -> let id = "f" ^ string_of_int (List.length env) in
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

let var_id n =
  if n >= 0
  then (<:ident< $lid:"v" ^ string_of_int n$ >>)
  else (<:ident< $lid:"v_" ^ string_of_int (-n)$ >>)

let var_ids from count =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (var_id (n + from - 1) :: acc) (n - 1)
  in
    aux [] count

let var_patt n = patt_of_id (var_id n)
let var_patts n count = List.map patt_of_id (var_ids n count)
let var_expr n = expr_of_id (var_id n)
let var_exprs n count = List.map expr_of_id (var_ids n count)

let compile_reader instrs return env =
  let rec aux instrs env n ret cont = match instrs with
    | [] -> cont (ret n, env)
    | instr :: instrs ->
        let n, f = match instr with
          | Istructure ->
              (n,
               fun (expr, env) ->
                 (<:expr<
                    let i = rpad8 i in
                      $expr$
                      >>,
                  env))
          | Iarray(mkexpr, etyp) ->
              (n + 1,
               fun (expr, env) ->
                 let array_reader_expr, env = mkexpr env in
                 let id, env = lookup array_reader_expr env in
                 let array_func = match padding_of_type etyp with
                   | 8 -> "read_array8"
                   | _ -> "read_array"
                 in
                   (<:expr<
                      let i, $var_patt n$ = $lid:array_func$ $lid:id$ buffer i in
                        $expr$
                        >>,
                    env))
          | Iaction func ->
              (n + 1,
               fun (expr, env) ->
                 (<:expr<
                    let i, $var_patt n$ = $lid:"read_" ^ func$ buffer i in
                      $expr$
                      >>,
                  env))
          | Iconvert func ->
              (n,
               fun (expr, env) ->
                 (<:expr<
                    let $var_patt (n - 1)$ = $func$ $var_expr (n - 1)$ in
                      $expr$
                      >>,
                  env))
          | Ipack(unpacker, packer, instrs) ->
              (n + 1,
               fun (expr, env) ->
                 aux instrs env n
                   (fun m ->
                      <:expr<
                        let $var_patt n$ = $packer (var_exprs n (m - n))$ in
                          $expr$
                          >>)
                   (fun x -> x))
        in
          aux instrs env n ret (fun x -> cont (f x))
  in
    aux instrs env 0 (fun n -> return (var_exprs 0 n)) (fun x -> x)

let compile_writer instrs return env =
  let rec aux instrs env start ret = match instrs with
    | [] -> (env, start, ret)
    | instr :: instrs ->
        let env, n, expr = aux instrs env start ret in
          match instr with
            | Istructure ->
                (env, n,
                 <:expr<
                   let i = wpad8 buffer i in
                     $expr$
                     >>)
            | Iarray(mkexpr, etyp) ->
                let array_writer_expr, env = mkexpr env in
                let id, env = lookup array_writer_expr env in
                let array_func = match padding_of_type etyp with
                  | 8 -> "write_array8"
                  | _ -> "write_array"
                in
                  (env, n + 1,
                   <:expr<
                     let i = $lid:array_func$ $lid:id$ buffer i $var_expr n$ in
                       $expr$
                       >>)
            | Iaction func ->
                (env, n + 1,
                 <:expr<
                   let i = $lid:"write_" ^ func$ buffer i $var_expr n$ in
                     $expr$
                     >>)
            | Iconvert func ->
                (env, n,
                 <:expr<
                   let $var_patt (n - 1)$ = $func$ $var_expr (n - 1)$ in
                     $expr$
                     >>)
            | Ipack(unpacker, packer, instrs) ->
                (env, n + 1,
                 let env, m, expr = aux instrs env n expr in
                   <:expr<
                     let $unpacker (List.rev (var_patts n (m - n)))$ = $var_expr n$ in
                       $expr$
                       >>)
  in
  let env, n, expr = aux instrs env 0 return  in
    (List.rev (var_ids 0 n), expr, env)
