(*
 * genCode.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types
open AbstractCode
open CodeConstants
open Helpers

let rec generate_reader for_array env instructions return_expr =
  let check_size expr next = match for_array with
    | true ->
        (<:expr<
           if i + $expr$ > limit
           then raise ($inconsistent_exn$ "invalid array size")
           else $next$
         >>)
    | false ->
        (<:expr<
           if i + $expr$ > String.length buffer
           then raise ($inconsistent_exn$ "invalid message size")
           else $next$
         >>)
  in
  let rec aux env = function
    | [] ->
        return_expr env
    | Update_env f :: instrs ->
        aux (f env) instrs
    | instr :: instrs ->
        let acc = aux env instrs in
          match instr with
            | Align n ->
                (<:expr<
                   let i = i + (($expr_of_int n$ - i) land $expr_of_int (n - 1)$) in
                     $acc$
                     >>)
            | Check_size_fixed n -> check_size <:expr< $expr_of_int n$ >> acc
            | Check_size_dynamic n -> check_size <:expr< len + $expr_of_int n$ >> acc
            | Check_array_size(gap, base_size) ->
                (<:expr<
                   if (len + $expr_of_int gap$) mod $expr_of_int base_size$ <> 0
                   then raise ($inconsistent_exn$ "invalid array size");
                 $acc$ >>)
            | Advance_fixed(x, _) ->
                (<:expr<
                   let i = i + $expr_of_int x$ in
                     $acc$
                     >>)
            | Advance_dynamic(x) ->
                (<:expr<
                   let i = i + len + $expr_of_int x$ in
                     $acc$
                     >>)
            | Expr(_, f) -> f env acc
            | Branches(expr, brs) ->
                (<:expr<
                   let i, $id:Env.last env$ = match $expr env$ with
                       $ Ast.mcOr_of_list
                         (List.map
                            (fun (patt, instrs, ret) ->
                               <:match_case< $patt env$ -> $generate_reader for_array env instrs (fun env -> <:expr< i, $ret env$ >>) $ >>)
                            brs) $
                   in
                     $acc$
                     >>)
            | _ -> acc
  in
    aux env instructions

let rec generate_writer for_array env instructions return_expr =
  let check_size expr next =
    (<:expr<
       let buffer =
         if i + $expr$ > String.length buffer
         then $realloc_buffer$
         else buffer
       in
         $next$
         >>)
  in
  let rec aux env = function
    | [] ->
        return_expr env
    | Update_env f :: instrs ->
        aux (f env) instrs
    | instr :: instrs ->
        let acc = aux env instrs in
          match instr with
            | Align n ->
                (<:expr<
                   let i = $lid:"pad" ^ string_of_int n$ i in
                     $acc$
                     >>)
            | Check_size_fixed n -> check_size <:expr< $expr_of_int n$ >> acc
            | Check_size_dynamic n -> check_size <:expr< len + $expr_of_int n$ >> acc
            | Check_array_size _ -> assert false
            | Advance_fixed(x, false) ->
                (<:expr<
                   let i = i + $expr_of_int x$ in
                     $acc$
                     >>)
            | Advance_fixed(1, true) ->
                (<:expr<
                   String.unsafe_set buffer i '\x00';
                 let i = i + 1 in
                   $acc$
                   >>)
            | Advance_fixed(n, true) ->
                (<:expr<
                   $lid:"zero" ^ string_of_int n$ i;
                 let i = i + $expr_of_int n$ in
                   $acc$
                   >>)
            | Advance_dynamic(x) ->
                (<:expr<
                   let i = i + len + $expr_of_int x$ in
                     $acc$
                     >>)
            | Expr(_, f) -> f env acc
            | Branches(expr, brs) ->
                (<:expr<
                   let (buffer, i) = match $expr env $ with
                       $ Ast.mcOr_of_list
                         (List.map
                            (fun (patt, instrs, _) ->
                               <:match_case< $patt env$ -> $generate_reader for_array env instrs (fun env -> <:expr< i >>)$ >>)
                            brs) $
                   in
                     $acc$
                     >>)
            | _ -> acc
  in
    aux env instructions
