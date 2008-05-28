(*
 * genCode.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Types
open AbstractCode
open CodeConstants
open Helpers

let _loc = Loc.ghost

let len_plus = function
  | 0 -> (<:expr< len >>)
  | n -> (<:expr< len + $expr_of_int n$ >>)

let rec generate_reader for_array remove_final_adv env instructions return_expr =
  let check_size expr next = match for_array with
    | true ->
        (<:expr<
           if i + $expr$ > limit
           then raise Reading.Invalid_array_size
           else $next$
         >>)
    | false ->
        (<:expr<
           if i + $expr$ > String.length buffer
           then raise Reading.Invalid_message_size
           else $next$
         >>)
  in
  let rec aux env = function
    | [] ->
        (remove_final_adv, return_expr env)
    | Update_env f :: instrs ->
        aux (f env) instrs
    | instr :: instrs ->
        let (final, acc) = aux env instrs in
          match instr with
            | Align n when not final ->
                (false,
                 <:expr<
                   let i = i + (($expr_of_int n$ - i) land $expr_of_int (n - 1)$) in
                     $acc$
                     >>)
            | Check_size_fixed n -> (false, check_size <:expr< $expr_of_int n$ >> acc)
            | Check_size_dynamic n -> (false, check_size (len_plus n) acc)
            | Check_array_size(gap, base_size) ->
                (false,
                 <:expr<
                   if ($len_plus gap$) mod $expr_of_int base_size$ <> 0
                   then raise Reading.Invalid_array_size
                   else $acc$ >>)
            | Advance_fixed(x, _) when not final ->
                (false,
                 <:expr<
                   let i = i + $expr_of_int x$ in
                     $acc$
                     >>)
            | Advance_dynamic(x) ->
                (false,
                 <:expr<
                   let i = i + $len_plus x$ in
                     $acc$
                     >>)
            | Expr(_, f) -> (false, f env acc)
            | Branches(expr, brs) ->
                (false,
                 <:expr<
                   let i, $id:Env.last env$ = match $expr env$ with
                       $ Ast.mcOr_of_list
                         (List.map
                            (fun (patt, instrs, ret) ->
                               <:match_case< $patt env$ -> $generate_reader for_array false env instrs (fun env -> <:expr< i, $ret env$ >>) $ >>)
                            brs) $
                   in
                     $acc$
                     >>)
            | _ -> (final, acc)
  in
    snd (aux env instructions)

let rec generate_writer for_array remove_final_adv env instructions return_expr =
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
        (remove_final_adv, return_expr env)
    | Update_env f :: instrs ->
        aux (f env) instrs
    | instr :: instrs ->
        let (final, acc) = aux env instrs in
          match instr with
            | Align n ->
                (false,
                 <:expr<
                   let diff = ($expr_of_int n$ - i) land $expr_of_int (n - 1)$ in
                     String.unsafe_fill buffer i diff $expr_of_chr '\x00'$;
                     $ if final
                     then acc
                     else (<:expr< let i = i + diff in $acc$ >>) $
                       >>)
            | Check_size_fixed n -> (false, check_size <:expr< $expr_of_int n$ >> acc)
            | Check_size_dynamic n -> (false, check_size (len_plus n) acc)
            | Check_array_size _ -> assert false
            | Advance_fixed(x, false) when not final ->
                (false,
                 <:expr<
                   let i = i + $expr_of_int x$ in
                     $acc$
                     >>)
            | Advance_fixed(1, true) when not final ->
                (false,
                 <:expr<
                   String.unsafe_set buffer i $expr_of_chr '\x00'$;
                 let i = i + 1 in
                   $acc$
                   >>)
            | Advance_fixed(n, true) when not final ->
                (false,
                 <:expr<
                   String.unsafe_fill buffer i $expr_of_int n$ $expr_of_chr '\x00'$;
                 let i = i + $expr_of_int n$ in
                   $acc$
                   >>)
            | Advance_dynamic(x) when not final ->
                (false,
                 <:expr<
                   let i = i + $len_plus x$ in
                     $acc$
                     >>)
            | Expr(_, f) -> (false, f env acc)
            | Branches(expr, brs) ->
                (false,
                 <:expr<
                   let (buffer, i) = match $expr env $ with
                       $ Ast.mcOr_of_list
                         (List.map
                            (fun (patt, instrs, _) ->
                               <:match_case< $patt env$ -> $generate_reader for_array false env instrs (fun env -> <:expr< i >>)$ >>)
                            brs) $
                   in
                     $acc$
                     >>)
            | _ -> (final, acc)
  in
    snd (aux env instructions)
