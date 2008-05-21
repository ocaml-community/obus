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

let _loc = Loc.ghost

let rec generate_reader instructions for_array return_expr =
  let check_size expr = match for_array with
    | true ->
        (<:expr<
           if i + $expr$ > limit
           then raise (Data_error "invalid array size")
             >>)
    | false ->
        (<:expr<
           if i + $expr$ > limit
           then raise (Data_error "invalid message size")
             >>)
  in
  let rec aux instrs env = match instrs with
    | [] ->
        return_expr
    | instr :: instrs ->
        let next = aux instrs in
          match instr with
            | Align n ->
                (<:expr<
                   let i = i + (($int:n$ - i) land $int:n - 1$) in
                     $next env$
                     >>)
            | Check chk ->
                (<:expr<
                   $ match chk with
                     | Chk_size_fixed n -> check_size <:expr< $int:n$ >>
                     | Chk_size_dynamic _ -> check_size <:expr< len + $int:n$ >>
                     | Chk_array_size(gap, base_size) ->
                         (<:expr<
                            if (len + $int:gap$) mod $int:base_size$ <> 0
                            then raise (Data_error "invalid array size") >>) $ ;
                         $next env$ >>)
            | Advance_fixed(x, _) ->
                (<:expr<
                   let i = i + $int:x$ in
                     $next env$
                     >>)
            | Advance_dynamic(x) ->
                (<:expr<
                   let i = i + len + $int:x$ in
                     $next env$
                     >>)
            | Update_env f -> next (f env)
            | Expr(_, f) -> f env (next env)
            | Branches f ->
                let e, brs = f env in
                  (<:expr<
                     let i, $Env.last env$ = match $e$ with
                         $ Ast.mcOr_of_list
                           (List.map
                              (fun (patt, instrs, ret) ->
                                 <:match_case< $patt$ -> $generate_reader instrs for_array <:expr< i, $ret$ >>$ >>)) $
                         >>)
            | Reset_padding _
            | Nothing -> next env
  in
    aux instructions

let rec generate_writer instructions for_array return_expr =
  let check_size expr = match for_array with
    | true ->
        (<:expr<
           if i + $expr$ > limit
           then raise Out_of_bounds
             >>)
    | false ->
        (<:expr<
           if i + $expr$ > limit
           then raise Out_of_bounds
             >>)
  in
  let rec aux instrs env = match instrs with
    | [] ->
        return_expr
    | instr :: instrs ->
        let next = aux instrs in
          match instr with
            | Align n ->
                (<:expr<
                   let i = $lid:"pad" ^ string_of_int n$ in
                     $next env$
                     >>)
            | Check chk ->
                (<:expr<
                   $ match chk with
                     | Chk_size_fixed n -> check_size <:expr< $int:n$ >>
                     | Chk_size_dynamic _ -> check_size <:expr< len + $int:n$ >>
                     | Chk_array_size _ -> assert false $ ;
                         $next env$ >>)
            | Advance_fixed(x, false) ->
                (<:expr<
                   let i = i + $int:x$ in
                     $next env$
                     >>)
            | Advance_fixed(1, true) ->
                (<:expr<
                   String.unsafe_set buffer i '\x00';
                 let i = i + 1 in
                   $next env$
                   >>)
            | Advance_fixed(n, true) ->
                (<:expr<
                   $lid:"zero" ^ n$;
                 let i = i + $int:x$ in
                   $next env$
                   >>)
            | Advance_dynamic(x) ->
                (<:expr<
                   let i = i + len + $int:x$ in
                     $next env$
                     >>)
            | Update_env f -> next (f env)
            | Expr(_, f) -> f env (next env)
            | Branches f ->
                let e, brs = f env in
                  (<:expr<
                     let i = match $e$ with
                         $ Ast.mcOr_of_list
                           (List.map
                              (fun (patt, instrs, _) ->
                                 <:match_case< $patt$ -> $generate_reader instrs for_array <:expr< i >>$ >>)) $
                         >>)
            | Reset_padding _
            | Nothing -> next env
  in
    aux instructions
