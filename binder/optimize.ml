(*
 * optimize.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types
open AbstractCode

type optimize_result = {
  opt_code : code;
  opt_size : int option;
  opt_without_initial_check : code;
  opt_alignment : int;
  opt_relative_position : int;
}

(* Replace Align instructions by Advance when possible. Return an
   optimized code plus the padding after the execution of the
   instruction with the given initial padding. It must be the first
   optimization. *)
let rec optimize_padding padding_important relative_pos padding = function
  | instr :: instrs ->
      let next is relative_pos padding =
        let (relative_pos, padding, instrs) = optimize_padding padding_important relative_pos padding instrs in
          (relative_pos, padding, is @ instrs) in
        begin match instr with
          | Align 1 ->
              next [] relative_pos padding
          | Align n ->
              if padding < n
              then
                (* Deduced informations are not sufficient to calculate
                   explicit padding, so we need dynamic padding *)
                next [Align n] 0 n
              else
                (* We have enough information to calculate the needed
                   padding *)
                let diff = (n - relative_pos) land (n - 1) in
                  if diff = 0
                  then
                    (* We are already padded to the wanted boundary,
                       nothing to do *)
                    next [] relative_pos padding
                  else
                    next [Advance_fixed(diff, padding_important)]
                      ((relative_pos + diff) mod padding) padding
          | Reset_padding(relative_pos, padding) ->
              next [] relative_pos padding
          | Reset_all ->
              next [Reset_all] 0 1
          | Advance_fixed(n, b) ->
              next [Advance_fixed(n, b)] ((relative_pos + n) mod padding) padding
          | Branches(expr, brs) ->
              next
                [Branches(expr,
                           List.map
                             (fun (patt, code, ret) ->
                                (patt, (let (_, _, x) = optimize_padding padding_important relative_pos padding code in x), ret))
                             brs)] relative_pos padding
          | _ -> next [instr] relative_pos padding
        end
  | [] -> (relative_pos, padding, [])

let size code =
  List.fold_left
    (fun acc instr -> match instr with
       | Advance_fixed(n, _) -> begin match acc with
           | Some x -> Some (x + n)
           | None -> None
         end
       | Reset_all
       | Align _
       | Advance_dynamic _
       | Branches _ -> None
       | _ -> acc) (Some 0) code

(* Factorise size-checkings *)
let rec optimize_check_size instructions =
  let insert_check count instr instrs = match count with
    | 0 -> instr :: instrs
    | n -> instr :: Check_size_fixed count :: instrs
  in
  let rec simplify count = function
    | instr :: instrs -> begin match instr with
        | Check_size_dynamic n ->
            (* We can no more factorize checking *)
            let count_after, simplified = simplify 0 instrs in
              (count, Check_size_dynamic (n + count_after) :: simplified)
        | Advance_fixed(n, _) ->
            let count_after, simplified = simplify (count + n) instrs in
              (count_after, instr :: simplified)
        | Branches(expr, brs) ->
            let count_after, simplified = simplify 0 instrs in
              (count,
               insert_check
                 count_after
                 (Branches(expr,
                            List.map (fun (patt, code, ret) ->
                                        (patt,
                                         (let count, code = optimize_check_size code in
                                            insert_check count Nothing code),
                                         ret))
                              brs))
                 simplified)
        | Reset_all
        | Align _ ->
            let count_after, simplified = simplify 0 instrs in
              (count, insert_check count_after instr simplified)
        | _ ->
            let count, simplified = simplify count instrs in
              (count, instr :: simplified)
      end
    | [] -> (count, [])
  in
    simplify 0 instructions

(* Factorize advance instruction *)
let optimize_advance code =
  let rec aux = function
    | [] -> []
    | instr :: instrs -> match instr with
        | Advance_fixed(n, b) ->
            let m, instrs = aux2 0 b instrs in
              Advance_fixed(n + m, b) :: instrs
        | _ -> instr :: aux instrs
  and aux2 n b code = match code with
    | [] -> (n, [])
    | instr :: instrs ->
        match instr with
          | Advance_fixed(n', b') when b' = b ->
              aux2 (n + n') b instrs
          | Reset_all
          | Check_size_fixed _
          | Check_size_dynamic _
          | Check_array_size _
          | Advance_fixed _
          | Advance_dynamic _
          | Align _
          | Branches _
          | Expr(true, _) ->
              (n, aux code)
          | _ ->
              let n, instrs = aux2 n b instrs in
                (n, instr :: instrs)
  in
    aux code

let optimize padding_important rel_pos padding code =
  let (rel_pos_end, padding_end, code) = optimize_padding padding_important rel_pos padding code in
  let size = size code in
  let count, code = optimize_check_size code in
  let code = optimize_advance code in
  let with_initial_check = match count with
    | 0 -> code
    | _ -> Check_size_fixed count :: code in
    {
      opt_code = with_initial_check;
      opt_size = size;
      opt_without_initial_check = code;
      opt_alignment = padding_end;
      opt_relative_position = rel_pos_end;
    }
