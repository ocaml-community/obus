(*
 * optimize.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Replace Align instructions by Advance when possible. Return an
   optimized code plus the padding after the execution of the
   instruction with the given initial padding. It must be the first
   optimization. *)
let rec optimize_padding relative_pos padding = function
  | instr :: instrs ->
      let next is relative_pos padding =
        let (relative_pos, padding, instrs) = optimize_padding relative_pos padding instrs in
          (relative_pos, padding, is @ instrs) in
        begin match instr with
          | Align 1 ->
              next [] relative_pos padding
          | Align n ->
              if padding < n
              then
                (* Deduced informations are not sufficient to calculate
                   explicit padding, so we need dynamic padding *)
                next [Pad n] 0 n
              else
                (* We have enough information to calculate the needed
                   padding *)
                let diff = n - (relative_pos mod n)
                  if diff = n
                  then
                    (* We are already padded to the wanted boundary,
                       nothing to do *)
                    next relative_pos padding
                  else
                    next [Advance_fixed(diff, true)]
                      ((relative_pos + diff) mod padding) padding
          | Reset_padding(relative_pos, padding) ->
              next [] relative_pos padding
          | Advance_fixed(n, b) ->
              next [Advance_fixed(n, b)] ((relative_pos + n) mod padding) padding
          |  _ -> next instr relative_pos padding
        end
  | [] -> (relative_pos, padding, [])

let ( ++ ) a b = match a with
  | Some a -> Some (a + b)
  | _ -> None

let size code =
  List.fold_left
    (fun acc instr -> match instr with
       | Advance_fixed(n, _) -> begin match acc with
           | Some x -> Some (x + n)
           | None -> None
         end
       | Align _
       | Advance_dynamic _
       | Branches _ -> None
       | _ -> acc) (Some 0)

(* Factorise size-checkings *)
let optimize_check_size instructions =
  let rec simplify count = function
    | instr :: instrs -> begin match instr with
        | Check(Chk_size_dynamic n) ->
            (* We can no more factorize checking *)
            let count_after, simplified = simplify 0 instrs in
              (count, Check(Chk_size_dynamic (n + count_after)) :: simlplified)
        | Advance_fixed(n, _) ->
            let count_after, simplified = simplify (count + n) instrs in
              (count_after, instr :: simplified)
        | Branches _
        | Align _ ->
            let count_after, simplified = simplify 0 instrs in
              begin match count_after with
                | 0 -> (count, instr :: simlplified)
                | _ -> (count, instr :: Check (Chk_size_fixed count_after) :: simplified)
              end
        | _ ->
            let count, simplified = simplify count instrs in
              (count, instr :: simplified)
      end
    | [] -> (count, [])

(* Factorize advance instruction *)
let optimize_advance code =
  let rec aux = function
    | [] -> 0
    | instr :: instrs -> match instr with
        | Advance_fixed(n, b) ->
            let m, instrs = aux2 0 b instrs in
              Advance_fixed(n + m, b) :: instrs
        | _ -> instr :: aux instrs
  in
  let rec aux2 n b code = match code with
    | [] -> (n, [])
    | instr :: instrs ->
        match instr with
          | Advance_fixed(n', b') when b' = b ->
              aux2 (n + n') b instrs
          | Check _
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

let optmize rel_pos padding code =
  let (rel_pos_end, padding_end, code) = optimize_padding rel_pos padding code in
  let size = size code in
  let count, code = optimize_size_check code in
  let check_instrs = match count with
    | 0 -> []
    | _ -> [Check (Chk_size_fixed count)] in
  let code = optimize_advance code with
      (code, check_instr, size, rel_pos_end, padding_end)
