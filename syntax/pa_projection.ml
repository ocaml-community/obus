(*
 * pa_projection.ml
 * ----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

(* Generate projections for records *)

open Camlp4.PreCast

let rec generate f = function
  | Ast.TyDcl(_loc, type_name, tps, rhs, _) ->
      begin match rhs with
        | <:ctyp< { $fields$ } >> ->
          List.map
            (f (Pa_type_conv.Gen.drop_variance_annotations _loc
                  (List.fold_left (fun acc tp -> let _loc = Ast.loc_of_ctyp tp in <:ctyp< $tp$ $acc$ >>) <:ctyp< $lid:type_name$ >> tps)))
            (List.map
               (function
                  | <:ctyp@loc< $lid:id$ : $t$ >> -> (loc, id, t)
                  | _ -> assert false)
               (Ast.list_of_ctyp fields []))

        | _ ->
            []
      end

  | Ast.TyAnd(_loc, tp1, tp2) ->
      generate f tp1 @ generate f tp2

  | _ ->
      assert false

let gen_str typ (_loc, id, t) = <:str_item< let $lid:id$ x = x.$lid:id$ >>
let gen_sig typ (_loc, id, t) = <:sig_item< val $lid:id$ : $typ$ -> $t$ >>

let _ =
  Pa_type_conv.add_generator "projection"
    (fun t -> Ast.stSem_of_list (generate gen_str t));

  Pa_type_conv.add_sig_generator "projection"
    (fun t -> Ast.sgSem_of_list (generate gen_sig t))
