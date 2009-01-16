(*
 * pa_constructor.ml
 * -----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Generate functionnal constructors *)

open Camlp4.PreCast

module Gen = Pa_type_conv.Gen

let rec list_of_sum_type = function
  | <:ctyp< $x$ | $y$ >> -> list_of_sum_type x @ list_of_sum_type y
  | <:ctyp@loc< $uid:cstr$ >> -> [(loc, cstr, [])]
  | <:ctyp@loc< $uid:cstr$ of $t$ >> -> [(loc, cstr, Ast.list_of_ctyp t [])]
  | t -> Loc.raise (Ast.loc_of_ctyp t) (Stream.Error "invalid variant definition")

let rec generate f = function
  | Ast.TyDcl(_loc, type_name, tps, rhs, _) ->
      begin match rhs with
        | Ast.TySum(_, sum_type)
        | <:ctyp< private $Ast.TySum(_, sum_type)$ >> ->
          List.map
            (f (Gen.drop_variance_annotations _loc
                  (List.fold_left (fun acc tp -> let _loc = Ast.loc_of_ctyp tp in <:ctyp< $tp$ $acc$ >>) <:ctyp< $lid:type_name$ >> tps)))
            (list_of_sum_type sum_type)

        | _ ->
            []
      end

  | Ast.TyAnd(_loc, tp1, tp2) ->
      generate f tp1 @ generate f tp2

  | _ ->
      assert false

(* Generate a list of variables of the same length of [l], with
   location from elements of [l] *)
let gen_vars get_loc l =
  snd (List.fold_left (fun (n, l) e ->
                         (n - 1, (get_loc e, Printf.sprintf "x%d" n) :: l))
         (List.length l - 1, []) l)

(* Build a list of patterns/expressions from a list of variables *)
let pvars = List.map (fun (loc, name) -> Gen.idp loc name)
let evars = List.map (fun (loc, name) -> Gen.ide loc name)

let gen_str typ (_loc, cstr, tl) =
  let vars = gen_vars Ast.loc_of_ctyp tl in
  <:str_item< let $lid:String.uncapitalize cstr$ =
                $Gen.abstract _loc (pvars vars) (Gen.apply _loc <:expr< $uid:cstr$ >> (evars vars))$ >>

let gen_sig typ (_loc, cstr, tl) =
  <:sig_item< val $lid:String.uncapitalize cstr$ :
                $List.fold_right (fun t acc ->
                                    let _loc = Ast.loc_of_ctyp t in
                                    <:ctyp< $t$ -> $acc$ >>) tl typ$ >>

let _ =
  Pa_type_conv.add_generator "constructor"
    (fun t -> Ast.stSem_of_list (generate gen_str t));

  Pa_type_conv.add_sig_generator "constructor"
    (fun t -> Ast.sgSem_of_list (generate gen_sig t))
