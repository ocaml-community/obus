(*
 * pa_obus.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4
open Camlp4.PreCast
open Printf

module Id : Sig.Id = struct
  let name = "Syntactic sugar for obus types"
  let version = "0.1"
end

module Make(Syntax : Sig.Camlp4Syntax) =
struct
  open Sig
  include Syntax

  let rec expr_of_patt = function
    | <:patt@_loc< $int:s$ >> -> <:expr< $int:s$ >> (* integer constant *)
    | <:patt@_loc< $int32:s$ >> -> <:expr< $int32:s$ >>
    | <:patt@_loc< $int64:s$ >> -> <:expr< $int64:s$ >>
    | <:patt@_loc< $nativeint:s$ >> -> <:expr< $nativeint:s$ >>
    | <:patt@_loc< $chr:c$ >> -> <:expr< $chr:c$ >> (* character constant *)
    | <:patt@_loc< $str:s$ >> -> <:expr< $str:s$ >> (* string constant *)
    | <:patt@_loc< $lid:b$ >> -> <:expr< $lid:b$ >> (* local variable *)
    | <:patt@_loc< $uid:b$ >> -> <:expr< $uid:b$ >> (* variable of other module *)
    | <:patt@_loc< ($tup:p$) >> ->                  (* tuple *)
      let e = expr_of_patt p in
        <:expr< ($tup:e$) >>
    | <:patt@_loc< { $r$ } >> ->
      Ast.ExRec(_loc, recbinding_of_patt r, Ast.ExNil _loc)
    | <:patt@_loc< ($e$ : $t$) >> ->                (* type restriction *)
      let p = expr_of_patt e in
        <:expr< ($p$ : $t$) >>
    | p ->
        Loc.raise (Ast.loc_of_patt p)
          (Stream.Error "expr_of_patt: this expression is not yet supported")

  and recbinding_of_patt = function
    | <:patt@_loc< >> -> <:rec_binding< >>
    | <:patt@_loc< $i$ = $p$ >> ->
      let p = expr_of_patt p in
        <:rec_binding< $i$ = $p$ >>
    | <:patt@_loc< $p1$ ; $p2$ >> ->
        let b1 = recbinding_of_patt p1
        and b2 = recbinding_of_patt p2 in
          <:rec_binding< $b1$; $b2$ >>
    | <:patt@_loc< $anti:_$ >> ->
      Loc.raise _loc
        (Stream.Error "recbinding_of_patt: antiquotation are not yet supported")
    | p -> Loc.raise (Ast.loc_of_patt p) (Stream.Error "recbinding_of_patt: not reached")

  (*** Utils ***)

  let abstract args expr =
    List.fold_right (fun arg acc ->
                       let _loc = Ast.loc_of_patt arg in
                       <:expr< fun $arg$ -> $acc$ >>)
      args expr

  let gen_vars l =
    snd (List.fold_left (fun (n, l) e ->
                           (n - 1, (Ast.loc_of_expr e, Printf.sprintf "x%d" n) :: l))
           (List.length l - 1, []) l)

  (***** Big tuple combinator creation *****)

  let make_tuple_type _loc l =
    List.fold_right (fun typ acc ->
                       let _loc = Ast.loc_of_expr typ in
                       <:expr< OBus_value.tpair $typ$ $acc$ >>)
      l <:expr< OBus_value.dunit >>

  let make_tuple_of_seq _loc l =
    let vars = gen_vars l in
    <:expr< fun $ List.fold_right (fun (_loc, var) acc -> <:patt< ($lid:var$, $acc$) >>) vars <:patt< () >> $ ->
              ( $ Ast.exCom_of_list (List.map (fun (_loc, var) -> <:expr< $lid:var$ >>) vars) $ ) >>

  let make_tuple_to_seq _loc l =
    let vars = gen_vars l in
    <:expr< fun $ Ast.PaTup(_loc, Ast.paCom_of_list (List.map (fun (_loc, var) -> <:patt< $lid:var$ >>) vars)) $ ->
      $ List.fold_right (fun (_loc, var) acc -> <:expr< ($lid:var$, $acc$) >>) vars <:expr< () >> $ >>

  (***** type combinator quotations *****)

  let expr_of_string loc str = Gram.parse_string expr_eoi loc str

  let _ = Camlp4_config.antiquotations := true

  let typ = Gram.Entry.mk "obus type"
  let typ_eoi = Gram.Entry.mk "obus type quotation"

  let ftyp = Gram.Entry.mk "obus functionnal type"
  let ftyp_eoi = Gram.Entry.mk "obus functionnal type quotation"

  EXTEND Gram
    GLOBAL: typ typ_eoi ftyp ftyp_eoi;

    typ:
      [ "star"
          [ t = SELF; "*"; tl = star_typ ->
              let l = t :: tl in
              let count = List.length l in
              if count <= 10
                (* if there is less than 10 type, use a predefined tuple combinator *)
              then List.fold_left (fun acc e -> <:expr< $acc$ $e$ >>) <:expr< $lid:"tup" ^ string_of_int count$ >> l
                (* if there is more, create on a new specific one *)
              else <:expr< OBus_value.wrap_sequence
                $make_tuple_type _loc l$
                $make_tuple_of_seq _loc l$
                $make_tuple_to_seq _loc l$ >> ]
      | "typ1"
        [ t1 = SELF; t2 = SELF -> <:expr< $t2$ $t1$ >> ]
      | "typ2"
        [ t1 = SELF; "."; t2 = SELF ->
            (try <:expr< $id:Ast.ident_of_expr t1$.$id:Ast.ident_of_expr t2$ >>
             with Invalid_argument s -> raise (Stream.Error s))
        | t1 = SELF; "("; t2 = SELF; ")" ->
            let t = <:expr< $t1$ $t2$ >> in
            (try <:expr< $id:Ast.ident_of_expr t$ >>
             with Invalid_argument s -> raise (Stream.Error s)) ]
      | "simple"
        [ "'"; i = a_LIDENT -> <:expr< $lid:i$ >>
        | i = a_LIDENT -> <:expr< $lid:"t" ^ i$ >>
        | i = a_UIDENT -> <:expr< $uid:i$ >>
        | `ANTIQUOT((""|"typ"), a) -> expr_of_string _loc a
        | "("; t = SELF; ","; mk = comma_typ_app; ")"; i = typ LEVEL "typ2" -> mk <:expr< $i$ $t$ >>
        | "("; t = SELF; ")" -> t
        ] ];

    comma_typ_app:
      [ [ t1 = typ; ","; t2 = SELF -> fun acc -> t2 <:expr< $acc$ $t1$ >>
        | t = typ -> fun acc -> <:expr< $acc$ $t$ >>
        ] ];

    star_typ:
      [ [ `ANTIQUOT((""|"typ"), a) -> [expr_of_string _loc a]
        | t1 = typ LEVEL "typ1"; "*"; t2 = SELF -> t1 :: t2
        | t = typ LEVEL "typ1" -> [t]
        ] ];

    typ_eoi:
      [ [ t = typ; `EOI -> t ] ];

    ftyp:
      [ [ t1 = typ; "->"; t2 = SELF -> <:expr< OBus_type.abstract $t1$ $t2$ >>
        | t = typ -> <:expr< OBus_type.reply $t$ >> ] ];

    ftyp_eoi:
      [ [ t = ftyp; `EOI -> t ] ];
  END

  let expand_typ loc _loc_name_opt quotation_contents =
    Gram.parse_string typ_eoi loc quotation_contents

  let expand_ftyp loc _loc_name_opt quotation_contents =
    Gram.parse_string ftyp_eoi loc quotation_contents

  let _ =
    Syntax.Quotation.add "obus_type" Syntax.Quotation.DynAst.expr_tag expand_typ;
    Syntax.Quotation.add "obus_func_type" Syntax.Quotation.DynAst.expr_tag expand_ftyp;
    Syntax.Quotation.default := "obus_func_type"

  (***** Bitwise and flag definitions *****)

  type vrn_def =
    | Vrn_classic
    | Vrn_poly

  let make_vrn_expr typ _loc id = match typ with
    | Vrn_classic -> <:expr< $uid:id$ >>
    | Vrn_poly -> Ast.ExVrn(_loc, id)

  let make_vrn_patt typ _loc id = match typ with
    | Vrn_classic -> <:patt< $uid:id$ >>
    | Vrn_poly -> Ast.PaVrn(_loc, id)

  let vrn_type_def _loc typ cstrs = match typ with
    | Vrn_classic -> Ast.sum_type_of_list
        (List.map (fun (p, e, loc, id) -> (loc, id, [])) cstrs)
    | Vrn_poly ->
        Ast.TyVrnEq(_loc, Ast.tyOr_of_list
                      (List.map (fun (p, e, _loc, id) -> <:ctyp< ` $id$ >>) cstrs))

  let make_caml_type_def _loc name vrn_typ cstrs =
    Ast.TyDcl(_loc, name, [], vrn_type_def _loc vrn_typ cstrs, [])

  let invalid_key _loc = Loc.raise _loc (Stream.Error "bitwise keys must be integers")

  let string_of_key = function
    | <:patt@_loc< $int:n$ >> -> n
    | <:patt@_loc< $int32:n$ >> -> n
    | <:patt@_loc< $int64:n$ >> -> n
    | p -> invalid_key (Ast.loc_of_patt p)

  let bw_read = function
    | <:patt@_loc< $int:n$ >> -> <:expr< x land $int:n$ <> 0 >>
    | <:patt@_loc< $int32:n$ >> -> <:expr< Int32.log_and x $int32:n$ <> 0l >>
    | <:patt@_loc< $int64:n$ >> -> <:expr< Int64.log_and x $int64:n$ <> 0L >>
    | p -> invalid_key (Ast.loc_of_patt p)


  let bw_write = function
    | <:expr@_loc< $int:n$ >> -> <:expr< acc lor $int:n$ >>
    | <:expr@_loc< $int32:n$ >> -> <:expr< Int32.log_or acc $int32:n$ >>
    | <:expr@_loc< $int64:n$ >> -> <:expr< Int64.log_or acc $int64:n$ >>
    | e -> invalid_key (Ast.loc_of_expr e)

  let bw_empty = function
    | <:patt@_loc< $int:n$ >> -> <:expr< 0 >>
    | <:patt@_loc< $int32:n$ >> -> <:expr< 0l >>
    | <:patt@_loc< $int64:n$ >> -> <:expr< 0L >>
    | p -> invalid_key (Ast.loc_of_patt p)

  EXTEND Gram
    GLOBAL:expr str_item;

    (*** Parsing of module implementation with obus annotations ***)

    obus_poly_constructor:
      [ [ p = patt; "->"; "`"; id = a_ident ->
            (p, expr_of_patt p, _loc, id)
        ] ];

    obus_constructor:
      [ [ p = patt; "->"; id = a_UIDENT ->
            (p, expr_of_patt p, _loc, id)
        ] ];

    obus_data_type:
      [ [ "["; OPT "|"; l = LIST1 obus_poly_constructor SEP "|"; "]" -> (Vrn_poly, l)
        | OPT "|"; l = LIST1 obus_constructor SEP "|" -> (Vrn_classic, l)
        ] ];

    str_item:
      [ [ "OBUS_BITWISE"; name = a_LIDENT; ":"; key_typ = typ; "="; (vrntyp, cstrs) = obus_data_type ->
            <:str_item<
              (* First create the caml type definition *)
              type $make_caml_type_def _loc name vrntyp cstrs$

              (* Construct the combinator *)
              let $lid:"t" ^ name ^ "_list"$ = OBus_type.wrap_basic $key_typ$
                (fun x ->
                   let l = [] in
                   $ List.fold_left
                       (fun acc (patt, expr, _loc, id) ->
                          <:expr< let l = if $bw_read patt$ then $make_vrn_expr vrntyp _loc id$ :: l else l in $acc$ >>)
                       <:expr< l >> cstrs $)
                (List.fold_left
                   (fun acc x -> match x with
                        $ Ast.mcOr_of_list
                          (List.map (fun (patt, expr, _loc, id) ->
                                       <:match_case< $make_vrn_patt vrntyp _loc id$ -> $bw_write expr$ >>)
                             cstrs) $)
                   $ let (patt, expr, _loc, name) = List.hd cstrs in bw_empty patt $)
            >>

        | "OBUS_FLAG"; name = a_LIDENT; ":"; key_typ = typ; "="; (vrntyp, cstrs) = obus_data_type ->
            <:str_item<
              type $make_caml_type_def _loc name vrntyp cstrs$

              let $lid:"t" ^ name$ = OBus_type.wrap_basic $key_typ$
                (function
                     $ Ast.mcOr_of_list
                       ((List.map
                           (fun (patt, expr, _loc, id) ->
                              <:match_case< $patt$ -> $make_vrn_expr vrntyp _loc id$ >>)
                           cstrs) @
                          [ <:match_case< _ -> (failwith $str:"invalid value for " ^ name$ : $lid:name$) >> ]) $)
                (function
                     $ Ast.mcOr_of_list
                       (List.map
                          (fun (patt, expr, _loc, id) ->
                             <:match_case< $make_vrn_patt vrntyp _loc id$ -> $expr$ >>)
                          cstrs) $)
            >>

        | "OBUS_EXN"; name = a_UIDENT; "="; dbus_name = a_STRING ->
            <:str_item<
                exception $uid:name$ of OBus_error.message
                let _ = register_exn $str:dbus_name$
                  (fun msg -> $uid:name$ msg)
                  (function
                     | $uid:name$ msg -> Some msg
                     | _ -> None)
            >>

        | "OBUS_GLOBAL_EXN"; name = a_UIDENT; "="; dbus_name = a_STRING ->
            <:str_item<
                exception $uid:name$ of OBus_error.message
                let _ = OBus_error.register $str:dbus_name$
                  (fun msg -> $uid:name$ msg)
                  (function
                     | $uid:name$ msg -> Some msg
                     | _ -> None)
            >>
        ] ];
  END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
