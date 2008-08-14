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

  let gen_vars get_loc l =
    snd (List.fold_left (fun (n, l) e ->
                           (n - 1, (get_loc e, Printf.sprintf "x%d" n) :: l))
           (List.length l - 1, []) l)

  let pvars = List.map (fun (_loc, var) -> <:patt< $lid:var$ >>)
  let evars = List.map (fun (_loc, var) -> <:expr< $lid:var$ >>)

  (***** Internal type combinators representation *****)

  type ty =
    | Tlid of Loc.t * string
    | Tuid of Loc.t * string
    | Tidapp of Loc.t * ty * ty
    | Tapp of Loc.t * ty * ty
    | Tstruct of Loc.t * ty
    | Tuple of Loc.t * ty list
    | Tdict_entry of Loc.t * ty * ty
    | Tvar of Loc.t * string
    | Tanti of Loc.t * Ast.expr

  let tuple _loc = function
    | [t] -> t
    | l -> Tuple(_loc, l)

  let loc_of_ty = function
    | Tlid(l, _) -> l
    | Tuid(l, _) -> l
    | Tidapp(l, _, _) -> l
    | Tapp(l, _, _) -> l
    | Tstruct(l, _) -> l
    | Tuple(l, _) -> l
    | Tdict_entry(l, _, _) -> l
    | Tvar(l, _) -> l
    | Tanti(l, _) -> l

  (***** obus type --> caml type *****)

  let rec ctyp_of_ty = function
    | Tlid(_loc, id) -> <:ctyp< $lid:id$ >>
    | Tuid(_loc, id) -> <:ctyp< $uid:id$ >>
    | Tidapp(_loc, a, b) ->
        (try <:ctyp< $id:Ast.ident_of_ctyp (ctyp_of_ty a)$.$id:Ast.ident_of_ctyp (ctyp_of_ty b)$ >>
         with Invalid_argument s -> raise (Stream.Error s))
    | Tapp(_loc, a, b) -> <:ctyp< $ctyp_of_ty b$ $ctyp_of_ty a$ >>
    | Tstruct(_loc, t) -> ctyp_of_ty t
    | Tuple(_loc, l) -> Ast.TyTup(_loc, Ast.tySta_of_list (List.map ctyp_of_ty l))
    | Tdict_entry(_loc, a, b) -> <:ctyp< ($ctyp_of_ty a$ * $ctyp_of_ty b$) >>
    | Tvar(_loc, v) -> <:ctyp< '$v$ >>
    | Tanti(_loc, e) -> Loc.raise _loc (Stream.Error "antiquotation not allowed here")

  (***** obus type --> expression *****)

  let make_tuple_type _loc l =
    List.fold_right (fun typ acc ->
                       let _loc = Ast.loc_of_expr typ in
                       <:expr< OBus_value.tpair $typ$ $acc$ >>)
      l <:expr< OBus_value.dunit >>

  let make_tuple_of_seq _loc l =
    let vars = gen_vars Ast.loc_of_expr l in
    <:expr< fun $ List.fold_right (fun (_loc, var) acc -> <:patt< ($lid:var$, $acc$) >>) vars <:patt< () >> $ ->
              ( $ Ast.exCom_of_list (evars vars) $ ) >>

  let make_tuple_to_seq _loc l =
    let vars = gen_vars Ast.loc_of_expr l in
    <:expr< fun $ Ast.PaTup(_loc, Ast.paCom_of_list (pvars vars)) $ ->
      $ List.fold_right (fun (_loc, var) acc -> <:expr< ($lid:var$, $acc$) >>) vars <:expr< () >> $ >>

  let make_tuple _loc = function
    | [] -> <:expr< OBus_type.tunit >>
    | [t] -> t
    | l ->
        let count = List.length l in
        if count <= 10
          (* if there is less than 10 type, use a predefined tuple combinator *)
        then List.fold_left (fun acc e -> <:expr< $acc$ $e$ >>) <:expr< $lid:"tup" ^ string_of_int count$ >> l
          (* if there is more, create on a new specific one *)
        else <:expr< OBus_value.wrap_sequence
          $make_tuple_type _loc l$
          $make_tuple_of_seq _loc l$
          $make_tuple_to_seq _loc l$ >>

  let rec expr_of_ty = function
    | Tlid(_loc, id) -> <:expr< $lid:"t" ^ id$ >>
    | Tuid(_loc, id) -> <:expr< $uid:id$ >>
    | Tidapp(_loc, a, b) ->
        (try <:expr< $id:Ast.ident_of_expr (expr_of_ty a)$.$id:Ast.ident_of_expr (expr_of_ty b)$ >>
         with Invalid_argument s -> raise (Stream.Error s))
    | Tapp(_loc, a, b) -> <:expr< $expr_of_ty a$ $expr_of_ty b$ >>
    | Tstruct(_loc, t) -> <:expr< OBus_type.tstructure $expr_of_ty t$ >>
    | Tuple(_loc, l) -> make_tuple _loc (List.map expr_of_ty l)
    | Tdict_entry(_loc, a, b) -> <:expr< OBus_type.tdict_entry $expr_of_ty a$ $expr_of_ty b$ >>
    | Tvar(_loc, v) -> <:expr< $lid:v$ >>
    | Tanti(_loc, e) -> e

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
          [ t = SELF; "*"; tl = star_typ -> tuple _loc (t :: tl) ]
      | "typ1"
        [ t1 = SELF; t2 = SELF -> Tapp(_loc, t2, t1) ]
      | "typ2"
        [ t1 = SELF; "."; t2 = SELF -> Tidapp(_loc, t1, t2) ]
      | "simple"
        [ "'"; i = a_LIDENT -> Tvar(_loc, i)
        | i = a_LIDENT -> Tlid(_loc, i)
        | i = a_UIDENT -> Tuid(_loc, i)
        | `ANTIQUOT((""|"obus_type"), a) -> Tanti(_loc, expr_of_string _loc a)
        | "("; t = SELF; ","; mk = comma_typ_app; ")"; i = typ LEVEL "typ2" -> mk (Tapp(_loc, i, t))
        | "("; t = SELF; ")" -> t
        | "["; tl = struct_typ ; "]" -> Tstruct(_loc, tuple _loc tl)
        | "{"; tk = SELF; ","; tv = SELF; "}" -> Tdict_entry(_loc, tk, tv)
        ] ];

    comma_typ_app:
      [ [ t1 = typ; ","; t2 = SELF -> fun acc -> t2 (Tapp(_loc, acc, t1))
        | t = typ -> fun acc -> Tapp(_loc, acc, t)
        ] ];

    star_typ:
      [ [ `ANTIQUOT((""|"obus_type"), a) -> [Tanti(_loc, expr_of_string _loc a)]
        | t1 = typ LEVEL "typ1"; "*"; t2 = SELF -> t1 :: t2
        | t = typ LEVEL "typ1" -> [t]
        ] ];

    struct_typ:
      [ [ tl = star_typ -> tl
        | -> []
        ] ];

    typ_eoi:
      [ [ t = typ; `EOI -> expr_of_ty t ] ];

    ftyp:
      [ [ t1 = typ; "->"; t2 = SELF -> <:expr< OBus_type.abstract $expr_of_ty t1$ $t2$ >>
        | t = typ -> <:expr< OBus_type.reply $expr_of_ty t$ >> ] ];

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

  (***** records *****)

  let make_record_decl _loc name tpl l =
    Ast.TyDcl(_loc, name, tpl,
              <:ctyp< { $Ast.record_type_of_list
                          (List.map (fun (loc, n, m, ty) ->  (loc, n, m, ctyp_of_ty ty)) l)
                          $ } >>,
              [])

  let make_record_expr _loc l =
    let vars = gen_vars (fun (l, _, _, _) -> l) l in
    <:expr< OBus_type.wrap_sequence $make_tuple _loc (List.map (fun (_, _, _, t) -> expr_of_ty t) l)$
      (fun $ Ast.PaTup(_loc, Ast.paCom_of_list (pvars vars)) $ ->
         $Ast.ExRec(_loc,
                    List.fold_left2 (fun acc (_loc, name, _, _) (_, var) ->
                                       <:rec_binding< $acc$; $lid:name$ = $lid:var$ >>)
                      (Ast.RbNil _loc) l vars,
                    Ast.ExNil _loc)$)
      (fun x ->
         $Ast.ExTup(_loc,
                    Ast.exCom_of_list
                      (List.map (fun (_loc, name, _, _) -> <:expr< x.$lid:name$ >>) l))$)
    >>

  let make_ty_def _loc n tpl expr =
    <:str_item<
      let $lid:"t" ^ n$ =
        $abstract (List.map (function
                               | <:ctyp@_loc< '$x$ >> -> <:patt< $lid:x$ >>
                               | <:ctyp@_loc< +'$x$ >> -> <:patt< $lid:x$ >>
                               | <:ctyp@_loc< -'$x$ >> -> <:patt< $lid:x$ >>
                               | t -> Loc.raise (Ast.loc_of_ctyp t) (Stream.Error "invalid type parameter"))
                     tpl)
          expr$
    >>

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

    obus_record_field:
      [ [ "mutable"; n = a_LIDENT; ":"; ty = typ -> (_loc, n, true, ty)
        | n = a_LIDENT; ":"; ty = typ -> (_loc, n, false, ty)
        ] ];

    obus_record_fields:
      [ [ a = obus_record_field; ";"; b = SELF -> a :: b;
        | x = obus_record_field -> [x]
        | -> [] ] ];

    obus_record:
      [ [ (n, tpl) = type_ident_and_parameters; "="; "{"; fields = obus_record_fields; "}" ->
            (n, tpl, fields)
        ] ];

    str_item:
      [ [ "OBUS_bitwise"; name = a_LIDENT; ":"; key_typ = typ; "="; (vrntyp, cstrs) = obus_data_type ->
            <:str_item<
              (* First create the caml type definition *)
              type $make_caml_type_def _loc name vrntyp cstrs$

              (* Construct the combinator *)
              let $lid:"t" ^ name ^ "_list"$ = OBus_type.wrap_basic $expr_of_ty key_typ$
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

        | "OBUS_flag"; name = a_LIDENT; ":"; key_typ = typ; "="; (vrntyp, cstrs) = obus_data_type ->
            <:str_item<
              type $make_caml_type_def _loc name vrntyp cstrs$

              let $lid:"t" ^ name$ = OBus_type.wrap_basic $expr_of_ty key_typ$
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

        | "OBUS_exn"; name = a_UIDENT; "="; dbus_name = a_STRING ->
            <:str_item<
                exception $uid:name$ of OBus_error.message
                let _ = register_exn $str:dbus_name$
                  (fun msg -> $uid:name$ msg)
                  (function
                     | $uid:name$ msg -> Some msg
                     | _ -> None)
            >>

        | "OBUS_global_exn"; name = a_UIDENT; "="; dbus_name = a_STRING ->
            <:str_item<
                exception $uid:name$ of OBus_error.message
                let _ = OBus_error.register $str:dbus_name$
                  (fun msg -> $uid:name$ msg)
                  (function
                     | $uid:name$ msg -> Some msg
                     | _ -> None)
            >>

        | "OBUS_record"; (n, tpl, l) = obus_record ->
            <:str_item<
                type $make_record_decl _loc n tpl l$
                $make_ty_def _loc n tpl (make_record_expr _loc l)$
            >>

        | "OBUS_struct"; (n, tpl, l) = obus_record ->
            <:str_item<
                type $make_record_decl _loc n tpl l$
                $make_ty_def _loc n tpl <:expr< OBus_type.tstructure $make_record_expr _loc l$ >>$
            >>

        | "OBUS_type"; (n, tpl) = type_ident_and_parameters; "="; t = typ ->
            <:str_item<
                type $Ast.TyDcl(_loc, n, [], ctyp_of_ty t, [])$
                $make_ty_def _loc n tpl (expr_of_ty t)$
            >>
        ] ];
  END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
