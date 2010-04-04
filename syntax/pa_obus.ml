(*
 * pa_obus.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Camlp4
open Camlp4.PreCast
open Syntax
open Printf

module Gen = Pa_type_conv.Gen

(* +-----------------------------------------------------------------+
   | Names translation                                               |
   +-----------------------------------------------------------------+ *)

let name_translator = ref `ocaml

let translate_lid str = match !name_translator with
  | `ocaml -> OBus_name.ocaml_lid str
  | `haskell -> OBus_name.haskell_lid str

let translate_uid str = match !name_translator with
  | `ocaml -> OBus_name.ocaml_uid str
  | `haskell -> OBus_name.haskell_uid str

let prepend_lid str id = match !name_translator with
  | `ocaml -> str ^ "_" ^ id
  | `haskell -> str ^ String.capitalize id

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

(* Generate a list of variables of the same length of [l], with
   location from elements of [l] *)
let gen_vars get_loc l =
  snd (List.fold_left (fun (n, l) e ->
                         (n - 1, (get_loc e, Printf.sprintf "__x%d" n) :: l))
         (List.length l - 1, []) l)

(* Build a list of patterns/expressions from a list of variables *)
let pvars = List.map (fun (loc, name) -> Gen.idp loc name)
let evars = List.map (fun (loc, name) -> Gen.ide loc name)

(* Returns the firsts [n] element of [l] and the rest of [l]. *)
let rec break_at n = function
  | [] ->
      ([], [])

  | l when n <= 0 ->
      ([], l)

  | x :: l ->
      let firsts, rest = break_at (n - 1) l in
      (x :: firsts, rest)

(* Check that a member name is valid *)
let check_member_name loc name =
  match OBus_name.validate_member name with
    | Some error ->
        Loc.raise loc (Stream.Error (OBus_string.error_message error))
    | None ->
        ()

(* Check that an interface name is valid *)
let check_interface_name loc name =
  match OBus_name.validate_interface name with
    | Some error ->
        Loc.raise loc (Stream.Error (OBus_string.error_message error))
    | None ->
        ()

(* Map a type with Lwt.t *)
let rec map_ctyp = function
  | <:ctyp@_loc< $x$ -> $y$ >> -> <:ctyp< $x$ -> $map_ctyp y$ >>
  | x -> let _loc = Ast.loc_of_ctyp x in <:ctyp< $x$ Lwt.t >>

(* Build the definition of a type combinator:

   {[
     type ('a, 'b) t = ('a, 'b) plop
   ]}

   ->

   {[
     let obus_t = fun a b -> obus_plop a b
   ]}
*)
let make_type_combinator_def _loc name tpl expr =
  <:str_item<
    let $lid:"obus_" ^ name$ = $Gen.abstract _loc (List.map (fun tp ->
                                                               let _loc = Ast.loc_of_ctyp tp in
                                                               <:patt< $lid:Gen.get_tparam_id tp$ >>) tpl) expr$
  >>

(* Returns a list of variables for all the argument of a
   functionnal type:

   [int -> string -> bool] -> [x0, x1] *)
let vars_of_func_ctyp ctyp =
  let rec collect = function
    | <:ctyp@_loc< $x$ -> $y$ >> -> x :: collect y
    | _ -> []
  in
  gen_vars Ast.loc_of_ctyp (collect ctyp)

(* +-----------------------------------------------------------------+
   | type --> type combinator expression                             |
   +-----------------------------------------------------------------+ *)

(* Return the expression for building a tuple type combinator. *)
let tuple_combinator _loc = function
  | [] -> <:expr< OBus_type.OBus_pervasives.unit >>
  | [t] -> t
  | l -> Gen.apply _loc <:expr< OBus_type.$lid:"tuple" ^ string_of_int (List.length l)$ >> l

(* The longest tuple type combinator that can be build with the predefined
   [OBus_type.tuple*] functions *)
let max_tuple = 10

(* Build a tuple in ``cascade'':

   {[
     tuple x1 x2 x3 ... x9
       (tuple x10 x11 ... x18
          (tuple x19 x20 x21))
   ]}
*)
let rec make_cascade_tuple tuple _loc l =
  match break_at 9 l with
    | [t], [] ->
        t

    | firsts, [] ->
        tuple _loc firsts

    | _, [_] ->
        tuple _loc l

    | firsts, rest ->
        tuple _loc (firsts @ [make_cascade_tuple tuple _loc rest])

(* Build a tuple type combinator in cascade:

   {[
     OBus_type.tuple10 t1 t2 t3 ... t9
       (OBus_type.tuple10 t10 t11 ... t18
          (OBus_type.tuple3 t19 t20 t21))
   ]}
*)
let make_cascade_tuple_combinator = make_cascade_tuple tuple_combinator

(* Build a cascade tuple pattern/expression:

   [(x1, x2, x3, ..., (x10, x11, ..., x18, (x19, x20, x21)))] *)
let make_cascade_tuple_patt = make_cascade_tuple (fun _loc l -> <:patt< $tup:Ast.paCom_of_list l$ >>)
let make_cascade_tuple_expr = make_cascade_tuple (fun _loc l -> <:expr< $tup:Ast.exCom_of_list l$ >>)

(* Build a tuple pattern/expression *)
let make_tuple_patt loc patts = Ast.PaTup(loc, Ast.paCom_of_list patts)
let make_tuple_expr loc exprs = Ast.ExTup(loc, Ast.exCom_of_list exprs)

(* Build the cast mapper for a tuple in ``cascade'':

   {[
     fun (x1, x2, x3, ..., (x10, x11, ..., x18, (x19, x20, x21))) ->
       (x1, x2, x3, ..., x21)
   ]}
*)
let make_cascade_tuple_mapper_cast _loc l =
  let vars = gen_vars Ast.loc_of_expr l in
  <:expr< fun $make_cascade_tuple_patt _loc (pvars vars)$ -> $make_tuple_expr _loc (evars vars)$ >>

(* Build the make mapper for a tuple in ``cascade'':

   {[
     fun (x1, x2, x3, ..., x21) ->
       (x1, x2, x3, ..., (x10, x11, ..., x18, (x19, x20, x21)))
   ]}
*)
let make_cascade_tuple_mapper_make _loc l =
  let vars = gen_vars Ast.loc_of_expr l in
  <:expr< fun $make_tuple_patt _loc (pvars vars)$ -> $make_cascade_tuple_expr _loc (evars vars)$ >>

(* Build the type combinator for a tuple *)
let make_tuple_combinator _loc l =
  if List.length l <= 10 then
    (* if there is less than 10 type, use a predefined tuple type
       combinator *)
    tuple_combinator _loc l

  else
    (* if there is more, create on a new specific one *)
    <:expr< OBus_type.map
              $make_cascade_tuple_combinator _loc l$
              $make_cascade_tuple_mapper_cast _loc l$
              $make_cascade_tuple_mapper_make _loc l$ >>

(* Add the "obus_" prefix to an identifier:

   [A.B.t] -> [A.B.obus_t]
*)
let rec map_ident = function
  | <:ident@_loc< $id1$ . $id2$ >> -> <:ident< $id1$ . $map_ident id2$ >>
  | <:ident@_loc< $lid:id$ >> -> <:ident< $lid:"obus_" ^ id$ >>
  | id -> id

(* Build a type combinator from a caml type *)
let rec type_combinator_of_ctyp = function
  | <:ctyp@_loc< $id:id$ >> -> <:expr< $id:map_ident id$ >>
  | <:ctyp@_loc< $x$ $y$ >> -> <:expr< $type_combinator_of_ctyp y$ $type_combinator_of_ctyp x$ >>
  | <:ctyp@_loc< $tup:l$ >> -> make_tuple_combinator _loc (List.map type_combinator_of_ctyp (Ast.list_of_ctyp l []))
  | <:ctyp@_loc< '$x$ >> -> <:expr< $lid:x$ >>
  | t -> Loc.raise (Ast.loc_of_ctyp t) (Failure "pa_obus: this kind of type cannot be used here")

(* Build a functionnal type combinator from a caml type *)
let rec func_combinator_of_ctyp = function
  | Ast.TyArr(_loc, Ast.TyLab(_, name, x), y) ->
      let y, names = func_combinator_of_ctyp y in
      (<:expr< OBus_type.abstract $type_combinator_of_ctyp x$ $y$ >>,
       (_loc, Some name) :: names)
  | <:ctyp@_loc< $x$ -> $y$ >> ->
      let y, names = func_combinator_of_ctyp y in
      (<:expr< OBus_type.abstract $type_combinator_of_ctyp x$ $y$ >>,
       (_loc, None) :: names)
  | t ->
      let _loc = Ast.loc_of_ctyp t in
      (<:expr< OBus_type.reply $type_combinator_of_ctyp t$ >>, [])

(* +-----------------------------------------------------------------+
   | Type combinator quotations                                      |
   +-----------------------------------------------------------------+ *)

let ctyp_eoi = Gram.Entry.mk "ctyp_eoi"

EXTEND Gram
  ctyp_eoi:
    [ [ t = ctyp; `EOI ->  t ] ];
END

let expand_type loc _loc_name_opt quotation_contents =
  type_combinator_of_ctyp (Gram.parse_string ctyp_eoi loc quotation_contents)

let expand_func loc _loc_name_opt quotation_contents =
  fst (func_combinator_of_ctyp (Gram.parse_string ctyp_eoi loc quotation_contents))

let _ =
  Quotation.add "obus_type" Quotation.DynAst.expr_tag expand_type;
  Quotation.add "obus_func" Quotation.DynAst.expr_tag expand_func

(* +-----------------------------------------------------------------+
   | The syntax extension                                            |
   +-----------------------------------------------------------------+ *)

EXTEND Gram
  GLOBAL:str_item class_expr;

  (* A member name, the D-Bus name is always specified, and the caml name
     may be sepecified with the "as" keyword *)
  obus_member:
    [ [ dbus_name = a_ident; "as"; caml_name = a_ident ->
          check_member_name _loc dbus_name;
          (dbus_name, caml_name)
      | dbus_name = a_ident ->
          check_member_name _loc dbus_name;
          (dbus_name, translate_lid dbus_name)
      ] ];

  obus_type:
    [ [ t = ctyp -> type_combinator_of_ctyp t ] ];

  obus_func:
    [ [ t = ctyp -> func_combinator_of_ctyp t ] ];

  str_item:
    [ [ "OBUS_name_translator"; n = a_STRING ->
          begin match n with
            | "ocaml" -> name_translator := `ocaml
            | "haskell" -> name_translator := `haskell
            | _ -> Loc.raise _loc (Stream.Error (sprintf "invalid name translator: %S, must be \"ocaml\" or \"haskell\"" n))
          end;
          <:str_item< >>

  (* +---------------------------------------------------------------+
     | Extension for proxy code                                      |
     +---------------------------------------------------------------+ *)

      | "OP_method"; (dname, cname) = obus_member; ":"; (typ, names) = obus_func ->
          if List.exists (fun (_loc, name) -> name <> None) names then
            let anonymous_names = gen_vars fst names in
            let pnames = List.map2 (fun (_, anon) (_loc, label) ->
                                      match label with
                                        | Some label -> Ast.PaLab(_loc, label, Ast.PaNil _loc)
                                        | None -> Gen.idp _loc anon) anonymous_names names
            and enames = List.map2 (fun (_, anon) (_loc, label) ->
                                      match label with
                                        | Some label -> Gen.ide _loc label
                                        | None -> Gen.ide _loc anon) anonymous_names names in
            <:str_item< let $lid:cname$ =
                          let __obus_type = $typ$ in
                          $Gen.abstract _loc (<:patt< __obus_proxy >> :: pnames)
                            (Gen.apply _loc <:expr< OBus_proxy.Interface.method_call op_interface $str:dname$ __obus_type __obus_proxy >> enames)$ >>
          else
            <:str_item< let $lid:cname$ = OBus_proxy.Interface.method_call op_interface $str:dname$ $typ$ >>

      | "OP_signal"; (dname, cname) = obus_member; ":"; typ = obus_type ->
          <:str_item< let $lid:cname$ = OBus_proxy.Interface.signal op_interface $str:dname$ $typ$ >>

      | "OP_property_r"; (dname, cname) = obus_member; ":"; typ = obus_type ->
          <:str_item< let $lid:cname$ = OBus_proxy.Interface.property op_interface $str:dname$ OBus_property.readable $typ$ >>

      | "OP_property_w"; (dname, cname) = obus_member; ":"; typ = obus_type ->
          <:str_item< let $lid:cname$ = OBus_proxy.Interface.property op_interface $str:dname$ OBus_property.writable $typ$ >>

      | "OP_property_rw"; (dname, cname) = obus_member; ":"; typ = obus_type ->
          <:str_item< let $lid:cname$ = OBus_proxy.Interface.property op_interface $str:dname$ OBus_property.readable_writable $typ$ >>

  (* +---------------------------------------------------------------+
     | Extension for local code                                      |
     +---------------------------------------------------------------+ *)

      | "OL_method"; (dname, cname) = obus_member; ":"; (typ, names) = obus_func; e = equal_expr ->
          (match e with
             | Some e ->
                 let vars = gen_vars fst names in
                 <:str_item< let $lid:cname$ =
                               let __obus_func = $e$ in
                               OBus_object.Interface.method_call ol_interface $str:dname$ $typ$
                                 $Gen.abstract _loc (pvars vars) (Gen.apply _loc <:expr< __obus_func >> (evars vars))$;
                               __obus_func >>
             | None ->
                 let vars = gen_vars (fun _ -> _loc) names in
                 <:str_item< let () = OBus_object.Interface.method_call ol_interface $str:dname$ $typ$ $Gen.abstract _loc (pvars vars) (Gen.apply _loc (Gen.ide _loc cname) (evars vars))$ >>)

      | "OL_signal"; (dname, cname) = obus_member; ":"; typ = obus_type ->
          <:str_item< let () = OBus_object.Interface.signal ol_interface $str:dname$ $typ$
                      let $lid:cname$ = OBus_object.Interface.emit ol_interface $str:dname$ $typ$ >>

      | "OL_property_r"; (dname, cname) = obus_member; ":"; typ = obus_type; e = equal_expr ->
          (match e with
             | Some e ->
                 <:str_item< let () = OBus_object.Interface.property_r ol_interface $str:dname$ $typ$ $e$ >>
             | None ->
                 <:str_item< let () = OBus_object.Interface.property_r ol_interface $str:dname$ $typ$ (fun __obj -> __obj.$lid:cname$) >>)

      | "OL_property_w"; (dname, cname) = obus_member; ":"; typ = obus_type; e = equal_expr ->
          (match e with
             | Some e ->
                 <:str_item< let () = OBus_object.Interface.property_w ol_interface $str:dname$ $typ$ $e$ >>
             | None ->
                 <:str_item< let () = OBus_object.Interface.property_w ol_interface $str:dname$ $typ$ (fun __obj -> __obj.$lid:prepend_lid "set" cname$) >>)

      | "OL_property_rw"; (dname, cname) = obus_member; ":"; typ = obus_type; es = equal_expr2 ->
          (match es with
             | Some(reader, writer) ->
                 <:str_item< let () = OBus_object.Interface.property_rw ol_interface $str:dname$ $typ$ $reader$ $writer$ >>
             | None ->
                 <:str_item< let () = OBus_object.Interface.property_rw ol_interface $str:dname$ $typ$ (fun __ obj -> __obj. $lid:cname$) (fun __obj -> __obj.$lid:prepend_lid "set" cname$) >>)

      ] ];

  equal_expr:
    [ [ "="; e = expr ->
          Some e
      | ->
          None ] ];

  equal_expr2:
    [ [ "="; e1 = expr LEVEL "simple"; e2 = expr LEVEL "simple" ->
          Some(e1, e2)
      | ->
          None ] ];
END

(* +-----------------------------------------------------------------+
   | Generators for types                                            |
   +-----------------------------------------------------------------+ *)

let rec generate f = function
  | Ast.TyDcl(loc, name, tpl, typ, _) ->
      [f loc name tpl typ]

  | Ast.TyAnd(_loc, tp1, tp2) ->
      generate f tp1 @ generate f tp2

  | _ ->
      assert false

(* Build a mapper for a record type *)
let make_record_combinator _loc fields =
  let vars = gen_vars (fun (loc, id, t) -> loc) fields in
  <:expr<
    OBus_type.map $make_cascade_tuple_combinator _loc
                      (List.map (fun (_, _, t) -> type_combinator_of_ctyp t) fields)$
      (fun $make_cascade_tuple_patt _loc (pvars vars)$ ->
         $Ast.ExRec(_loc,
                    Ast.rbSem_of_list
                      (List.map2 (fun (_loc, name, _) (_, var) -> <:rec_binding< $lid:name$ = $lid:var$ >>) fields vars),
                    Ast.ExNil _loc)$)
      (fun x ->
         $make_cascade_tuple_expr _loc
            (List.map (fun (_loc, name, _) -> <:expr< x.$lid:name$ >>) fields)$)
  >>

let _ =
  Pa_type_conv.add_generator "obus"
    (fun typ -> Ast.stSem_of_list
       (generate (fun loc name tpl typ ->
                    let rec loop = function
                      | <:ctyp< private $t$ >> ->
                          loop t
                      | <:ctyp@_loc< $id:_$ >>
                      | <:ctyp@_loc< ( $tup:_$ ) >>
                      | <:ctyp@_loc< '$_$ >>
                      | <:ctyp@_loc< $_$ $_$ >> as typ ->
                          make_type_combinator_def _loc name tpl (type_combinator_of_ctyp typ)
                      | <:ctyp@_loc< { $fields$ } >> ->
                          make_type_combinator_def _loc name tpl (make_record_combinator _loc
                                                                    (List.map
                                                                       (function
                                                                          | <:ctyp@loc< $lid:id$ : mutable $t$ >> -> (loc, id, t)
                                                                          | <:ctyp@loc< $lid:id$ : $t$ >> -> (loc, id, t)
                                                                          | _ -> assert false)
                                                                       (Ast.list_of_ctyp fields [])))
                      | <:ctyp< $tp1$ = $tp2$ >> ->
                          loop tp2
                      | _ ->
                          Loc.raise (Ast.loc_of_ctyp typ) (Stream.Error "pa_obus: obus does not know how to handle this type")
                    in
                    loop typ) typ))

let obus_type_class = Gram.Entry.mk "obus_type_class"
let obus_type_class_eoi = Gram.Entry.mk "obus_type_class_eoi"

let valid_lclasses = ["basic"; "single"; "sequence"]
let valid_rclasses = ["basic"; "container"; "sequence"]

EXTEND Gram
  GLOBAL: obus_type_class obus_type_class_eoi;

  obus_type_class:
    [ [ x = a_LIDENT; "->"; (y, ret) = SELF ->
          if List.mem x valid_lclasses then
            (x :: y, ret)
          else if x = "container" then
            Loc.raise _loc (Stream.Error "this class can not be used at the left side of an arrow")
          else
            Loc.raise _loc (Stream.Error (sprintf "pa_obus: invalid classe %s, must be one of %s"
                                            x (String.concat ", " valid_lclasses)))
      | x = a_LIDENT ->
          if List.mem x valid_rclasses then
            ([], x)
          else if x = "single" then
            Loc.raise _loc (Stream.Error "this class can not be used at the right side of an arrow")
          else
            Loc.raise _loc (Stream.Error (sprintf "pa_obus: invalid classe %s, must be one of %s"
                                            x (String.concat ", " valid_rclasses)))
      ] ];

  obus_type_class_eoi:
    [ [ (l, ret) = obus_type_class; `EOI -> (_loc, l, ret) ] ];
END

let _ =
  Pa_type_conv.add_sig_generator_with_arg "obus" obus_type_class_eoi
    (fun typ arg ->
       match arg with
         | None ->
             Loc.raise (Ast.loc_of_ctyp typ) (Stream.Error "pa_obus: argument recquired for the 'obus' generator")

         | Some(loc, classes, ret_class) ->
             Ast.sgSem_of_list
               (generate
                  (fun _loc name tpl typ ->
                     if List.length tpl <> List.length classes then
                       Loc.raise loc (Stream.Error "wrong number of arguments")
                     else
                       let ret_typ = Pa_type_conv.Gen.drop_variance_annotations _loc
                         (List.fold_left (fun acc tp ->
                                            let _loc = Ast.loc_of_ctyp tp in
                                            <:ctyp< $tp$ $acc$ >>) <:ctyp< $lid:name$ >> tpl) in
                       <:sig_item<
                         val $lid:"obus_" ^ name$ :
                           $List.fold_right2
                              (fun tp cl acc ->
                                 <:ctyp< ('$Gen.get_tparam_id tp$, _) OBus_type.$lid:"cl_" ^ cl$ -> $acc$ >>)
                              tpl classes <:ctyp< $ret_typ$ OBus_type.$lid:ret_class$ >>$
                       >>) typ))

(* +-----------------------------------------------------------------+
   | Generators for exceptions                                       |
   +-----------------------------------------------------------------+ *)

let _ =
  Pa_type_conv.add_generator_with_arg ~is_exn:true "obus" expr_eoi
    (fun typ arg -> match typ, arg with
       | _, None ->
           Loc.raise (Ast.loc_of_ctyp typ) (Stream.Error "pa_obus: argument recquired for the 'obus' generator")

       | <:ctyp@_loc< $uid:caml_name$ of $_$ >>, Some dbus_name ->
           <:str_item<
             let _ = OBus_error.register $dbus_name$
               (fun msg -> $uid:caml_name$ msg)
               (function
                  | $uid:caml_name$ msg -> Some msg
                  | _ -> None)
           >>

       | _ ->
           Loc.raise (Ast.loc_of_ctyp typ) (Stream.Error "pa_obus: ``Caml_name of string'' expected"))
