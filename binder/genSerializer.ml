(*
 * genSerializer.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Btypes
open Instruction
open Helpers
open Compile
open Solver

let _loc = Loc.ghost

type eqn = caml_type * dbus_type
type sol = env Instruction.t list
type rule = (eqn, sol) Solver.rule

type e0 = expr
type e1 = expr -> expr
type e2 = expr -> expr -> expr
type e3 = expr -> expr -> expr -> expr

let match_types a b =
  let add var term sub =
    try
      if List.assoc var sub = term
      then sub
      else raise Exit
    with
      | Not_found -> (var, term) :: sub
  in

  let rec aux sub a b = match (a, b) with
    | Var v, t -> add v t sub
    | Type(ta, arga), Type(tb, argb) when ta = tb -> List.fold_left2 aux sub arga argb
    | Tuple la, Tuple lb -> List.fold_left2 aux sub la lb
    | _ -> raise Exit
  in
    try
      Some(aux [] a b)
    with
        _ -> None

let rec substitute sub =
  let rec aux = function
    | Type(id, args) -> Type(id, List.map aux args)
    | Tuple l -> Tuple(List.map aux l)
    | Var v -> List.assoc v sub
  in aux

let rec count instrs =
  List.fold_left (fun acc -> function
                    | Iconvert _
                    | Istructure -> acc
                    | _ -> acc + 1) 0 instrs

let flat instrs =
  match count instrs with
    | 0
    | 1 -> instrs
    | n -> [Ipack(Ast.paCom_of_list,
                  Ast.exCom_of_list,
                  instrs)]

(* Rule for structure *)
let structure_rule = function
  | x, `structure y ->
      dep [< (x, `seq y) >] (fun x -> Istructure :: x)
  | _ -> fail

(* Rule for dealing with tuple in types *)
let tuple_rule = function
  | Tuple [], `seq [] ->
      success []
  | Tuple [x], y ->
      dep [< (x, y) >] flat
  | x, `seq [y] ->
      dep [< (x, (y : dbus_single_type :> dbus_type)) >] (fun x -> x)
  | Tuple(x :: lx), `seq(y :: ly) ->
      dep [< (x, (y : dbus_single_type :> dbus_type)); (Tuple lx, `seq ly) >]
        (fun hd tl -> flat hd @ tl)
  | _ -> fail

let rec last = function
  | [] -> (None, [])
  | [x] -> (Some x, [])
  | x :: l -> let e, l = last l in (e, x :: l)

(* Match two tuple, starting by the end *)
let reverse_tuple_rule = function
  | Tuple lx, `seq ly -> begin
      match last lx, last ly with
        | (Some x, lx), (Some y, ly) ->
            dep [< (x, (y : dbus_single_type :> dbus_type)); (Tuple lx, `seq ly) >]
              (fun a b -> b @ flat a)
        | _ -> fail
    end
  | _ -> fail

let (>>=) x f = match x with
  | Some v -> f v
  | None -> fail

let default_actions =
  [ (char, `byte), "char_byte";
    (int, `byte), "int_byte";
    (int, `int16), "int_int16";
    (int, `int32), "int_int32";
    (int, `int64), "int_int64";
    (int, `uint16), "int_uint16";
    (int, `uint32), "int_uint32";
    (int, `uint64), "int_uint64";
    (int32, `int32), "int32_int32";
    (int64, `int64), "int64_int64";
    (int32, `uint32), "int32_uint32";
    (int64, `uint64), "int64_uint64";
    (float, `double), "float_double";
    (bool, `boolean), "bool_boolean";
    (string, `string), "string_string";
    (string, `signature), "string_signature";
    (path, `object_path), "path_object_path";
    (obus_value, `variant), "variant";
    (obus_types, `signature), "types_signature" ]

let common_rule eqn =
  Util.assoc eqn default_actions >>= (fun act -> success [Iaction act])

let common_rules =
  [ common_rule;
    structure_rule;
    tuple_rule;
    reverse_tuple_rule ]

let identity x = x

let ifmatch x a b f =
  match_types a x
  >>= (fun sub -> f (substitute sub b))

let rule_record typ fields =
  let ids = List.map (fun (name, _) -> ident_of_string name) fields in
    fun (x, y) ->
      match_types typ x
      >>= (fun sub ->
             let types = List.map (fun (_, t) -> substitute sub t) fields in
               dep [< (tuple types, y) >]
                 (fun instrs ->
                    [Ipack((fun l ->
                              patt_record
                                (List.combine ids l)),
                           (fun l ->
                              expr_record
                                (List.combine ids l)),
                           instrs)]))

module Reading =
struct
  let make_array_reader reverse empty add instrs env = match reverse with
    | false ->
        let expr, env = compile_reader instrs
          (fun exprs -> <:expr< aux i $add exprs <:expr< acc >>$ >>) env in
          (<:expr<(
             fun limit buffer i ->
               let rec aux i acc =
                 if i < limit
                 then $expr$
                 else if i > limit
                 then raise (Reading_error "invalid array size")
                 else acc
               in
                 aux i $empty$
           )>>, env)
    | true ->
        let expr, env = compile_reader instrs
          (fun exprs -> <:expr< let acc = aux i in $add exprs <:expr< acc >>$ >>) env in
          (<:expr<(
             fun limit buffer i ->
               let rec aux i =
                 if i < limit
                 then $expr$
                 else if i > limit
                 then raise (Reading_error "invalid array size")
                 else $empty$
               in
                 aux i
           )>>, env)

  let rule_array typ elt_type ?(reverse=false) empty add = function
    | x, `array y ->
        ifmatch x typ elt_type
          (fun elt_type ->
             dep [< (elt_type, (y : dbus_single_type :> dbus_type)) >]
               (fun instrs -> [Iarray(make_array_reader reverse empty
                                        (function
                                           | [x] -> add x
                                           | l -> failwith
                                               ("invalid number of read values for an array: "
                                                ^ string_of_int (List.length l)))
                                        (flat instrs), y)]))
    | _ -> fail

  let rule_dict typ key_type val_type ?(reverse=false) empty add = function
    | x, `dict(k, v) ->
        match_types typ x
        >>= (fun sub ->
               let key_type = substitute sub key_type in
               let val_type = substitute sub val_type in
                 dep [< (key_type, (k : dbus_basic_type :> dbus_type)); (val_type, (v : dbus_single_type :> dbus_type)) >]
                   (fun kis vis -> [Iarray(make_array_reader reverse empty
                                             (function
                                                | [x; y] -> add x y
                                                | l -> failwith
                                                    ("invalid number of read values for an dict: "
                                                     ^ string_of_int (List.length l)))
                                             (Istructure :: flat kis @ flat vis),
                                           `structure [(k : dbus_basic_type :> dbus_single_type); v])]))
    | _ -> fail

  let default_rules =
    common_rules
    @ [ rule_array (list (v"x")) (v"x") ~reverse:true
          (<:expr< [] >>) (fun x acc -> <:expr< $x$ :: $acc$ >>);
        rule_dict (list (tuple [v"k"; v"v"])) (v"k") (v"v")
          (<:expr< [] >>) (fun k v acc -> <:expr< ($k$, $v$) :: $acc$ >>);
        rule_dict (typ "Hashtbl.t" [v"k"; v"v"]) (v"k") (v"v")
          (<:expr< Hashtbl.create 42 >>)
          (fun k v acc -> <:expr< Hashtbl.add $acc$ $k$ $v$; $acc$ >>) ]
end

module Writing =
struct
  let make_array_writer mkfold fold instrs env =
    let patts, expr, env = compile_writer instrs (<:expr< i >>) env in
      (<:expr<(
         fun buffer i v ->
           let rec aux = $mkfold patts <:patt< i >> expr$ in
             $fold <:expr< aux >> <:expr< v >> <:expr< i >>$
       )>>, env)

  let mk_fold elt acc expr = (<:expr< fun $elt$ $acc$ -> $expr$ >>)

  let rule_array typ elt_type ?(mk_fold_func=mk_fold) fold = function
    | x, `array y ->
        ifmatch x typ elt_type
          (fun elt_type ->
             dep [< (elt_type, (y : dbus_single_type :> dbus_type)) >]
               (fun instrs -> [Iarray(make_array_writer
                                        (function
                                           | [x] -> mk_fold_func (patt_of_id x)
                                           | l -> failwith
                                               ("invalid number of written values for an array: "
                                                ^ string_of_int (List.length l)))
                                        fold (flat instrs), y)]))
    | _ -> fail

  let mk_fold k v acc expr = (<:expr< fun $k$ $v$ $acc$ -> $expr$ >>)

  let rule_dict typ key_type val_type ?(mk_fold_func=mk_fold) fold = function
    | x, `dict(k, v) ->
        match_types typ x
        >>= (fun sub ->
               let key_type = substitute sub key_type in
               let val_type = substitute sub val_type in
                 dep [< (key_type, (k : dbus_basic_type :> dbus_type)); (val_type, (v : dbus_single_type :> dbus_type)) >]
                   (fun kis vis -> [Iarray(make_array_writer
                                             (function
                                                | [x; y] -> mk_fold_func (patt_of_id x) (patt_of_id y)
                                                | l -> failwith
                                                    ("invalid number of written values for an dict: "
                                                     ^ string_of_int (List.length l)))
                                             fold (Istructure :: flat kis @ flat vis),
                                           `structure [(k : dbus_basic_type :> dbus_single_type); v])]))
    | _ -> fail

  let default_rules =
    common_rules
    @ [ rule_array (list (v"x")) (v"x")
          ~mk_fold_func:(fun elt acc expr -> <:expr< fun $acc$ $elt$ -> $expr$ >>)
          (fun f l x -> <:expr< List.fold_left $f$ $x$ $l$ >>);
        rule_dict (list (tuple [v"k"; v"v"])) (v"k") (v"v")
          ~mk_fold_func:(fun k v acc expr -> <:expr< fun $acc$ ($k$, $v$) -> $expr$ >>)
          (fun f l x -> <:expr< List.fold_left $f$ $x$ $l$ >>);
        rule_dict (typ "Hashtbl.t" [v"k"; v"v"]) (v"k") (v"v")
          (fun f l x -> <:expr< Hashtbl.fold $f$ $l$ $x$ >>) ]
end
