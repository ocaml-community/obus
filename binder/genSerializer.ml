(*
 * genSerializer.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Types
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

(* Rule for dealing with tuple in types *)
let tuple_rule = function
  | Tuple [x], y ->
      dep [< (x, y) >] flat
  | x, Tstructure [y] ->
      dep [< (x, y) >] (fun x -> Istructure :: x)
  | Tuple(x :: lx), Tstructure(y :: ly) ->
      dep [< (x, y); (Tuple lx, Tstructure ly) >]
        (fun hd tl -> flat hd @ tl)
  | _ -> fail

let rec last = function
  | [] -> (None, [])
  | [x] -> (Some x, [])
  | x :: l -> let e, l = last l in (e, x :: l)

(* Match two tuple, starting by the end *)
let reverse_tuple_rule = function
  | Tuple lx, Tstructure ly -> begin
      match last lx, last ly with
        | (Some x, lx), (Some y, ly) ->
            dep [< (x, y); (Tuple lx, Tstructure ly) >]
              (fun a b -> b @ flat a)
        | _ -> fail
    end
  | _ -> fail

let (>>=) x f = match x with
  | Some v -> f v
  | None -> fail

let default_actions =
  [ (char, Tbyte), "char_byte";
    (int, Tbyte), "int_byte";
    (int, Tint16), "int_int16";
    (int, Tint32), "int_int32";
    (int, Tint64), "int_int64";
    (int, Tuint16), "int_uint16";
    (int, Tuint32), "int_uint32";
    (int, Tuint64), "int_uint64";
    (int32, Tint32), "int32_int32";
    (int64, Tint64), "int64_int64";
    (int32, Tuint32), "int32_uint32";
    (int64, Tuint64), "int64_uint64";
    (float, Tdouble), "float_double";
    (bool, Tboolean), "bool_boolean";
    (string, Tstring), "string_string";
    (string, Tsignature), "string_signature";
    (string, Tobject_path), "string_object_path";
    (obus_value, Tvariant), "variant";
    (obus_dtypes, Tsignature), "dtypes" ]

let common_rule eqn =
  Util.assoc eqn default_actions >>= (fun act -> success [Iaction act])

let common_rules =
  [ common_rule;
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
    | x, Tarray y ->
        ifmatch x typ elt_type
          (fun elt_type ->
             dep [< (elt_type, y) >]
               (fun instrs -> [Iarray(make_array_reader reverse empty
                                        (function
                                           | [x] -> add x
                                           | l -> failwith
                                               ("invalid number of read values for an array: "
                                                ^ string_of_int (List.length l)))
                                        (flat instrs), y)]))
    | _ -> fail

  let rule_dict typ key_type val_type ?(reverse=false) empty add = function
    | x, Tdict(k, v) ->
        match_types typ x
        >>= (fun sub ->
               let key_type = substitute sub key_type in
               let val_type = substitute sub val_type in
                 dep [< (key_type, k); (val_type, v) >]
                   (fun kis vis -> [Iarray(make_array_reader reverse empty
                                             (function
                                                | [x; y] -> add x y
                                                | l -> failwith
                                                    ("invalid number of read values for an dict: "
                                                     ^ string_of_int (List.length l)))
                                             (Istructure :: flat kis @ flat vis),
                                           Tstructure [k; v])]))
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
    | x, Tarray y ->
        ifmatch x typ elt_type
          (fun elt_type ->
             dep [< (elt_type, y) >]
               (fun instrs -> [Iarray(make_array_writer
                                        (function
                                           | [x] -> mk_fold_func x
                                           | l -> failwith
                                               ("invalid number of written values for an array: "
                                                ^ string_of_int (List.length l)))
                                        fold (flat instrs), y)]))
    | _ -> fail

  let mk_fold k v acc expr = (<:expr< fun $k$ $v$ $acc$ -> $expr$ >>)

  let rule_dict typ key_type val_type ?(mk_fold_func=mk_fold) fold = function
    | x, Tdict(k, v) ->
        match_types typ x
        >>= (fun sub ->
               let key_type = substitute sub key_type in
               let val_type = substitute sub val_type in
                 dep [< (key_type, k); (val_type, v) >]
                   (fun kis vis -> [Iarray(make_array_writer
                                             (function
                                                | [x; y] -> mk_fold_func x y
                                                | l -> failwith
                                                    ("invalid number of written values for an dict: "
                                                     ^ string_of_int (List.length l)))
                                             fold (Istructure :: flat kis @ flat vis),
                                           Tstructure [k; v])]))
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
