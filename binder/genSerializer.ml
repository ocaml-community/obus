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
open AbstractCode

let _loc = Loc.ghost

(* Make a tuple reader from a list of reader *)
let rflat readers =
  let count = List.length readers in
    List.flatten readers
    @ [Expr(false,
            fun env next ->
              <:expr<
                let $Env.nth (count - 1) env$ = $Ast.exCom_of_list (Env.lasts count env)$ in
                  $next$ >>);
       Update_env (Env.add (1 - count))]

(* Make a tuple writer from a list of writer *)
let wflat writers =
  let count = List.length writers in
    Update_env (Env.add (count - 1))
    :: Expr(false,
            fun env next ->
              <:expr<
                let $Ast.paCom_of_list (List.rev (Env.lasts count env))$ = $Env.nth (count - 1) env$ in
                  $next$
                  >>)
    :: List.flatten writers

module RG = Generate.Make(struct type t = code let flat l = rflat end)
module WG = Generate.Make(struct type t = code let flat l = wflat end)

type env = (ident * expr) list
type rule = env ref -> (caml_id, dbus_id) RG.rule * (caml_id, dbus_id) WG.rule

let rule caml_patt dbus_patt deps_seq deps_rest reader writer =
  (RG.rule caml_patt dbus_patt deps_seq deps_rest reader,
   WG.rule caml_patt dbus_patt deps_seq deps_rest writer)

let len_id = (<:ident< len >>)
let typ_id = (<:ident< typ >>)

let simple_reader_expr id func next =
  (<:expr<
     let $id$ = $lid:func$ buffer i in
       $next$
       >>)

let uint8_reader_expr id next =
  (<:expr<
     let $id$ = Char.code (String.unsafe_get buffer i) in
       $next$
       >>)

let simple_writer_expr id func next =
  (<:expr< $lid:func$ buffer i $id$; $next$ >>)

let uint8_writer_expr id next =
  (<:expr< String.unsafe_set buffer i (Char.unsafe_chr $id$); $next$ >>)

let fixed_reader expr len =
  [Align len;
   Update_env (Env.add 1);
   Expr(true, expr);
   Advance_fixed(len, false)])

let fixed_writer expr len =
  [Align len;
   Expr(true, expr);
   Update_env (Env.add (-1));
   Advance_fixed(len, false)])

(* Serialization of integers/booleans *)
let simple_serializer caml_type dbus_type len _ =
  let func = caml_type ^ "_" ^ dbus_type in
    rule (typ caml_type []) (typ dbus_type []) Seq.nil []
      (fun _ -> fixed_reader (fun env next -> simple_reader_expr (Env.last env) func next) len)
      (fun _ -> fixed_zriter (fun env next -> simple_writer_expr (Env.last env) func next) len)

let string_type =
  | Str_big
  | Str_small

let string_reader string_type =
  let len_size = match string_type with
    | Str_big -> 4
    | Str_small -> 1 in
    [Align len_size;
     Expr(true,
          fun _ next ->
            match string_type with
              | Str_big -> simple_reader_expr len_id "int_uint32" next
              | Str_small -> uint8_reader_expr len_id next);
     Advance_fixed(len_size, false);
     Check(Chk_size_dynamic 1);
     Update_env (Env.add 1);
     Expr(true,
          fun env next ->
            <:expr<
              let $Env.last env$ = String.create $len_id$ in
                String.unsafe_blit buffer i $lid:var$ 0 $len_id$;
                $next$
                >>);
     Reset_padding(0, 1);
     Advance_dynamic 1])

let string_writer string_type =
  let len_size = match string_type with
    | Str_big -> 4
    | Str_small -> 1 in
    [Expr(false,
          fun env next ->
            <:expr<
              let $len_id$ = String.length $Env.last env$ in
                $next$
                >>);
     Align len_size;
     Expr(true,
          fun _ next ->
            match string_type with
              | Str_big -> simple_writer_expr len_id "int_uint32" next
              | Str_small -> uint8_writer_expr len_id next);
     Advance_fixed(len_size, false);
     Check(Chk_size_dynamic 1);
     Expr(true,
          fun env next ->
            <:expr<
              String.unsafe_blit $Env.last env$ 0 buffer i $len_id$;
            $next$
            >>);
     Update_env (Env.add (-1));
     Reset_padding(0, 1);
     Advance_dynamic 1])

(* Serialization of string and object path *)
let string_serializer caml_type dbus_type string_type _ =
  rule caml_type dbus_type
    (fun _ -> string_reader string_type)
    (fun _ -> string_writer string_type)

let id_for_expr expr env =
  match Util.find_map (fun (id, e) -> if expr = e then Some id else None) !env with
    | None ->
        let id = <:ident< $lid:"__intern_" ^ string_of_int (List.length !env)$ >> in
          env := (id, expr) :: !env;
          id
    | Some id -> id

let rule_alias ta tb _ =
  rule ta (v"x") [< (tb, v"x") >] []
    (fun b_reader _ -> rflat b_reader)
    (fun b_writer _ -> wflat b_reader)

let rule_convert ta tb a_of_b b_of_a _ =
  rule ta (v"x") [< (tb, v"x") >] []
    (fun b_reader _ ->
       rflat b_reader
       @ [Expr(false,
               fun env next ->
                 <:expr<
                   let $Env.last env$ = $a_of_b$ $Env.last env$ in
                     $next$
                     >>)])
    (fun b_writer _ ->
       Expr(false,
            fun env next ->
              <:expr<
                let $Env.last env$ = $b_of_a$ $Env.last env$ in
                  $next$ >>)
       :: wflat b_writer)

let padding intrs =
  match Util.find_map (function
                         | Align n -> Some n
                         | _ -> None) instrs with
    | Some n -> n
    | None -> failwith "alignement information missing!"

let array_reader instrs reverse empty add env =
  let padding = padding instrs in
  let (init_instrs, init_check_instrs, init_space, end_rel, end_pad) = Optimize.optimize 0 padding instrs in
  let (loop_instrs, loop_check_instrs, loop_space, end_rel', end_pad') = Optimize.optimize end_rel end_pad instrs in
    assert (end_rel = end_rel' && end_pad = end_pad' && ((init_space = None) = (loop_space = None)));
    let init_instrs = match init_space with
      | None -> init_check_instrs @ init_instrs
      | Some _ -> init_instrs
    and loop_instrs = match loop_space with
      | None -> loop_check_instrs @ loop_instrs
      | Some _ -> loop_instrs in
    let id = id_for_expr "reader"
      (match reverse with
         | false ->
             (<:expr<
                fun i limit ->
                  let rec aux i acc =
                    if i = limit
                    then acc
                    else begin
                      $GenCode.generate_reader loop_instrs true
                        (<:expr< aux i $add <:expr< acc >>$ >>)$
                    end in
                    if i = limit
                    then $empty$
                    else begin
                      $GenCode.generate_reader init_instrs true
                        (<:expr< aux i $add empty$ >>)$
                    end
                      >>)
         | true ->
             (<:expr<
                fun i limit ->
                  let rec aux i =
                    if i = limit
                    then $empty$
                    else begin
                      $GenCode.generate_reader loop_instrs true
                        (<:expr<
                           let acc = aux i in
                             $add <:expr< acc >>$
                             >>)$
                    end in
                    if i = limit
                    then $empty$
                    else begin
                      $GenCode.generate_reader init_instrs true
                        (<:expr<
                           let acc = $id$ i in
                             $add empty$
                             >>)$
                    end
                      >>)) env in
      [Align 4;
       Expr(true,
            fun env next ->
              simple_reader_expr len_id "int_uint32"
                (<:expr<
                   if len > $int:Constant.max_array_size$
                   then raise Read_error "array too big!";
                 $next$
                 >>));
       Advance_fixed(4, false);
       Check (Chk_size_dynamic 0);
       (match init_space, loop_space with
          | Some a, Some b ->
              Check(Chk_array_size(b - a, b))
          | _ -> Nothing);
       Align padding;
       Update_env (Env.add 1);
       Expr(true,
            fun env next ->
              <:expr<
                let $Env.last env$ = $id$ i (i + len) in
                  $next$
                  >>);
       (match init_space with
          | Some n when n mod padding = 0 -> Reset_padding(0, padding)
          | Some n when n land 1 = 1 -> Reset_padding(0, 1)
          | Some n when n land 3 = 2 -> Reset_padding(0, 2)
          | Some n when n land 7 = 4 -> Reset_padding(0, 4)
          | _ -> Reset_padding(0, 1))])

let array_writer instrs fold make_func env =
  let padding = padding instrs in
  let (instrs, check_instrs, space, end_rel, end_pad) = Optimize.optimize 0 padding instrs in
  let instrs =
    if end_rel = 0 && end_pad >= padding
    then check_instrs @ instrs
    else
      let (instrs, check_instrs, _, _, _) = Optimize.optimize 0 1 (Align padding :: instrs) in
        check_instrs @ instrs
  in
  let id = id_for_expr "writer"
    (make_func (<:ident< i >>)
       (<:expr<
          $GenCode.generate_writer instrs true$
        >>)) in
    [Align 4;
     Expr(true,
          fun env next ->
            <:expr<
              let j = i in
                $next$
                >>);
     Advance_fixed(4, false);
     Align padding;
     Expr(true,
          fun env next ->
            <:expr<
              let k = i in
                $fold <:expr< id >> <:expr< i >> <:expr< $Env.last env$ >>$;
                int_uint32 j (i - k);
                $next$
            >>);
     Update_env (Env.add (-1));
     (match space with
        | Some n when n mod padding = 0 -> Reset_padding(0, padding)
        | Some n when n land 1 = 1 -> Reset_padding(0, 1)
        | Some n when n land 3 = 2 -> Reset_padding(0, 2)
        | Some n when n land 7 = 4 -> Reset_padding(0, 4)
        | _ -> Reset_padding(0, 1))])

let rule_array typ elt_type reverse empty add fold make_func env =
  rule typ (darray (v"x")) [< (elt_type, v"x") >] []
    (fun elt_reader _ -> array_reader (rflat elt_reader) reverse empty (add <:expr< v0 >>))
    (fun elt_writer _ -> array_writer (wflat elt_writer) fold (make_func <:ident< v0 >>))

let rule_dict typ key_type val_type reverse empty add fold make_func env =
  rule typ (ddict (v"k") (v"v")) [< (key_type, v"k"); (val_type, v"v") >] []
    (fun key_reader val_reader _ ->
       array_reader (Align 8 :: rflat key_reader @ rflat val_reader)
         reverse empty (add <:expr< v0 >> <:expr< v1 >>))
    (fun key_writer val_writer _ ->
       array_writer (Align 8 :: wflat key_writer @ wflat val_writer)
         fold (make_func <:ident< v0 >> <:ident< v1 >>))

let rule_set module_name elt_type _ =
  let id = (<:ident< $lid:module_name$ >>) in
    rule_array (typ (module_name ^ ".t")) elt_type false
      (<:expr< $id$ . empty >>)
      (fun x acc -> <:expr< $id$ . add $x$ $acc$ >>)
      (fun f l x -> <:expr< $id$ . fold $f$ $l$ $x$ >>)
      (fun x i e -> <:expr< fun $x$ $i$ -> $e$)

let rule_map module_name key_type _ =
  let id = (<:ident< $lid:module_name$ >>) in
    rule_dict (typ (module_name ^ ".t") (v"x")) key_type (v"x") false
      (<:expr< $id$ . empty >>)
      (fun k v acc -> <:expr< $id$ . add $k$ $v$ $acc$ >>)
      (fun f l x -> <:expr< $id$ . fold $f$ $l$ $x$ >>)
      (fun k v i e -> <:expr< fun $k$ $v$ $i$ -> $e$)

let rule_record typ fields _ =
  let count = List.length fields in
  let vars = gen_names "field" fields in
    rule typ (tuple vars) [<>] (List.map2 (fun (_, t) x -> (t, x)) fields vars)
      (fun readers ->
         List.flatten readers
         @ [Expr(false,
                 fun env next ->
                   let $Env.nth (count - 1)$ =
                     { $Ast.rbSem_of_list
                         (List.map2 (fun (name, _) id -> <:rec_binding< $lid:name$ = $id$ >>)
                            fields $Env.lasts count$) $ } in
                     $next$);
            Update_env (Env.add (1 - count))])
      (fun writers ->
         Update_env (Env.add (count - 1))
         :: Expr(false,
                 fun env next ->
                   let { $Ast.paSem_of_list
                           (List.map2 (fun (name, _) id -> <:patt< $lid:name$ = $id$ >>)
                              fields $Env.lasts count$) $ } = $Env.nth (count - 1) in
                     $next$)
         :: List.flatten writers)

let sig_matcher dbust =
  let dbus_sig = signature_of_dtypes dbust in
  let total_sig = Printf.sprintf "%c%s\x00" (char_of_int (String.length dbus_sig)) dbus_sig in
  let len = String.length total_sig in
    [Expr(true,
          fun env next ->
            <:expr<
              if not string_match buffer i $str:total_sig$ $int:len$
              then raise Data_error "unexpected variant signature"
                $next$
                >>);
     Advance_fixed(len, false)]

let sig_writer dbust =
  let dbus_sig = signature_of_dtypes dbust in
  let total_sig = Printf.sprintf "%c%s\x00" (char_of_int (String.length dbus_sig)) dbus_sig in
  let len = String.length total_sig in
    [Expr(fun _ next ->
            <:expr<
              String.blit $str:total_sig$ 0 buffer i $int:len$
            >>);
     Advance_fixed(len, false)]

let rule_variant typ key_type variants env =
  let count = List.length fields in
    rule typ (tuple [v"x"; dvariant]) [< (key_type, v"x") >]
      (List.map (fun (_, _, _, cts, dt) -> (tuple cts, dbus_type_of_dtypes dt)) variants)
      (fun key_reader readers ->
         key_reader
         @ [Branches(fun env ->
                       (<:expr< $Env.last env$ >>,
                        List.map2
                          (fun (patt, _, name, camlts, dbust) reader ->
                             (patt,
                              Update_env (Env.add (-1))
                              :: sig_matcher dbust
                              @ List.flatten reader,
                              List.fold_left (fun acc x -> Ast.ExApp(_loc, acc, x))
                                (<:expr< $uid:name$ >>)
                                (Env.lasts (List.length camlts) env)))
                          variants readers));
            Reset_padding(0, 1)])
      (fun key_writer writers ->
         let count = (List.fold_left
                        (fun acc (_, _, _, ts, _) -> max acc (List.length ts))
                        0 variants) in
           [Update_env (Env.add count);
            Branches(fun env ->
                       (<:expr< $Env.nth count env$ >>,
                        List.map2
                          (fun (_, expr, name, camlts, dbust) writer ->
                             (List.fold_left (fun acc x -> Ast.PaApp(_loc, acc, x))
                                (<:patt< $uid:name$ >>)
                                (List.rev (Env.lasts (List.length camlts) env)),
                              Update_env (Env.add 1)
                              :: Expr(fun env enxt ->
                                        <:expr<
                                          let $Env.last env$ = $expr$ in
                                            $next$
                                            >>)
                              :: key_writer
                              @ sig_writer dbust
                              @ List.flatten writer,
                              <:expr< >>))
                          variants writers));
            Reset_padding(0, 1)])

let rule_record_option typ key_type fields env =
  let count = List.length fields in
    rule typ (darray (dstructure [v"x"; dvariant])) [< (key_type, v"x") >]
      (List.map (fun (_, _, _, ct, dt) -> (ct, dbus_type_of_dtypes dt)) fields)
      (fun key_reader readers ->
         (*instrs reverse empty add env *)
         array_reader
           (Align 8
            :: key_reader
            @ [Branches(fun env ->
                          (<:expr< $Env.last env$ >>,
                           List.map2
                             (fun (patt, _, name, _, dbust) reader ->
                                (patt,
                                 Update_env (Env.add (-1))
                                 :: sig_matcher dbust
                                 @ rflat reader,
                                 <:expr< { acc with $lid:name$ = Some(v0) } >>))
                             fields readers));
               Reset_padding(0, 1)])
           false
           (<:expr< $Ast.rbSem_of_list
              (List.map (fun (_, _, name, _, _) -> <:rec_binding< $lid:name$ = None >>) fields)$
            >>)
           (fun acc -> acc))
      (fun key_writer writers ->
         [Align 4;
          Expr(true,
               fun env next ->
                 <:expr<
                   let j = i in
                     $next$
                     >>);
          Advance_fixed(4, false);
          Align 8;
          Expr(true,
               fun env next ->
                 <:expr<
                   let k = i in
                     $(List.fold_right2
                         (fun (_, expr, name, _, dbust) writer acc ->
                            let instrs =
                              (Align 8
                               :: Update_env (Env.add 1)
                               :: Expr(fun env enxt ->
                                         <:expr<
                                           let $Env.last env$ = $expr$ in
                                             $next$
                                             >>)
                               :: key_writer
                               @ sig_writer dbust
                               @ wflat writer) in
                            let (instrs, check_instrs, _, _, _) = Optimize.optimize 0 8 instrs in
                            let instrs = check_instrs @ instrs in
                              <:expr<
                                let i = match $Env.last env$.$lid:name$ with
                                  | None -> i
                                  | Some($Env.last (-1)$) ->
                                      $GenCode.generate_writer instrs true <:expr< i >>$
                                in $acc$
                                   >>)
                         fields writers
                         (<:expr<
                            int_uint32 j (i - k);
                          $next$ >>))$);
          Update_env (Env.add (-1));
          Reset_padding(0, 1)])

(* Serialization of all basic types *)
let default_rules =
  [ (fun _ -> rule (v"x") (dstructure (v"x")) [< (v"x", v"x") >] []
       (fun reader _ -> Align 8 :: rflat reader)
       (fun writer _ -> Align 8 :: wflat writer));
    (fun _ -> rule char dbyte Seq.nil []
       (fun _ -> fixed_reader (fun env next -> uint8_reader_expr (Env.last env) next) 1)
       (fun _ -> fixed_writer (fun env next -> uint8_writer_expr (Env.last env) next) 1));
    (fun _ -> rule int dbool Seq.nil []
      (fun _ -> fixed_reader (fun env next -> simple_reader_expr (Env.last env) "int_uint32" next) len)
      (fun _ -> fixed_zriter (fun env next -> simple_writer_expr (Env.last env) "int_uint32" next) len));
    simple_serializer "int" "int16" 2;
    simple_serializer "int" "uint16" 2;
    simple_serializer "int" "int32" 4;
    simple_serializer "int" "uint32" 4;
    simple_serializer "int32" "int32" 4;
    simple_serializer "int32" "uint32" 4;
    simple_serializer "int" "int64" 8;
    simple_serializer "int" "uint64" 8;
    simple_serializer "int64" "int64" 8;
    simple_serializer "int64" "uint64" 8;
    simple_serializer "float" "double" 8;
    string_serializer string dstring Str_big;
    string_serializer string dobject_path Str_big;
    string_serializer string dsignature Str_small;
    rule_convert int char
      (<:expr< int_of_char >>) (<:expr< char_of_int >>);
    rule_convert bool int
      (<:expr< (function
                  | 0 -> false
                  | 1 -> true
                  | n -> raise Data_error ("invalid boolean value: " ^ string_of_int n)) >>)
      (<:expr< (function
                  | false -> 0
                  | true -> 1) >>);
    rule_array (list (v"x")) (v"x") true
      (<:expr< [] >>)
      (fun x acc -> <:expr< $x$ :: $acc$ >>)
      (fun f l x -> <:expr< List.fold_left $f$ $x$ $l$ >>)
      (fun x i e -> <:expr< fun $i$ $x$ -> $e$ >>);
    rule_dict (list (tuple (v"x") (v"y"))) (v"x") (v"y") false
      (<:expr< [] >>)
      (fun k v acc -> <:expr< ($k$, $v$) :: $acc$ >>)
      (fun f l x -> <:expr< List.fold_left $f$ $x$ $l$ >>)
      (fun k v i e -> <:expr< fun $i$ ($k$, $v$) -> $e$ >>);
    rule_dict (typ "Hashtbl.t" (tuple (v"x") (v"y"))) (v"x") (v"y") false
      (<:expr< Hashtbl.create 42 >>)
      (fun k v acc -> <:expr< Hashtbl.add $acc$ $k$ $v$; $acc$ >>)
      (fun f l x -> <:expr< Hashtbl.fold $f$ $l$ $x$ >>)
      (fun k v i e -> <:expr< fun $k$ $v$ $i$ -> $e$ >>) ]

let gen part rules camlt dbust env =
  let env = ref env in
    match RG.generate (List.map (fun f -> part (f env)) rules) camlt dbust with
      | None -> assert false
      | Some x -> (x, !env)

let gen_reader rules camlt dbust env =
  gen fst rules camlt dbust env

let gen_writer rules camlt dbust env =
  gen snd rules camlt dbust env
