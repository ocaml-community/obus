(*
 * genSerializer.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types
open AbstractCode
open Optimize
open Helpers

type dbus_id = string

let dbyte = typ "byte" []
let dboolean = typ "boolean" []
let dint16 = typ "int16" []
let dint32 = typ "int32" []
let dint64 = typ "int64" []
let duint16 = typ "uint16" []
let duint32 = typ "uint32" []
let duint64 = typ "uint64" []
let ddouble = typ "double" []
let dstring = typ "string" []
let dsignature = typ "signature" []
let dobject_path = typ "object_path" []
let darray t = typ "array" [t]
let ddict k v = typ "dict" [k; v]
let dstructure l = typ "structure" [l]
let dvariant = typ "variant" []

let rec typ_of_dtype = function
  | Tbyte -> dbyte
  | Tboolean -> dboolean
  | Tint16 -> dint16
  | Tint32 -> dint32
  | Tint64 -> dint64
  | Tuint16 -> duint16
  | Tuint32 -> duint32
  | Tuint64 -> duint64
  | Tdouble -> ddouble
  | Tstring -> dstring
  | Tsignature -> dsignature
  | Tobject_path -> dobject_path
  | Tarray(t) -> darray (typ_of_dtype t)
  | Tdict(k, v) -> ddict (typ_of_dtype k) (typ_of_dtype v)
  | Tstructure(l) -> dstructure (typ_of_dbus_type l)
  | Tvariant -> dvariant
and typ_of_dbus_type l = tuple (List.map typ_of_dtype l)

(* Make a tuple reader from a list of reader *)
let rflat readers =
  let count = List.length readers in
    List.flatten readers
    @ match count with
      | 1 -> []
      | _ -> [Expr(false,
                   fun env next ->
                     bind
                       (Env.nth (count - 1) env)
                       (Ast.exCom_of_list
                          (List.map (fun x -> <:expr< $id:x$ >>) (Env.lasts count env)))
                       next);
              Update_env (Env.add (1 - count))]

(* Make a tuple writer from a list of writer *)
let wflat writers =
  let count = List.length writers in
    (match count with
       | 1 -> []
       | _ -> [Update_env (Env.add (count - 1));
               Expr(false,
                    fun env next ->
                      bind_patt
                        (Ast.paCom_of_list
                           (List.rev
                              (List.map (fun x -> <:patt< $id:x$ >>)
                                 (Env.lasts count env))))
                        (expr_of_id (Env.nth (count - 1) env))
                        next)])
    @ List.flatten writers

type env = (ident * expr) list
type rule = env ref -> code list Generate.rule * code list Generate.rule

let rule caml_patt dbus_patt deps_seq deps_rest reader writer =
  (Generate.rule caml_patt dbus_patt deps_seq deps_rest reader,
   Generate.rule caml_patt dbus_patt deps_seq deps_rest writer)

let len_id = (<:ident< len >>)
let typ_id = (<:ident< typ >>)
let idx = (<:expr< i >>)

(* Serialization of integers/booleans *)
let simple_serializer caml_type dbus_type len _ =
  rule (typ caml_type []) (typ dbus_type []) Seq.nil []
    (fun _ ->
       [[Align len;
         Update_env (Env.add 1);
         Expr(true, fun env next ->
                bind
                  (Env.last env)
                  (CodeConstants.fixed_reader caml_type dbus_type idx)
                  next);
         Advance_fixed(len, false)]])
    (fun _ ->
       [[Align len;
         Expr(true,
              fun env next ->
                seq
                  [CodeConstants.fixed_writer caml_type dbus_type idx (expr_of_id (Env.last env));
                   next]);
         Update_env (Env.add (-1));
         Advance_fixed(len, false)]])

type string_type =
  | Str_big
  | Str_small

let dbus_type_for_len = function
  | Str_big -> "uint32"
  | Str_small -> "byte"

let len_reader string_type idx = CodeConstants.fixed_reader "int" (dbus_type_for_len string_type) idx
let len_writer string_type idx len = CodeConstants.fixed_writer "int" (dbus_type_for_len string_type) idx len

let string_reader string_type =
  let len_size = match string_type with
    | Str_big -> 4
    | Str_small -> 1 in
    [Align len_size;
     Expr(true, fun _ -> bind len_id (len_reader string_type idx));
     Advance_fixed(len_size, false);
     Check_size_dynamic 1;
     Update_env (Env.add 1);
     Expr(true,
          fun env next ->
            bind
              (Env.last env)
              (<:expr< String.create $id:len_id$ >>)
              (seq [CodeConstants.string_reader idx (expr_of_id (Env.last env)) (expr_of_id len_id);
                    next]));
     Reset_padding(0, 1);
     Advance_dynamic 1]

let string_writer string_type =
  let len_size = match string_type with
    | Str_big -> 4
    | Str_small -> 1 in
    [Expr(false,
          fun env -> bind len_id <:expr< String.length $id:Env.last env$ >>);
     Align len_size;
     Expr(true,
          fun _ next -> seq [len_writer string_type idx (expr_of_id len_id); next]);
     Advance_fixed(len_size, false);
     Check_size_dynamic 1;
     Expr(true,
          fun env next ->
            seq [CodeConstants.string_writer idx (expr_of_id (Env.last env)) (expr_of_id len_id);
                 next]);
     Update_env (Env.add (-1));
     Reset_padding(0, 1);
     Advance_dynamic 1]

(* Serialization of string and object path *)
let string_serializer caml_type dbus_type string_type _ =
  rule caml_type dbus_type [<>] []
    (fun _ -> [string_reader string_type])
    (fun _ -> [string_writer string_type])

let id_for_expr expr env =
  match Util.find_map (fun (id, e) -> if expr = e then Some id else None) !env with
    | None ->
        let id = <:ident< $lid:"__intern_" ^ string_of_int (List.length !env)$ >> in
          env := (id, expr) :: !env;
          id
    | Some id -> id

let rule_alias ta tb _ =
  rule ta (v"x") [< (tb, v"x") >] []
    (fun b_reader _ -> [rflat b_reader])
    (fun b_writer _ -> [wflat b_writer])

let rule_convert ta tb a_of_b b_of_a _ =
  rule ta (v"x") [< (tb, v"x") >] []
    (fun b_reader _ ->
       [rflat b_reader
        @ [Expr(false,
                fun env ->
                  bind
                    (Env.last env)
                    (app a_of_b (expr_of_id (Env.last env))))]])
    (fun b_writer _ ->
       [Expr(false,
             fun env ->
               bind
                 (Env.last env)
                 (app b_of_a (expr_of_id (Env.last env))))
        :: wflat b_writer])

let rule_constant fake_type typ value error _ =
  rule fake_type (v"x") [< (typ, v"x") >] []
    (fun reader _ ->
       [rflat reader
        @ [Expr(false,
                fun env next ->
                  <:expr<
                    if $id:Env.last env$ <> $value$
                    then $app error (expr_of_id (Env.last env))$
                    else $next$ >>);
           Update_env (Env.add (-1))]])
    (fun writer _ ->
       [Update_env (Env.add 1)
        :: Expr(false,
                fun env ->
                  bind
                    (Env.last env)
                    value)
        :: wflat writer])

let padding instrs =
  match Util.find_map (function
                         | Align n -> Some n
                         | _ -> None) instrs with
    | Some n -> n
    | None -> failwith "alignement information missing!"

let array_reader instrs reverse empty add env =
  let padding = padding instrs in
  let opt1 = optimize 0 padding instrs in
  let opt2 = optimize opt1.opt_relative_position opt1.opt_alignment instrs in
    assert (opt1.opt_relative_position = opt2.opt_relative_position
        && opt1.opt_alignment = opt2.opt_alignment
        && ((opt1.opt_size = None) = (opt2.opt_size = None)));
    let init_instrs = match opt1.opt_size with
      | None -> opt1.opt_initial_check @ opt1.opt_code
      | Some _ -> opt1.opt_code
    and loop_instrs = match opt2.opt_size with
      | None -> opt2.opt_initial_check @ opt2.opt_code
      | Some _ -> opt2.opt_code in
    let id = id_for_expr
      (match reverse with
         | false ->
             (<:expr<
                fun i limit ->
                  let rec aux i acc =
                    if i = limit
                    then acc
                    else begin
                      $GenCode.generate_reader true Env.empty loop_instrs
                        (fun env -> <:expr< aux i $add <:expr< acc >>$ >>)$
                    end in
                    if i = limit
                    then $empty$
                    else begin
                      $GenCode.generate_reader true Env.empty init_instrs
                        (fun env -> <:expr< aux i $add empty$ >>)$
                    end
                      >>)
         | true ->
             (<:expr<
                fun i limit ->
                  let rec aux i =
                    if i = limit
                    then $empty$
                    else begin
                      $GenCode.generate_reader true Env.empty loop_instrs
                        (fun env ->
                           <:expr<
                             let acc = aux i in
                               $add <:expr< acc >>$
                               >>)$
                    end in
                    if i = limit
                    then $empty$
                    else begin
                      $GenCode.generate_reader true Env.empty init_instrs
                        (fun env ->
                           <:expr<
                             let acc = aux i in
                               $add empty$
                               >>)$
                    end
                      >>)) env in
      [Align 4;
       Expr(true,
            fun env next ->
              bind
                len_id
                (CodeConstants.fixed_reader "int" "uint32" idx)
                (<:expr<
                   if len > $expr_of_int Constant.max_array_size$
                   then raise Read_error "array too big!";
                 $next$
                 >>));
       Advance_fixed(4, false);
       Check_size_dynamic 0;
       (match opt1.opt_size, opt2.opt_size with
          | Some a, Some b ->
              Check_array_size(b - a, b)
          | _ -> Nothing);
       Align padding;
       Update_env (Env.add 1);
       Expr(true,
            fun env ->
              bind
                (Env.last env)
                (<:expr< $id:id$ i (i + len) >>));
       (match opt2.opt_size with
          | Some n when n mod padding = 0 -> Reset_padding(0, padding)
          | Some n when n land 1 = 1 -> Reset_padding(0, 1)
          | Some n when n land 3 = 2 -> Reset_padding(0, 2)
          | Some n when n land 7 = 4 -> Reset_padding(0, 4)
          | _ -> Reset_padding(0, 1))]

let array_writer instrs fold make_func nbval env =
  let padding = padding instrs in
  let opt = Optimize.optimize 0 padding instrs in
  let instrs =
    if opt.opt_relative_position = 0 && opt.opt_alignment >= padding
    then opt.opt_initial_check @ opt.opt_code
    else
      let opt = optimize 0 1 (Align padding :: instrs) in
       opt.opt_initial_check @ opt.opt_code
  in
  let id = id_for_expr
    (make_func (<:patt< (buffer, i) >>)
       (<:expr<
          $ (GenCode.generate_writer true (Env.init nbval) instrs
               (fun env -> <:expr< (buffer, i) >>)) $
        >>)) env in
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
              let (buffer, i) = $fold (expr_of_id id) <:expr< (buffer, i) >> (expr_of_id (Env.last env))$ in
              let len = k - i in
                if len > $expr_of_int Constant.max_array_size$
                then raise Writing.Array_too_big
                else begin
                  $CodeConstants.fixed_writer "int" "uint32" <:expr< j >> <:expr< len >>$;
                  $next$
                end
                >>);
     Update_env (Env.add (-1));
     (match opt.opt_size with
        | Some n when n mod padding = 0 -> Reset_padding(0, padding)
        | Some n when n land 1 = 1 -> Reset_padding(0, 1)
        | Some n when n land 3 = 2 -> Reset_padding(0, 2)
        | Some n when n land 7 = 4 -> Reset_padding(0, 4)
        | _ -> Reset_padding(0, 1))]

let rule_array typ elt_type reverse empty add fold make_func env =
  rule typ (darray (v"x")) [< (elt_type, v"x") >] []
    (fun elt_reader _ -> [array_reader (rflat elt_reader) reverse empty (add <:expr< v0 >>) env])
    (fun elt_writer _ -> [array_writer (wflat elt_writer) fold (make_func <:patt< v0 >>) 1 env])

let rule_dict typ key_type val_type reverse empty add fold make_func env =
  rule typ (ddict (v"k") (v"v")) [< (key_type, v"k"); (val_type, v"v") >] []
    (fun key_reader val_reader _ ->
       [array_reader (Align 8 :: rflat key_reader @ rflat val_reader)
          reverse empty (add <:expr< v0 >> <:expr< v1 >>) env])
    (fun key_writer val_writer _ ->
       [array_writer (Align 8 :: wflat key_writer @ wflat val_writer)
          fold (make_func <:patt< v0 >> <:patt< v1 >>) 2 env])

let rule_set module_name elt_type =
  let id = (<:ident< $lid:module_name$ >>) in
    rule_array (typ (module_name ^ ".t") []) elt_type false
      (<:expr< $id:id$ . empty >>)
      (fun x acc -> <:expr< $id:id$ . add $x$ $acc$ >>)
      (fun f l x -> <:expr< $id:id$ . fold $f$ $l$ $x$ >>)
      (fun x i e -> <:expr< fun $x$ $i$ -> $e$ >>)

let rule_map module_name key_type =
  let id = (<:ident< $lid:module_name$ >>) in
    rule_dict (typ (module_name ^ ".t") [v"x"]) key_type (v"x") false
      (<:expr< $id:id$ . empty >>)
      (fun k v acc -> <:expr< $id:id$ . add $k$ $v$ $acc$ >>)
      (fun f l x -> <:expr< $id:id$ . fold $f$ $l$ $x$ >>)
      (fun k v i e -> <:expr< fun $k$ $v$ $i$ -> $e$ >>)

type record_field =
  | F of string
  | Fake of expr * expr

let rule_record typ fields _ =
  let vars = Util.gen_names "field" fields in
  let names = List.map fst fields in
  let reals = Util.filter_map (function
                                 | F(name) -> Some(name)
                                 | Fake _ -> None) names in
  let count = List.length reals in
    rule typ (tuple (List.map v vars)) [<>] (List.map2 (fun (_, t) x -> (t, v x)) fields vars)
      (fun readers ->
         let rec flat names readers = match names, readers with
           | [], readers -> List.flatten readers
           | F n :: names, reader :: readers ->
               reader @ flat names readers
           | Fake(value, error) :: names, reader :: readers ->
               reader
               @ [Expr(false,
                       fun env next ->
                         <:expr<
                           if $id:Env.last env$ <> $value$
                           then $app error (expr_of_id (Env.last env))$
                           else $next$ >>);
                  Update_env (Env.add (-1))]
               @ flat names readers
           | _ -> assert false in
           [flat names (List.map rflat readers)
            @ [Expr(false,
                    fun env ->
                      bind
                        (Env.nth (count - 1) env)
                        (expr_record
                           (List.map2
                              (fun name id -> (ident_of_string name, expr_of_id id))
                              reals (Env.lasts count env))));
               Update_env (Env.add (1 - count))]])
      (fun writers ->
         let rec flat names writers = match names, writers with
           | [], writers -> List.flatten writers
           | F n :: names, writer :: writers ->
               writer @ flat names writers
           | Fake(value, error) :: names, writer :: writers ->
               Update_env (Env.add 1)
               :: Expr(false, fun env -> bind (Env.last env) value)
               :: writer @ flat names writers
           | _ -> assert false in
           [Update_env (Env.add (count - 1))
            :: Expr(false,
                    fun env ->
                      bind_patt
                        (patt_record
                           (List.map2
                              (fun name id -> (ident_of_string name, patt_of_id id))
                              reals (Env.lasts count env)))
                        (expr_of_id (Env.nth (count - 1) env)))
            :: flat names (List.map wflat writers)])

let sig_matcher dbust =
  let dbus_sig = signature_of_dbus_type dbust in
    [Expr(true,
          fun env next ->
            seq
              [CodeConstants.signature_checker dbus_sig idx;
               next]);
     Advance_fixed(String.length dbus_sig + 2, false)]

let sig_writer dbust =
  let dbus_sig = signature_of_dbus_type dbust in
  let total_sig = Printf.sprintf "%c%s\x00" (char_of_int (String.length dbus_sig)) dbus_sig in
  let len = String.length total_sig in
    [Expr(true,
          fun _ next ->
            seq
              [CodeConstants.string_writer idx
                 (expr_of_str total_sig)
                 (expr_of_int len);
               next]);
     Advance_fixed(len, false)]

let rule_variant typ key_type variants env =
  rule typ (tuple [v"x"; dvariant]) [< (key_type, v"x") >]
    (List.map (fun (_, _, _, cts, dt) -> (tuple cts, typ_of_dbus_type dt)) variants)
    (fun key_reader readers ->
       [rflat key_reader
        @ [Branches((fun env -> <:expr< $id:Env.last env$ >>),
                    List.map2
                      (fun (patt, _, name, camlts, dbust) reader ->
                         ((fun env -> patt),
                          Update_env (Env.add (-1))
                          :: sig_matcher dbust
                          @ List.flatten reader,
                          (fun env ->
                             List.fold_left
                               (fun acc x -> Ast.ExApp(_loc, acc, <:expr< $id:x$ >>))
                               (<:expr< $idexpr_of_string name$ >>)
                               (Env.lasts (List.length camlts) env))))
                      variants readers);
           Reset_padding(0, 1)]])
    (fun key_writer writers ->
       [[Branches((fun env -> <:expr< $id:Env.last env$ >>),
                  List.map2
                    (fun (_, expr, name, camlts, dbust) writer ->
                       ((fun env ->
                           List.fold_left
                             (fun acc x -> Ast.PaApp(_loc, acc, x))
                             (<:patt< $idpatt_of_string name$ >>)
                             (List.rev (List.map (fun x -> <:patt< $id:x$ >>) (Env.slice 0 (List.length camlts) env)))),
                        Update_env (Env.add (List.length camlts + 1))
                        :: Expr(false, fun env -> bind (Env.last env) expr)
                        :: wflat key_writer
                        @ sig_writer dbust
                        @ List.flatten writer,
                        (fun env -> <:expr< >>)))
                    variants writers);
         Reset_padding(0, 1)]])

let rule_record_option typ key_type fields env =
  rule typ (darray (dstructure (tuple [v"x"; dvariant]))) [< (key_type, v"x") >]
    (List.map (fun (_, _, _, ct, dt) -> (tuple [ct], typ_of_dbus_type dt)) fields)
    (fun key_reader readers ->
       [array_reader
          (Align 8
           :: rflat key_reader
           @ [Branches((fun env -> <:expr< $id:Env.last env$ >>),
                       List.map2
                         (fun (patt, _, name, _, dbust) reader ->
                            ((fun env -> patt),
                             Update_env (Env.add (-1))
                             :: sig_matcher dbust
                             @ rflat reader,
                             (fun env -> <:expr< { acc with $ident_of_string name$ = Some($id:Env.last env$) } >>)))
                         fields readers);
              Reset_padding(0, 1)])
          false
          (expr_record (List.map (fun (_, _, name, _, _) -> (ident_of_string name, <:expr< None >>)) fields))
          (fun acc -> acc)
          env])
    (fun key_writer writers ->
       [[Align 4;
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
                              :: Update_env (Env.add 2)
                              :: Expr(false,
                                      fun env next ->
                                        <:expr<
                                          let $id:Env.last env$ = $expr$ in
                                            $next$
                                            >>)
                              :: wflat key_writer
                              @ sig_writer dbust
                              @ wflat writer) in
                           let opt = optimize 0 1 instrs in
                           let instrs = opt.opt_initial_check @ opt.opt_code in
                             <:expr<
                               let (buffer, i) = match $id:Env.last env$.$ident_of_string name$ with
                                 | None -> (buffer, i)
                                 | Some($id:Env.nth (-1) env$) ->
                                     $GenCode.generate_writer true env instrs (fun env -> <:expr< (buffer, i) >>)$
                               in $acc$
                                  >>)
                        fields writers
                        (<:expr<
                           let len = k - i in
                             if len > $expr_of_int Constant.max_array_size$
                             then raise Writing.Array_too_big
                             else begin
                               $CodeConstants.fixed_writer "int" "uint32" <:expr< j >> <:expr< len >>$;
                               $next$
                             end >>))$ >>);
         Update_env (Env.add (-1));
         Reset_padding(0, 1)]])

(* Serialization of all basic types *)
let default_rules =
  [ (fun _ -> rule (v"x") (dstructure (v"x")) [< (v"x", v"x") >] []
       (fun reader _ -> [Align 8 :: rflat reader])
       (fun writer _ -> [Align 8 :: wflat writer]));
    (fun _ -> rule (cons (v"x") (v"y")) (cons (v"x") (v"y")) [< (v"x", v"x"); (v"y", v"y") >] []
       (fun x_reader y_reader _ -> x_reader @ y_reader)
       (fun x_writer y_writer _ -> x_writer @ y_writer));
    (fun _ -> rule nil nil  [<>] []
       (fun _ -> [])
       (fun _ -> []));
    simple_serializer "char" "byte" 1;
    simple_serializer "int" "byte" 1;
    simple_serializer "int" "boolean" 4;
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
                  | n -> raise Content_error ("invalid boolean value: " ^ string_of_int n)) >>)
      (<:expr< (function
                  | false -> 0
                  | true -> 1) >>);
    rule_array (list (v"x")) (v"x") true
      (<:expr< [] >>)
      (fun x acc -> <:expr< $x$ :: $acc$ >>)
      (fun f l x -> <:expr< List.fold_left $f$ $x$ $l$ >>)
      (fun x i e -> <:expr< fun $i$ $x$ -> $e$ >>);
    rule_dict (list (tuple [v"x"; v"y"])) (v"x") (v"y") false
      (<:expr< [] >>)
      (fun k v acc -> <:expr< ($k$, $v$) :: $acc$ >>)
      (fun f l x -> <:expr< List.fold_left $f$ $x$ $l$ >>)
      (fun k v i e -> <:expr< fun $i$ ($k$, $v$) -> $e$ >>);
    rule_dict (typ "Hashtbl.t" [v"x"; v"y"]) (v"x") (v"y") false
      (<:expr< Hashtbl.create 42 >>)
      (fun k v acc -> <:expr< Hashtbl.add $acc$ $k$ $v$; $acc$ >>)
      (fun f l x -> <:expr< Hashtbl.fold $f$ $l$ $x$ >>)
      (fun k v i e -> <:expr< fun $k$ $v$ $i$ -> $e$ >>) ]

let gen part trace rules camlt dbust env =
  let dbust = typ_of_dbus_type dbust in
  let env = ref env in
    match Generate.generate ~trace:trace (List.map (fun f -> part (f env)) rules) camlt dbust with
      | None -> failwith
          (Printf.sprintf
             "cannot find a convertion between this caml type: %s and this dbus type: %s"
             (string_of_type camlt)
             (string_of_type dbust))
      | Some x -> (!env, List.flatten x)

let gen_reader = gen fst
let gen_writer = gen snd
