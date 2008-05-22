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
open Optimize

let _loc = Loc.ghost

let dot_regexp = Str.regexp "\\."

let ident_of_string name =
  Ast.idAcc_of_list
    (List.map
       (fun id ->
          if id <> "" && Char.uppercase id.[0] <> id.[0]
          then <:ident< $lid:id$ >>
          else <:ident< $uid:id$ >>)
       (Str.split dot_regexp name))

let idexpr_of_string name =
  if name.[0] = '`'
  then Ast.ExVrn(_loc, name)
  else (<:expr< $id:ident_of_string name$ >>)

let idpatt_of_string name =
  if name.[0] = '`'
  then Ast.PaVrn(_loc, name)
  else (<:patt< $id:ident_of_string name$ >>)

(* Make a tuple reader from a list of reader *)
let rflat readers =
  let count = List.length readers in
    List.flatten readers
    @ match count with
      | 1 -> []
      | _ -> [Expr(false,
                   fun env next ->
                     <:expr<
                       let $id:Env.nth (count - 1) env$ =
                         $ Ast.exCom_of_list
                           (List.map (fun x -> <:expr< $id:x$ >>) (Env.lasts count env)) $ in
                         $next$ >>);
              Update_env (Env.add (1 - count))]

(* Make a tuple writer from a list of writer *)
let wflat writers =
  let count = List.length writers in
    (match count with
       | 1 -> []
       | _ -> [Update_env (Env.add (count - 1));
               Expr(false,
                    fun env next ->
                      <:expr<
                        let $Ast.paCom_of_list
                            (List.rev
                               (List.map (fun x -> <:patt< $id:x$ >>)
                                  (Env.lasts count env)))$ = $id:Env.nth (count - 1) env$ in
                          $next$
                          >>)])
    @ List.flatten writers

module RG = Generate.Make(struct type t = code let flat = rflat end)
module WG = Generate.Make(struct type t = code let flat = wflat end)

type env = (ident * expr) list
type rule = env ref -> (caml_id, dbus_id) RG.rule * (caml_id, dbus_id) WG.rule

let rule caml_patt dbus_patt deps_seq deps_rest reader writer =
  (RG.rule caml_patt dbus_patt deps_seq deps_rest reader,
   WG.rule caml_patt dbus_patt deps_seq deps_rest writer)

let len_id = (<:ident< len >>)
let typ_id = (<:ident< typ >>)

let simple_reader_expr id func next =
  (<:expr<
     let $id:id$ = $lid:func$ buffer i in
       $next$
       >>)

let uint8_reader_expr id next =
  (<:expr<
     let $id:id$ = Char.code (String.unsafe_get buffer i) in
       $next$
       >>)

let simple_writer_expr id func next =
  (<:expr< $lid:func$ buffer i $id:id$; $next$ >>)

let uint8_writer_expr id next =
  (<:expr< String.unsafe_set buffer i (Char.unsafe_chr $id:id$); $next$ >>)

let fixed_reader expr len =
  [Align len;
   Update_env (Env.add 1);
   Expr(true, expr);
   Advance_fixed(len, false)]

let fixed_writer expr len =
  [Align len;
   Expr(true, expr);
   Update_env (Env.add (-1));
   Advance_fixed(len, false)]

(* Serialization of integers/booleans *)
let simple_serializer func caml_type dbus_type len _ =
  rule caml_type dbus_type Seq.nil []
    (fun _ -> [fixed_reader (fun env next -> simple_reader_expr (Env.last env) func next) len])
    (fun _ -> [fixed_writer (fun env next -> simple_writer_expr (Env.last env) func next) len])

type string_type =
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
     Check_size_dynamic 1;
     Update_env (Env.add 1);
     Expr(true,
          fun env next ->
            <:expr<
              let $id:Env.last env$ = String.create $id:len_id$ in
                String.unsafe_blit buffer i $id:Env.last env$ 0 $id:len_id$;
                $next$
                >>);
     Reset_padding(0, 1);
     Advance_dynamic 1]

let string_writer string_type =
  let len_size = match string_type with
    | Str_big -> 4
    | Str_small -> 1 in
    [Expr(false,
          fun env next ->
            <:expr<
              let $id:len_id$ = String.length $id:Env.last env$ in
                $next$
                >>);
     Align len_size;
     Expr(true,
          fun _ next ->
            match string_type with
              | Str_big -> simple_writer_expr len_id "int_uint32" next
              | Str_small -> uint8_writer_expr len_id next);
     Advance_fixed(len_size, false);
     Check_size_dynamic 1;
     Expr(true,
          fun env next ->
            <:expr<
              String.unsafe_blit $id:Env.last env$ 0 buffer i $id:len_id$;
            $next$
            >>);
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
                fun env next ->
                  <:expr<
                    let $id:Env.last env$ = $a_of_b$ $id:Env.last env$ in
                      $next$
                      >>)]])
    (fun b_writer _ ->
       [Expr(false,
             fun env next ->
               <:expr<
                 let $id:Env.last env$ = $b_of_a$ $id:Env.last env$ in
                   $next$ >>)
        :: wflat b_writer])

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
              simple_reader_expr len_id "int_uint32"
                (<:expr<
                   if len > $int:string_of_int Constant.max_array_size$
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
            fun env next ->
              <:expr<
                let $id:Env.last env$ = $id:id$ i (i + len) in
                  $next$
                  >>);
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
    (make_func (<:ident< i >>)
       (<:expr<
          $ (GenCode.generate_writer true (Env.init nbval) instrs
               (fun env -> <:expr< i >>)) $
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
                $fold <:expr< $id:id$ >> <:expr< i >> <:expr< $id:Env.last env$ >>$;
                int_uint32 j (i - k);
                $next$
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
    (fun elt_writer _ -> [array_writer (wflat elt_writer) fold (make_func <:ident< v0 >>) 1 env])

let rule_dict typ key_type val_type reverse empty add fold make_func env =
  rule typ (ddict (v"k") (v"v")) [< (key_type, v"k"); (val_type, v"v") >] []
    (fun key_reader val_reader _ ->
       [array_reader (Align 8 :: rflat key_reader @ rflat val_reader)
          reverse empty (add <:expr< v0 >> <:expr< v1 >>) env])
    (fun key_writer val_writer _ ->
       [array_writer (Align 8 :: wflat key_writer @ wflat val_writer)
          fold (make_func <:ident< v0 >> <:ident< v1 >>) 2 env])

let rule_set module_name elt_type =
  let id = (<:ident< $lid:module_name$ >>) in
    rule_array (typ (module_name ^ ".t") []) elt_type false
      (<:expr< $id:id$ . empty >>)
      (fun x acc -> <:expr< $id:id$ . add $x$ $acc$ >>)
      (fun f l x -> <:expr< $id:id$ . fold $f$ $l$ $x$ >>)
      (fun x i e -> <:expr< fun $id:x$ $id:i$ -> $e$ >>)

let rule_map module_name key_type =
  let id = (<:ident< $lid:module_name$ >>) in
    rule_dict (typ (module_name ^ ".t") [v"x"]) key_type (v"x") false
      (<:expr< $id:id$ . empty >>)
      (fun k v acc -> <:expr< $id:id$ . add $k$ $v$ $acc$ >>)
      (fun f l x -> <:expr< $id:id$ . fold $f$ $l$ $x$ >>)
      (fun k v i e -> <:expr< fun $id:k$ $id:v$ $id:i$ -> $e$ >>)

let rule_record typ fields _ =
  let count = List.length fields in
  let vars = Util.gen_names "field" fields in
    rule typ (tuple (List.map v vars)) [<>] (List.map2 (fun (_, t) x -> (t, v x)) fields vars)
      (fun readers ->
         [List.flatten (List.map rflat readers)
          @ [Expr(false,
                  fun env next ->
                    <:expr<
                      let $id:Env.nth (count - 1) env$ =
                        $ Ast.ExRec(_loc,
                                    Ast.rbSem_of_list
                                      (List.map2 (fun (name, _) id -> <:rec_binding< $ident_of_string name$ = $id:id$ >>)
                                         fields (Env.lasts count env)),
                                    Ast.ExNil _loc) $ in
                        $next$ >>);
             Update_env (Env.add (1 - count))]])
      (fun writers ->
         [Update_env (Env.add (count - 1))
          :: Expr(false,
                  fun env next ->
                    <:expr<
                      let $ Ast.PaRec(_loc,
                                      Ast.paSem_of_list
                                        (List.map2 (fun (name, _) id -> <:patt< $ident_of_string name$ = $id:id$ >>)
                                           fields (Env.lasts count env))) $ = $id:Env.nth (count - 1) env$ in
                        $next$ >>)
          :: List.flatten (List.map wflat writers)])

let sig_matcher dbust =
  let dbus_sig = signature_of_dtypes dbust in
  let total_sig = Printf.sprintf "%c%s\x00" (char_of_int (String.length dbus_sig)) dbus_sig in
  let len = String.length total_sig in
    [Expr(true,
          fun env next ->
            <:expr<
              if not string_match buffer i $str:total_sig$ $int:string_of_int len$
              then raise Data_error "unexpected variant signature"
                $next$
                >>);
     Advance_fixed(len, false)]

let sig_writer dbust =
  let dbus_sig = signature_of_dtypes dbust in
  let total_sig = Printf.sprintf "%c%s\x00" (char_of_int (String.length dbus_sig)) dbus_sig in
  let len = String.length total_sig in
    [Expr(true,
          fun _ next ->
            <:expr<
              String.blit $str:total_sig$ 0 buffer i $int:string_of_int len$
            >>);
     Advance_fixed(len, false)]

let rule_variant typ key_type variants env =
  rule typ (tuple [v"x"; dvariant]) [< (key_type, v"x") >]
    (List.map (fun (_, _, _, cts, dt) -> (tuple cts, dbus_type_of_dtypes dt)) variants)
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
                        :: Expr(false,
                                fun env next ->
                                  <:expr<
                                    let $id:Env.last env$ = $expr$ in
                                      $next$
                                      >>)
                        :: wflat key_writer
                        @ sig_writer dbust
                        @ List.flatten writer,
                        (fun env -> <:expr< >>)))
                    variants writers);
         Reset_padding(0, 1)]])

let rule_record_option typ key_type fields env =
  rule typ (darray (dstructure (tuple [v"x"; dvariant]))) [< (key_type, v"x") >]
    (List.map (fun (_, _, _, ct, dt) -> (tuple [ct], dbus_type_of_dtypes dt)) fields)
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
          (Ast.ExRec(_loc,
                     <:expr< $Ast.rbSem_of_list
                       (List.map (fun (_, _, name, _, _) -> <:rec_binding< $ident_of_string name$ = None >>) fields)$
                     >>,
                     Ast.ExNil _loc))
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
                               let i = match $id:Env.last env$.$ident_of_string name$ with
                                 | None -> i
                                 | Some($id:Env.nth (-1) env$) ->
                                     $GenCode.generate_writer true env instrs (fun env -> <:expr< i >>)$
                               in $acc$
                                  >>)
                        fields writers
                        (<:expr<
                           int_uint32 j (i - k);
                         $next$ >>))$ >>);
         Update_env (Env.add (-1));
         Reset_padding(0, 1)]])

(* Serialization of all basic types *)
let default_rules =
  [ (fun _ -> rule (v"x") (dstructure (v"x")) [< (v"x", v"x") >] []
       (fun reader _ -> [Align 8 :: rflat reader])
       (fun writer _ -> [Align 8 :: wflat writer]));
    (fun _ -> rule char dbyte Seq.nil []
       (fun _ -> [fixed_reader (fun env next -> uint8_reader_expr (Env.last env) next) 1])
       (fun _ -> [fixed_writer (fun env next -> uint8_writer_expr (Env.last env) next) 1]));
    simple_serializer "int_uint32" int dboolean 4;
    simple_serializer "int_int16" int dint16 2;
    simple_serializer "int_uint16" int duint16 2;
    simple_serializer "int_int32" int dint32 4;
    simple_serializer "int_uint32" int duint32 4;
    simple_serializer "int32_int32" int32 dint32 4;
    simple_serializer "int32_uint32" int32 duint32 4;
    simple_serializer "int_int64" int dint64 8;
    simple_serializer "int_uint64" int duint64 8;
    simple_serializer "int64_int64" int64 dint64 8;
    simple_serializer "int64_uint64" int64 duint64 8;
    simple_serializer "float_double" float ddouble 8;
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
    rule_convert int bool
      (<:expr< (function
                  | false -> 0
                  | true -> 1) >>)
      (<:expr< (function
                  | 0 -> false
                  | 1 -> true
                  | n -> raise Data_error ("invalid boolean value: " ^ string_of_int n)) >>);
    rule_array (list (v"x")) (v"x") true
      (<:expr< [] >>)
      (fun x acc -> <:expr< $x$ :: $acc$ >>)
      (fun f l x -> <:expr< List.fold_left $f$ $x$ $l$ >>)
      (fun x i e -> <:expr< fun $id:i$ $id:x$ -> $e$ >>);
    rule_dict (list (tuple [v"x"; v"y"])) (v"x") (v"y") false
      (<:expr< [] >>)
      (fun k v acc -> <:expr< ($k$, $v$) :: $acc$ >>)
      (fun f l x -> <:expr< List.fold_left $f$ $x$ $l$ >>)
      (fun k v i e -> <:expr< fun $id:i$ ($id:k$, $id:v$) -> $e$ >>);
    rule_dict (typ "Hashtbl.t" [v"x"; v"y"]) (v"x") (v"y") false
      (<:expr< Hashtbl.create 42 >>)
      (fun k v acc -> <:expr< Hashtbl.add $acc$ $k$ $v$; $acc$ >>)
      (fun f l x -> <:expr< Hashtbl.fold $f$ $l$ $x$ >>)
      (fun k v i e -> <:expr< fun $id:k$ $id:v$ $id:i$ -> $e$ >>) ]

let gen generator part rules camlt dbust env =
  let env = ref env in
    match generator (List.map (fun f -> part (f env)) rules) camlt dbust with
      | None -> assert false
      | Some x -> (!env, List.flatten x)

let gen_reader rules camlt dbust env =
  gen RG.generate fst rules camlt dbust env

let gen_writer rules camlt dbust env =
  gen WG.generate snd rules camlt dbust env
