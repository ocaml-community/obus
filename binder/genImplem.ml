(*
 * genImplem.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Types
open GenSerializer
open Compile
open Helpers
open Introspect
open Solver
open Instruction

let _loc = Loc.ghost

let types_module_name mods = String.concat "_" ("Internal_types" :: mods)

let st_of_env env = Ast.stSem_of_list
  (List.map (fun (id, expr) -> <:str_item< let $lid:id$ = $expr$ >>)
     (dump_env env))

let make_module name sts =
  (<:str_item<
   module $uid:name$ = struct
     $ Ast.stSem_of_list sts $
   end
     >>)

let empty_st = (<:str_item< >>)

let make_ctyps t = List.map ctyp_of_caml_type t

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make
          (Camlp4.OCamlInitSyntax.Make(Ast)(Gram)(Quotation))))

let gen_type_definitions node =
  Tree.fold begin fun path (interf_name, content, proxy_typ, to_proxy) acc ->
    let st = make_module (types_module_name path)
      ((match interf_name with
          | "" -> empty_st
          | _ ->
              (<:str_item<
               type t = unit
               let interface = Interface.intern_make () $expr_of_str interf_name$;;
               >>))
       :: List.map begin function
         | Exception(_, dname, cname) ->
             (<:str_item< exception $uid:cname$ of string >>)
         | Interf(implem, true)
         | Implem(implem) ->
             Caml.Gram.parse Caml.str_item _loc (Stream.of_string implem)
         | _ -> empty_st
       end content
       @ [ let signals = Util.filter_map (function
                                            | Signal(doc, dname, cname, (_, ctypes)) ->
                                                Some(_loc, cname,
                                                     make_ctyps ctypes)
                                            | _ -> None) content in
             if signals = []
             then empty_st
             else
               (<:str_item<
                type signal = $ Ast.sum_type_of_list signals $
                    >>) ])
    in
      (<:str_item< $acc$;; $st$ >>)
  end empty_st node

let dbus_types args = Tseq (List.map (fun (_, _, dbus_type, _) -> dbus_type) args)
let gen_args l = List.map ident_of_string (StrUtil.gen_names "v" l)

let get_func path func =
  idexpr_of_string
    (if func <> "" && func.[0] = ':'
     then
       Str.string_after func 1
     else
       (types_module_name path) ^ "." ^ func)

let rule_wconvert path ta tb func = function
  | x, y when x = ta ->
      dep [< (tb, y) >]
        (fun writer -> Iconvert(<:expr< $get_func path func$ >>)
           :: flat writer)
  | _ -> fail

let rule_rconvert path ta tb func = function
  | x, y when x = ta ->
      dep [< (tb, y) >]
        (fun reader -> flat reader
           @ [Iconvert(<:expr< $get_func path func$ >>)])
  | _ -> fail

let solve rules caml_types args =
  let eqn = (tuple caml_types, dbus_types args) in
    match Solver.solve rules eqn with
      | Some instrs -> instrs
      | None ->
          failwith ("cannot find a solution of this equation: " ^ (string_of_eqn eqn))

let gen_writers node =
  Tree.fold begin fun path (interf_name, content, proxy_typ, to_proxy) acc ->
    List.fold_left begin fun (rules, env, mapping) -> function
      | Method(_, dname, cname, (args, caml_types), _) ->
          let vars, expr, env = compile_writer
            (solve rules caml_types args)
            <:expr< i >> env
          in
          let vars' = vars @ [ (<:ident< buffer >>) ; (<:ident< i >>) ] in
          let id, env = lookup (func vars' expr) env in
            (rules, env, ((path, cname), (vars, id)) :: mapping)
      | Convert(a, b, _, Some b_of_a) ->
          (rule_wconvert path a b b_of_a :: rules, env, mapping)
      | x -> (rules, env, mapping)
    end acc content
  end (Writing.default_rules, empty_env, []) node

let rec gen_readers node =
  Tree.fold begin fun path (interf_name, content, proxy_typ, to_proxy) acc ->
    List.fold_left begin fun (rules, env, mapping) -> function
      | Method(_, dname, cname, _, (args, caml_types))
      | Signal(_, dname, cname, (args, caml_types)) ->
          let expr, env = compile_reader
            (solve rules caml_types args)
            (fun l -> appn <:expr< cont >> l) env
          in
          let id, env = lookup <:expr< fun cont buffer i -> $expr$ >> env in
            (rules, env, ((path, cname), id) :: mapping)
      | Convert(a, b, Some a_of_b, _) ->
          (rule_rconvert path a b a_of_b :: rules, env, mapping)
      | x -> (rules, env, mapping)
    end acc content
  end (Reading.default_rules, empty_env, []) node

let make_sig args = signature_of_dbus_type (dbus_types args)
let make_tuple l = match l with
  | [] -> <:expr< () >>
  | _ -> let names = StrUtil.gen_names "x" l in
      func
        (List.map ident_of_string names)
        (Ast.exCom_of_list (List.map idexpr_of_string names))
let make_handler l = match l with
  | [] -> <:expr< handler >>
  | _ -> let names = StrUtil.gen_names "x" l in
      (<:expr<
         fun $ Ast.paCom_of_list (List.map idpatt_of_string names) $ ->
           $appn <:expr< handler >> (List.map idexpr_of_string names)$ >>)

let gen_funcs wmapping rmapping node =
  let implem_tree =
    Tree.map begin fun path (interf_name, content, proxy_typ, to_proxy) ->
      let proxy_expr = match to_proxy with
        | Some f -> <:expr< $get_func path f$ proxy >>
        | None -> <:expr< proxy >>
      in
      let sts = List.map begin function
        | Method(_, dname, cname, (ins, _), (outs, _)) ->
            let args, writer_id = List.assoc (path, cname) wmapping in
            let eargs = List.map expr_of_id args in
            let reader_id = List.assoc (path, cname) rmapping in

            let writer_expr = appn <:expr< $lid:"__writer_" ^ writer_id$ >> eargs in
            let func_def name mode args handler =
              let func_expr =
                func args <:expr< Proxy.$lid:"intern_proxy_call_" ^ mode$ $proxy_expr$ __desc $writer_expr$ $handler$ >> in
                (<:str_item<
                   let $lid:name$ proxy = $func_expr$
                     >>)
            in
              Ast.stSem_of_list
                [ (<:str_item<
                   let __desc = {
                     Proxy.intern_mcd_interface = interface;
                     Proxy.intern_mcd_member = $str:dname$;
                     Proxy.intern_mcd_input_signature = $str:make_sig ins$;
                     Proxy.intern_mcd_output_signature = $str:make_sig outs$;
                     Proxy.intern_mcd_reader = $lid:"__reader_" ^ reader_id$ $make_tuple outs$;
                   }
                     >>);
                  func_def cname "sync" args <:expr< >>;
                  func_def (cname ^ "_async") "async" (args @ [ <:ident< handler >> ]) (make_handler outs);
                  func_def (cname ^ "_cookie") "cookie" args <:expr< >> ]

        | _ -> empty_st
      end content in
      let st = Ast.stSem_of_list sts in

      let signals = Util.filter_map (function
                                       | Signal(doc, dname, cname, (args, ctypes)) ->
                                           Some(dname, cname, args, ctypes)
                                       | _ -> None) content in
      let st_signals = match signals with
        | [] -> empty_st
        | _ ->
            (<:str_item<
               let signals =
                 Signal.make_set interface
                   (function
                        $ Ast.mcOr_of_list
                          (List.map begin fun (dname, cname, args, caml_types) ->
                             let names = gen_args caml_types in
                             let dbus_sig = make_sig args in
                             let reader_id = List.assoc (path, cname) rmapping in
                             let make_variant = func names (appn (idexpr_of_string cname) (List.map expr_of_id names)) in
                               (<:match_case< $str:dname$, $str:dbus_sig$ -> Some($lid:"__reader_" ^ reader_id$ $make_variant$) >>)
                    end signals
                 @ [ <:match_case< _ -> None >> ]) $)
                 >>)
      in
        (<:str_item<
         include $uid:types_module_name path$;;
         $st$;;
         $st_signals$
         >>)
    end node in

    Ast.stSem_of_list
      (Tree.flat
         (fun st subs ->
            (match st with
               | Some st -> [st]
               | None -> [])
            @ List.map (fun (name, sts) -> make_module name sts) subs)
         implem_tree)

let get_exceptions node =
  Tree.fold begin fun path (interf_name, content, proxy_typ, to_proxy) acc ->
    List.fold_left begin fun acc -> function
      | Exception(doc, dname, cname) ->
          (dname,
           Ast.idAcc_of_list (List.map ident_of_string (path @ [cname])))
          :: acc
      | _ -> acc
    end acc content
  end [] node

let uniq l = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l

let gen node =
  let types_st = gen_type_definitions node in
  let (_, writers, wmapping) = gen_writers node
  and (_, readers, rmapping) = gen_readers node in
  let funcs = gen_funcs wmapping rmapping node in
  let exns = get_exceptions node in
    (<:str_item<
       open Wire
       $types_st$;;
       module MakeWriter(Writer : Wire.Writer)(VWriter : Values.Writer) =
       struct
         open Writer;;
         open VWriter;;
         $st_of_env writers$
       end;;
       module MakeReader(Reader : Wire.Reader)(VReader : Values.Reader) =
       struct
         open Reader;;
         open VReader;;
         $st_of_env readers$
       end;;
       module LEW = MakeWriter(Wire.LEWriter)(Values.LEWriter)
       module BEW = MakeWriter(Wire.BEWriter)(Values.BEWriter)
       module LER = MakeReader(Wire.LEReader)(Values.LEReader)
       module BER = MakeReader(Wire.BEReader)(Values.BEReader)
       $ Ast.stSem_of_list
         (List.map
            (fun (args, id) ->
               let eargs = List.map expr_of_id args in
                 <:str_item< let $lid:"__writer_" ^ id$ =
                   $ func args
                     (<:expr< (fun bo buffer i ->
                                 match bo with
                                   | Little_endian -> $ appn <:expr< LEW.$lid:id$ >> eargs $ buffer i
                                   | Big_endian -> $ appn <:expr< BEW.$lid:id$ >> eargs $ buffer i)
                      >>) $ >>)
            (uniq (List.map snd wmapping))) $;;
       $ Ast.stSem_of_list
         (List.map
            (fun id ->
               <:str_item< let $lid:"__reader_" ^ id$ cont bo buffer i = match bo with
                 | Little_endian -> LER.$lid:id$ cont buffer i
                 | Big_endian -> BER.$lid:id$ cont buffer i
                     >>)
            (uniq (List.map snd rmapping))) $;;
       $funcs$;;
       let _ =
         Error.register_maker
           (fun name msg ->
              match name with
                  $ Ast.mcOr_of_list
                    (List.map
                       (fun (dname, cname) ->
                          <:match_case< $str:dname$ -> Some($id:cname$ msg) >>)
                       exns
                    @ [ <:match_case< _ -> None >> ]) $);
         Error.register_unmaker
           (function
                $ Ast.mcOr_of_list
                  (List.map
                     (fun (dname, cname) ->
                        <:match_case< $id:cname$ msg -> Some($str:dname$, msg) >>)
                     exns
                   @ [ <:match_case< _ -> None >> ]) $);;
       >>)
