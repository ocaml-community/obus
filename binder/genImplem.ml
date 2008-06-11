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

let dbus_types args = Tseq (List.map (fun (_, _, dbus_type, _) -> dbus_type) args)
let gen_args l = List.map ident_of_string (StrUtil.gen_names "v" l)

let rule_wconvert path ta tb func = function
  | x, y when x = ta ->
      dep [< (tb, y) >]
        (fun writer -> Iconvert(<:expr< $idexpr_of_string func$ >>)
           :: flat writer)
  | _ -> fail

let rule_rconvert path ta tb func = function
  | x, y when x = ta ->
      dep [< (tb, y) >]
        (fun reader -> flat reader
           @ [Iconvert(<:expr< $idexpr_of_string func$ >>)])
  | _ -> fail

let solve rules caml_types args =
  let eqn = (tuple caml_types, dbus_types args) in
    match Solver.solve rules eqn with
      | Some instrs -> instrs
      | None ->
          failwith ("cannot find a solution of this equation: " ^ (string_of_eqn eqn))

let rec gen_signal_readers rules signals =
  List.fold_left begin fun (env, mapping) (dname, cname, args, caml_types) ->
    let expr, env = compile_reader (solve rules caml_types args) (fun l -> appn <:expr< cont >> l) env in
    let id, env = lookup <:expr< fun cont buffer i -> $expr$ >> env in
      (env, (dname, id) :: mapping)
  end (empty_env, []) signals

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

let fresh_uid =
  let count = ref 0 in
    fun () -> incr count; "I_" ^ string_of_int !count

let gen_writer rules args caml_types =
  let vars, expr, env = compile_writer (solve rules caml_types args) <:expr< i >> empty_env in
  let vars' = vars @ [ (<:ident< buffer >>) ; (<:ident< i >>) ] in
  let evars = List.map expr_of_id vars' in
  let id, env = lookup (func vars' expr) env in
  let mkw = fresh_uid ()
  and lew = fresh_uid ()
  and bew = fresh_uid () in
    (<:str_item<
     module $uid:mkw$(Writer : Wire.Writer)(VWriter : Values.Writer) =
     struct
       open Wire;;
       open Values;;
       open Writer;;
       open VWriter;;
       $st_of_env env$
     end;;
     module $uid:lew$ = $uid:mkw$(Wire.LEWriter)(Values.LEWriter)
     module $uid:bew$ = $uid:mkw$(Wire.BEWriter)(Values.BEWriter)
     let __writer =
       $ func vars
         (<:expr< (fun bo buffer i ->
                     match bo with
                       | Wire.Little_endian -> $ appn <:expr< $uid:lew$.$lid:id$ >> evars $
                       | Wire.Big_endian -> $ appn <:expr< $uid:bew$.$lid:id$ >> evars $)
          >>) $
       >>, vars)

let gen_reader rules args caml_types =
  let expr, env = compile_reader (solve rules caml_types args) (fun l -> appn <:expr< cont >> l) empty_env in
  let id, env = lookup <:expr< fun cont buffer i -> $expr$ >> env in
  let mkr = fresh_uid ()
  and ler = fresh_uid ()
  and ber = fresh_uid () in
    (<:str_item<
     module $uid:mkr$(Reader : Wire.Reader)(VReader : Values.Reader) =
     struct
       open Wire;;
       open Values;;
       open Reader;;
       open VReader;;
       $st_of_env env$
     end;;
     module $uid:ler$ = $uid:mkr$(Wire.LEReader)(Values.LEReader)
     module $uid:ber$ = $uid:mkr$(Wire.BEReader)(Values.BEReader)
     let __reader cont bo buffer i = match bo with
       | Wire.Little_endian -> $uid:ler$.$lid:id$ cont buffer i
       | Wire.Big_endian -> $uid:ber$.$lid:id$ cont buffer i
           >>)

let uniq l = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l

let gen_interf path (interf_name, content, proxy_typ, to_proxy) (rrules, wrules) =
  let proxy_expr = match to_proxy with
    | Some f -> <:expr< $idexpr_of_string f$ proxy >>
    | None -> <:expr< proxy >>
  in

  let st_interface_def = match interf_name with
    | "" -> empty_st
    | _ ->
        (<:str_item<
         type t = unit
         let interface = Interface.intern_make () $str:interf_name$;;
         >>)
  in

  let (rrules, wrules, st_main) = List.fold_left begin fun (rrules, wrules, st) decl -> match decl with
    | Method(_, dname, cname, (ins, in_caml_types), (outs, out_caml_types)) ->
        let st_writer, args = gen_writer wrules ins in_caml_types in
        let st_reader = gen_reader rrules outs out_caml_types in

        let writer_expr = appn <:expr< __writer >> (List.map expr_of_id args) in
        let func_def suffix call_suffix f =
          let func_expr =
            func args
              (f <:expr< Proxy.$lid:"intern_proxy_call_" ^ call_suffix$ $proxy_expr$ __desc $writer_expr$ >>)
          in
            (<:str_item<
               let $lid:cname ^ suffix$ proxy = $func_expr$
                 >>)
        in
          (rrules,
           wrules,
           <:str_item<
             $st$;;
           $st_writer$;;
           $st_reader$;;
           let __desc = {
             Proxy.intern_mcd_interface = interface;
             Proxy.intern_mcd_member = $str:dname$;
             Proxy.intern_mcd_input_signature = $str:make_sig ins$;
             Proxy.intern_mcd_output_signature = $str:make_sig outs$;
             Proxy.intern_mcd_reader = __reader $make_tuple outs$;
           };;
           $ func_def "" "sync" (fun x -> x) $;;
           $ func_def "_async" "async"
             (fun x -> <:expr< (fun ?on_error handler -> $x$ ?on_error $make_handler outs$) >>) $;;
           $ func_def "_cookie" "cookie" (fun x -> x) $;;
           $ func_def "_no_reply" "no_reply" (fun x -> x) $ >>)
    | Exception(_, dname, cname) ->
        (rrules,
         wrules,
         <:str_item< $st$;; exception $uid:cname$ of string >>)
    | Interf(implem, true)
    | Implem(implem) ->
        (rrules,
         wrules,
         <:str_item< $st$;; $ Caml.Gram.parse Caml.str_item _loc (Stream.of_string implem) $ >>)
    | Convert(a, b, a_of_b, b_of_a) ->
        ((match a_of_b with
            | Some f -> rule_rconvert path a b f :: rrules
            | None -> rrules),
         (match b_of_a with
            | Some f -> rule_wconvert path a b f :: wrules
            | None -> wrules),
         st)
    | _ -> (rrules, wrules, st)
  end (rrules, wrules, empty_st) content in

  let signals = Util.filter_map (function
                                   | Signal(doc, dname, cname, (args, ctypes)) ->
                                       Some(dname, cname, args, ctypes)
                                   | _ -> None) content in
  let st_signals = match signals with
    | [] -> empty_st
    | _ ->
        let env, mapping = gen_signal_readers rrules signals in
        let mkr = fresh_uid ()
        and ler = fresh_uid ()
        and ber = fresh_uid () in
          (<:str_item<
           type signal = $ Ast.sum_type_of_list
               (List.map
                  (fun (dname, cname, args, ctypes) -> (_loc, cname, make_ctyps ctypes))
                  signals) $;;
           module $uid:mkr$(Reader : Wire.Reader)(VReader : Values.Reader) =
           struct
             open Wire;;
             open Values;;
             open Reader;;
             open VReader;;
             $st_of_env env$
           end;;
           module $uid:ler$ = $uid:mkr$(Wire.LEReader)(Values.LEReader)
           module $uid:ber$ = $uid:mkr$(Wire.BEReader)(Values.BEReader)
           $ Ast.stSem_of_list
             (List.map
                (fun id ->
                   <:str_item< let $lid:"__reader_" ^ id$ cont bo buffer i = match bo with
                     | Wire.Little_endian -> $uid:ler$.$lid:id$ cont buffer i
                     | Wire.Big_endian -> $uid:ber$.$lid:id$ cont buffer i
                         >>)
                (uniq (List.map snd mapping))) $;;
           let signals =
             Signal.make_set interface
               (function
                    $ Ast.mcOr_of_list
                      (List.map begin fun (dname, cname, args, caml_types) ->
                         let names = gen_args caml_types in
                         let dbus_sig = make_sig args in
                         let reader_id = List.assoc dname mapping in
                         let make_variant = func names (appn (idexpr_of_string cname) (List.map expr_of_id names)) in
                           (<:match_case< $str:dname$, $str:dbus_sig$ -> Some($lid:"__reader_" ^ reader_id$ $make_variant$) >>)
                       end signals
                       @ [ <:match_case< _ -> None >> ]) $)
             >>)
  in
    ((rrules, wrules),
     <:str_item<
       $st_interface_def$;;
     $st_main$;;
     $st_signals$
     >>)

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

let gen node =
  let _, implem_tree = Tree.fold_map gen_interf
    (GenSerializer.Reading.default_rules,
     GenSerializer.Writing.default_rules) node in
  let exns = get_exceptions node in
  let st = Ast.stSem_of_list
    (Tree.flat
       (fun st subs ->
          (match st with
             | Some st -> [st]
             | None -> [])
          @ List.map (fun (name, sts) -> make_module name sts) subs)
       implem_tree) in
    match exns with
      | [] -> st
      | _ ->
          (<:str_item<
             $st$;;
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
