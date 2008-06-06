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
  (List.map (fun (id, expr) -> <:str_item< let $id:id$ = $expr$ >>)
     (dump_env env))

let find_or_add name expr env mapping =
  match Util.find_map
    (function
       | (id, expr') when expr = expr' -> Some id
       | _ -> None) env with
      | Some id -> (env, (name, id) :: mapping)
      | None -> let id = (<:ident< $lid:"__intern_" ^ string_of_int (List.length env)$ >>) in
          ((id, expr) :: env, (name, id) :: mapping)

let make_module name sts =
  (<:str_item<
   module $uid:name$ = struct
     $ Ast.stSem_of_list sts $
   end
     >>)

let empty_st = (<:str_item< >>)

let make_ctyps t = List.map ctyp_of_caml_type t

let make_type_def name rec_process mode = function
  | [] -> (<:str_item< type $lid:name$ >>)
  | l ->
      let type_def = match mode with
        | M_poly ->
            Ast.TyVrnEq
              (_loc,
               Ast.tyOr_of_list
                 (List.map
                    (fun (name, typ) ->
                       <:ctyp< `$uid:name$ of $Ast.tyAnd_of_list (make_ctyps typ)$ >>) l))
        | M_variant ->
            Ast.sum_type_of_list
              (List.map (fun (name, typ) -> (_loc, name, make_ctyps typ)) l)
        | M_record ->
            Ast.record_type_of_list
              (List.map
                 (fun (name, typ) -> (_loc, name, false, ctyp_of_caml_type (rec_process (tuple typ)))) l)
      in
        (<:str_item< type $lid:name$ = $type_def$ >>)

let invalid_key name =
  (<:match_case< n -> raise (Content_error
                               (Printf.sprintf
                                  "invalid key for %s: %d\\n" $str:name$ n)) >>)

let flag_vp_reader name mkcstr flags =
  (<:expr< function $ Ast.mcOr_of_list
       (List.map
          (fun (key, name) ->
             <:match_case< $patt_of_int key$ -> $mkcstr name$ >>)
          flags
        @ [ invalid_key name ]) $ >>)

let flag_vp_writer mkcstr flags =
  (<:expr< function $ Ast.mcOr_of_list
       (List.map
          (fun (key, name) ->
             <:match_case< $mkcstr name$ -> $expr_of_int key$ >>)
          flags) $ >>)

let flag_rec_reader name flags =
  (<:expr< List.fold_left
     (fun acc x -> match x with
          $ Ast.mcOr_of_list
            (List.map
               (fun (key, name) ->
                  <:match_case< $patt_of_int key$ -> { acc with $ident_of_string name$ = true } >>)
               flags
             @ [ invalid_key name ]) $)
     $expr_record (List.map (fun (key, name) -> (ident_of_string name, <:expr< true >>)) flags)$
   >>)

let flag_rec_writer flags =
  (<:expr< fun x ->
     let l = [] in
       $ List.fold_left
         (fun acc (key, name) ->
            <:expr<
              let l =
                if x . $idexpr_of_string name$
                then $expr_of_int key$ :: l
                else l in
                $acc$
                >>) <:expr< l >> flags $ >>)

let bw_flag_vp_reader mkcstr flags =
  (<:expr< fun x ->
     let l = [] in
       $ List.fold_left
         (fun acc (key, name) ->
            <:expr<
              let l =
                if x land $expr_of_int (1 lsl key)$ <> 0
                then $mkcstr name$ :: l
                else l in
                $acc$
                >>) <:expr< l >> flags $ >>)

let bw_flag_vp_writer mkcstr flags =
  (<:expr< List.fold_left
     (fun acc x -> acc lor
        (match x with
             $ Ast.mcOr_of_list
               (List.map
                  (fun (key, name) ->
                     <:match_case< $mkcstr name$ -> $expr_of_int (1 lsl key)$ >>)
                  flags) $)) 0 >>)

let bw_flag_rec_reader flags =
  func [ <:ident< x >> ]
    (expr_record
       (List.map
          (fun (key, name) ->
             (ident_of_string name,
              <:expr< x land $expr_of_int (1 lsl key)$ <> 0 >>))
          flags))

let bw_flag_rec_writer flags =
  (<:expr< fun x ->
     $ List.fold_left
       (fun acc (key, name) ->
          <:expr<
            $acc$ lor (if x.$idexpr_of_string name$
                       then $expr_of_int (1 lsl key)$
                       else 0) >>)
       (<:expr< 0 >>)
       flags $ >>)

let vrnexpr_of_string s = (<:expr< `$uid:s$ >>)
let vrnpatt_of_string s = (<:patt< `$uid:s$ >>)

let flag_funcs name flags mode bitwise =
  let flags = List.map
    (fun (key, name, doc) -> (key, name)) flags in
  let reader_func, writer_func = match bitwise, mode with
    | false, M_poly -> (flag_vp_reader name vrnexpr_of_string, flag_vp_writer vrnpatt_of_string)
    | false, M_variant -> (flag_vp_reader name idexpr_of_string, flag_vp_writer idpatt_of_string)
    | false, M_record -> (flag_rec_reader name, flag_rec_writer)
    | true, M_poly -> (bw_flag_vp_reader vrnexpr_of_string, bw_flag_vp_writer vrnpatt_of_string)
    | true, M_variant -> (bw_flag_vp_reader idexpr_of_string, bw_flag_vp_writer idpatt_of_string)
    | true, M_record -> (bw_flag_rec_reader, bw_flag_rec_writer)
  in
    (reader_func flags,
     writer_func flags)

let flag_types t mode bitwise = match bitwise, mode with
  | false, M_poly
  | false, M_variant -> (t, int)
  | false, M_record -> (t, list int)
  | true, M_poly
  | true, M_variant -> (list t, int)
  | true, M_record -> (t, int)

let gen_type_definitions node =
  Tree.fold begin fun path (interf_name, content) acc ->
    let st = make_module (types_module_name path)
      ((match interf_name with
          | "" -> empty_st
          | _ ->
              let t = String.capitalize (Str.global_replace Util.dot_regexp "_" interf_name) in
                (<:str_item<
                 type t = [ $Ast.TyVrn(_loc, t)$ ]
                 let interface = Interface.make_interface_for_proxy $Ast.ExVrn(_loc, t)$ $expr_of_str interf_name$;;
                 >>))
       :: List.map begin function
         | Flag(_, name, mode, flags, bitwise, modul) ->
             let flags = match modul with
               | Some s -> List.map (fun (key, name, doc) -> (key, s ^ "." ^ name, doc)) flags
               | None -> flags
             in
             let convertion_funcs =
               let reader, writer = flag_funcs name flags mode bitwise in
               let ta, tb = flag_types (typ name []) mode bitwise in
               let ta = ctyp_of_caml_type ta and tb = ctyp_of_caml_type tb in
                 (<:str_item<
                    let $lid:name ^ "_reader"$ : $tb$ -> $ta$ = $reader$;;
                  let $lid:name ^ "_writer"$ : $ta$ -> $tb$ = $writer$;;
                  >>) in
               if modul = None
               then
                 (<:str_item<
                    $make_type_def name (fun _ -> bool) mode (List.map (fun (_, name, _) -> (name, [])) flags)$;;
                  $convertion_funcs$;;
                  >>)
               else
                 convertion_funcs
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
let gen_args l = List.map ident_of_string (Util.gen_names "v" l)

let rule_wconvert ta tb func = function
  | x, y when x = ta ->
      dep [< (tb, y) >]
        (fun writer -> Iconvert(<:expr< $idexpr_of_string func$ >>)
           :: flat writer)
  | _ -> fail

let rule_rconvert ta tb func = function
  | x, y when x = ta ->
      dep [< (tb, y) >]
        (fun reader -> flat reader
           @ [Iconvert(<:expr< $idexpr_of_string func$ >>)])
  | _ -> fail

let rule_flag make_rule ta tb path name suffix =
  make_rule ta tb (types_module_name path ^ "." ^ name ^ "_" ^ suffix)

let solve rules caml_types args =
  let eqn = (tuple caml_types, dbus_types args) in
    match Solver.solve rules eqn with
      | Some instrs -> instrs
      | None ->
          failwith ("cannot find a solution of this equation: " ^ (string_of_eqn eqn))

let gen_writers node =
  Tree.fold begin fun path (interf_name, content) acc ->
    List.fold_left begin fun (rules, env, mapping) -> function
      | Method(_, dname, cname, (args, caml_types), _) ->
          let vars, expr, env = compile_writer
            (solve rules caml_types args)
            <:expr< i >> env
          in
          let vars' = vars @ [ (<:ident< buffer >>) ; (<:ident< i >>) ] in
          let id, env = lookup (func vars' expr) env in
            (rules, env, ((path, cname), (vars, id)) :: mapping)
      | Flag(_, name, mode, flags, bitwise, modul) ->
          let type_a, type_b = flag_types (typ name []) mode bitwise in
            (rule_flag rule_wconvert type_a type_b path name "writer" :: rules, env, mapping)
      | Convert(_, a, b, _, Some b_of_a) ->
          (rule_wconvert a b b_of_a :: rules, env, mapping)
      | x -> (rules, env, mapping)
    end acc content
  end (Writing.default_rules, empty_env, []) node

let rec gen_readers node =
  Tree.fold begin fun path (interf_name, content) acc ->
    List.fold_left begin fun (rules, env, mapping) -> function
      | Method(_, dname, cname, _, (args, caml_types))
      | Signal(_, dname, cname, (args, caml_types)) ->
          let expr, env = compile_reader
            (solve rules caml_types args)
            (function
               | [] -> <:expr< cont () >>
               | l -> appn <:expr< cont >> l) env
          in
          let id, env = lookup <:expr< fun cont buffer i -> $expr$ >> env in
            (rules, env, ((path, cname), id) :: mapping)
      | Flag(_, name, mode, flags, bitwise, modul) ->
          let type_a, type_b = flag_types (typ name []) mode bitwise in
            (rule_flag rule_rconvert type_a type_b path name "reader" :: rules, env ,mapping)
      | Convert(_, a, b, Some a_of_b, _) ->
          (rule_rconvert a b a_of_b :: rules, env, mapping)
      | x -> (rules, env, mapping)
    end acc content
  end (Reading.default_rules, empty_env, []) node

let make_sig args = signature_of_dbus_type (dbus_types args)
let make_tuple l = match l with
  | [] -> <:expr< fun () -> () >>
  | _ -> let names = Util.gen_names "x" l in
      func
        (List.map ident_of_string names)
        (Ast.exCom_of_list (List.map idexpr_of_string names))

let gen_funcs internal wmapping rmapping node =
  let proxy_var, proxy_expr = match internal with
    | true -> <:patt< bus >>, <:expr< __to_proxy bus >>
    | false -> <:patt< proxy >>, <:expr< proxy >>
  in

  let implem_tree =
    Tree.map begin fun path (interf_name, content) ->
      let sts = List.map begin function
        | Method(_, dname, cname, (ins, _), (outs, _)) ->
            let args, writer_id = List.assoc (path, cname) wmapping in
            let reader_id = List.assoc (path, cname) rmapping in

            let writer_expr = appn <:expr< __writer >> (List.map expr_of_id args) in
            let func_def name mode args handler =
              let func_expr =
                func args <:expr< $lid:"__" ^ mode$ __desc $writer_expr$ $handler$ $proxy_expr$ >> in
                (<:str_item<
                   let $lid:name$ $proxy_var$ = $func_expr$
                     >>)
            in
              Ast.stSem_of_list
                [ (<:str_item<
                     let __writer = match Info.native_byte_order with
                       | Little_endian -> LEW.$id:writer_id$
                       | Big_endian -> BEW.$id:writer_id$
                   let __desc = {
                     Proxy.method_interface = interface;
                     Proxy.method_member = $str:dname$;
                     Proxy.method_in_sig = $str:make_sig ins$;
                     Proxy.method_out_sig = $str:make_sig outs$;
                     Proxy.method_le_reader = LER.$id:reader_id$;
                     Proxy.method_be_reader = BER.$id:reader_id$;
                   }
                     >>);
                  func_def cname "sync" args (make_tuple outs);
                  func_def (cname ^ "_async") "async" (args @ [ <:ident< handler >> ]) (<:expr< handler >>);
                  func_def (cname ^ "_cookie") "cookie" args (make_tuple outs) ]

        | Proxy(_, name, typ, dest, path) ->
            let arg0, expr0 = match typ with
              | P_bus -> (Some "bus", <:expr< Bus.make_proxy bus interface >>)
              | P_connection -> (Some "connection", <:expr< Proxy.make connection interface >>)
            and arg1, expr1 = match dest with
              | Some s -> (None, expr_of_str s)
              | None -> (Some "dest", <:expr< dest >>)
            and arg2, expr2 = match path with
              | Some s -> (None, expr_of_str s)
              | None -> (Some "path", <:expr< path >>)
            in

            let args = List.map ident_of_string (Util.filter_map (fun x -> x) [arg0; arg1; arg2]) in
              (<:str_item<
                 let $lid:name$ = $func args <:expr< $expr0$ $ match typ with
                   | P_bus -> expr1
                   | P_connection -> <:expr< ~destination:$expr1$ >> $ $expr2$ >> $ >>)
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
                   (fun connection handler header buffer i -> match header with
                      | { Header.message_type = Header.Signal;
                          Header.fields =
                            { Header.interface = Some $str:interf_name$;
                              Header.member = Some member;
                              Header.signature = signature } } ->
                          let signature = match signature with
                            | Some s -> s
                            | None -> ""
                          in
                            begin match member, signature with
                                $ Ast.mcOr_of_list
                                  (List.map begin fun (dname, cname, args, caml_types) ->
                                     let names = gen_args caml_types in
                                     let dbus_sig = make_sig args in
                                     let reader_id = List.assoc (path, cname) rmapping in
                                       (<:match_case< $str:dname$, $str:dbus_sig$ ->
                                         handler
                                           (__make_proxy connection interface header)
                                           ((match header.Header.byte_order with
                                               | Wire.Little_endian ->
                                                   LER.$id:reader_id$
                                               | Wire.Big_endian ->
                                                   BER.$id:reader_id$)
                                              $ func names (appn (idexpr_of_string cname)
                                                              (List.map expr_of_id names)) $
                                               buffer i) >>)
                                   end signals
                                   @ [ <:match_case< _ -> false >> ]) $
                            end
                      | _ -> false)
                 >>)
      in
        (<:str_item<
         include $uid:types_module_name path$;;
         $ if internal
         then <:str_item< let __to_proxy bus = Bus.make_proxy bus interface "org.freedesktop.DBus" "/org/freedesktop/DBus" >>
         else empty_st $
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

let gen internal node =
  let types_st = gen_type_definitions node in
  let (_, writers, wmapping) = gen_writers node
  and (_, readers, rmapping) = gen_readers node in
  let funcs = gen_funcs internal wmapping rmapping node in
    (<:str_item<
       $ if internal
       then empty_st
       else <:str_item< open OBus >> $;;
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
       let __sync desc writer return proxy = Proxy.proxy_call Connection.raw_send_message_sync desc writer return proxy
       let __async desc writer return proxy = Proxy.proxy_call Connection.raw_send_message_async desc writer return proxy
       let __cookie desc writer return proxy = Proxy.proxy_call Cookie.raw_send_message_with_cookie desc writer return proxy
       let __make_proxy connection interface header = match header.Header.fields.Header.path with
         | Some p ->
             Proxy.make connection interface
               ?destination:header.Header.fields.Header.sender p
         | None -> raise (Content_error "signal without object path");;
       $funcs$
       >>)
