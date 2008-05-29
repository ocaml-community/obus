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
open Optimize
open Helpers
open Introspect

let _loc = Loc.ghost

let dbus_types args = List.map (fun (_, _, dbus_type) -> dbus_type) args
let args_count caml_type = List.length (list_of_tuple caml_type)
let args_names count = Util.gen_list (fun n -> <:ident< $lid:"v" ^ string_of_int n$ >>) 0 count

let st_of_env env = Ast.stSem_of_list
  (List.map (fun (id, expr) ->
               <:str_item<
                 let $id:id$ = $expr$
                   >>) (List.rev env))

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

let types_module_name mods = Util.rjoin "_" ("Internal_types" :: mods)

let rec gen_types prefix (Node(interf_name, interf_content, sons)) =
  make_module (types_module_name prefix)
    (List.map begin function
       | Flag(name, values) ->
           let is_poly = List.exists (fun (_, name) -> name.[0] = '`') values in
           let typ =
             if is_poly
             then Ast.TyVrnEq(_loc, Ast.tyOr_of_list (List.map (fun (_, name) -> Ast.TyVrn(_loc, String.sub name 1 (String.length name - 1))) values))
             else Ast.sum_type_of_list (List.map (fun (_, name) -> (_loc, name, [])) values) in
             (<:str_item<
              type $lid:name$ = $typ$;;
              let $lid:name ^ "_of_int"$ = function
                  $ Ast.mcOr_of_list
                    (List.map
                       (fun (key, name) ->
                          <:match_case< $patt_of_int key$ -> $idexpr_of_string name$ >>)
                       values
                     @ [ <:match_case< _ -> raise Reading.Unexpected_key >> ]) $
              let $lid:"int_of_" ^ name$ = function
                  $ Ast.mcOr_of_list
                    (List.map
                       (fun (key, name) ->
                          <:match_case< $idpatt_of_string name$ -> $expr_of_int key$ >>)
                       values) $
                  >>)
       | _ -> empty_st
     end interf_content)
  :: List.flatten (List.map (fun (name, node) -> gen_types (name :: prefix) node) sons)

let add_rule prefix rules env mapping = function
    | Flag(name, values) ->
        (rule_convert (typ name []) int
           (<:expr< $uid:types_module_name prefix$ . $lid:name ^ "_of_int"$ >>)
           (<:expr< $uid:types_module_name prefix$ . $lid:"int_of_" ^ name$ >>) :: rules,
         env,
         mapping)
    | _ -> (rules, env, mapping)

let rec gen_writers rules env mapping prefix (Node(interf_name, interf_content, sons)) =
  let (rules, env, mapping) = List.fold_left begin fun (rules, env, mapping) -> function
    | Method(dname, cname, (in_args, in_caml_type), _) ->
        let in_args_count = args_count in_caml_type in
        let in_args_names = args_names in_args_count in
        let in_dbus_types = dbus_types in_args in
        let env, writer_code = gen_writer false rules in_caml_type in_dbus_types env in
        let writer_expr =
          GenCode.generate_writer false false (Env.init in_args_count)
            (optimize true 0 8 writer_code).opt_code
            (fun _ -> <:expr< (buffer, i) >>) in
        let writer_fun_expr = func (List.rev in_args_names) writer_expr in
        let env, mapping = find_or_add
          (cname :: prefix)
          (<:expr< fun buffer i -> $writer_fun_expr$ >>)
          env mapping
        in
          (rules,
           env,
           mapping)
    | x -> add_rule prefix rules env mapping x
  end (rules, env, mapping) interf_content in
    List.fold_left
      (fun (rules, env, mapping) (name, node) ->
         gen_writers rules env mapping (name :: prefix) node)
      (rules, env, mapping)
      sons

let rec gen_readers rules env mapping prefix (Node(interf_name, interf_content, sons)) =
  let (rules, env, mapping) = List.fold_left begin fun (rules, env, mapping) -> function
    | Method(dname, cname, _, (out_args, out_caml_type)) ->
        let out_dbus_types = dbus_types out_args in
        let env, reader_code = gen_reader false rules out_caml_type out_dbus_types env in
        let reader_expr =
          GenCode.generate_reader false true Env.empty
            (optimize false 0 8 reader_code).opt_code
            (fun env -> match Env.size env with
               | 0 -> (<:expr< __make_result () >>)
               | _ -> appn (<:expr< __make_result >>) (Env.all env)) in
        let reader_fun_expr = func [ <:ident< __make_result >> ] reader_expr in
        let env, mapping = find_or_add
          (cname :: prefix)
          (<:expr< fun buffer i -> $reader_fun_expr$ >>)
          env mapping
        in
          (rules,
           env,
           mapping)
    | x -> add_rule prefix rules env mapping x
  end (rules, env, mapping) interf_content in
    List.fold_left
      (fun (rules, env, mapping) (name, node) ->
         gen_readers rules env mapping (name :: prefix) node)
      (rules, env, mapping)
      sons

let gen_funcs wmapping rmapping node =
  let rec aux prefix (Node(interf_name, interf_content, sons)) =
    let st = List.fold_left begin fun st -> function
      | Method(dname, cname, (in_args, in_caml_type), (out_args, out_caml_type)) ->
          let in_args_count = args_count in_caml_type
          and out_args_count = args_count out_caml_type in
          let in_args_names = args_names in_args_count in
          let in_dbus_types = dbus_types in_args
          and out_dbus_types = dbus_types out_args in
          let in_dbus_sig = signature_of_dbus_types in_dbus_types
          and out_dbus_sig = signature_of_dbus_types out_dbus_types in

          let sig_match_expr good bad = match out_dbus_sig with
            | "" -> (<:expr< match header.Header.fields.Header.signature with
                       | Some ""
                       | None -> $good$
                       | _ -> $bad$ >>)
            | s -> (<:expr< match header.Header.fields.Header.signature with
                      | Some $patt_of_str s$ -> $good$
                      | _ -> $bad$ >>) in

          let convertor_name = cname :: prefix in
          let writer_id = List.assoc convertor_name wmapping
          and reader_id = List.assoc convertor_name rmapping in

          let func_expr = func in_args_names
            (<:expr< call_func
               (Proxy.connection proxy)
               (__method_call_header proxy $expr_of_str interf_name$ $expr_of_str dname$ $expr_of_str in_dbus_sig$)
               (fun bo buffer i ->
                  match bo with
                    | Header.Little_endian ->
                        $appn <:expr< LocalLEWriter.$id:writer_id$ buffer i >> in_args_names$
                    | Header.Big_endian ->
                        $appn <:expr< LocalBEWriter.$id:writer_id$ buffer i >> in_args_names$)
               (fun header buffer i ->
                  $ sig_match_expr
                    (<:expr< match header.Header.byte_order with
                       | Header.Little_endian ->
                           LocalLEReader.$id:reader_id$ buffer i result_func
                       | Header.Big_endian ->
                           LocalBEReader.$id:reader_id$ buffer i result_func >>)
                    (<:expr< raise Wire.Reading.Unexpected_signature >>) $) >>) in

          let make_tuple_of_out_args = (<:expr< $lid:"__make_tuple_" ^ string_of_int out_args_count$ >>) in
          let intern = (<:ident< $lid:"__" ^ cname ^ "_call"$ >>) in
            (<:str_item<
               $st$;;
           let $id:intern$ = fun call_func result_func proxy -> $func_expr$

           let $lid:cname$ = $id:intern$ Connection.raw_send_message_sync $make_tuple_of_out_args$
           let $lid:cname ^ "_async"$ proxy =
             $ func (in_args_names @ [ <:ident< handler >> ])
               (appn <:expr< $id:intern$ Connection.raw_send_message_async handler proxy >> in_args_names) $
           let $lid:cname ^ "_cookie"$ = $id:intern$ Cookie.raw_send_message_with_cookie $make_tuple_of_out_args$
             >>)
      | Proxy(name, typ, dest, path) ->
          let arg0, expr0 = match typ with
            | `Bus -> (Some "bus", <:expr< Bus.make_proxy bus interface >>)
            | `Connection -> (Some "connection", <:expr< Proxy.make connection interface >>)
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
                 | `Bus -> expr1
                 | `Connection -> <:expr< ~destination:$expr1$ >> $ $expr2$ >> $ >>)
      | _ -> st
    end empty_st interf_content in
    let st = match contain_dbus_declaration interf_content with
      | false -> st
      | true -> (<:str_item<
                 type t = unit
                 type proxy = t Proxy.t
                 let interface = Interface.make_interface_for_proxy () $expr_of_str interf_name$;;
                 $st$
                 >>) in
    let st =
      (<:str_item<
       include $uid:types_module_name prefix$;;
       $st$
       >>) in
      Ast.stSem_of_list
        (st :: List.map (fun (name, node) ->
                           (<:str_item<
                            module $uid:name$ =
                            struct
                              $aux (name :: prefix) node$
                            end >>)) sons)
  in
    aux [] node

module IntSet = Set.Make(struct type t = int let compare = compare end)

let rec collect_tuple_maker set (Node(_, interf_content, sons)) =
  let set = List.fold_left begin fun set -> function
    | Method(_, _, _, (_, out_caml_type)) ->
        IntSet.add (args_count out_caml_type) set
    | _ -> set
  end set interf_content in
    List.fold_left
      (fun set (_, node) -> collect_tuple_maker set node) set sons

let gen rules node =
  let types_st = Ast.stSem_of_list (gen_types [] node) in
  let (_, writers, wmapping) = gen_writers rules [] [] [] node
  and (_, readers, rmapping) = gen_readers rules [] [] [] node in
  let funcs = gen_funcs wmapping rmapping node in
    (<:str_item<
       $types_st$;;
     module LocalLEWriter = struct
       open Values.LEWriter;;
       open Wire.LEWriter;;
       $st_of_env writers$
     end;;
     module LocalBEWriter = struct
       open Values.BEWriter;;
       open Wire.BEWriter;;
       $st_of_env writers$
     end;;
     module LocalLEReader = struct
       open Values.LEReader;;
       open Wire.LEReader;;
       $st_of_env readers$
     end;;
     module LocalBEReader = struct
       open Values.BEReader;;
       open Wire.BEReader;;
       $st_of_env readers$
     end;;
     $ IntSet.fold
       (fun n acc ->
          let expr = match n with
            | 0 -> (<:expr< fun _ -> () >>)
            | _ -> let args = args_names n in
                func args (Ast.exCom_of_list (List.map expr_of_id args)) in
            (<:str_item<
               $acc$;;
             let $lid:"__make_tuple_" ^ string_of_int n$ = $expr$
               >>))
       (collect_tuple_maker IntSet.empty node)
       (<:str_item< >>) $;;
     let __method_call_header proxy interface member signature =
       {
         Header.byte_order = Info.native_byte_order;
         Header.message_type = Header.Method_call;
         Header.flags = Header.default_flags;
         Header.length = ();
         Header.serial = ();
         Header.fields =
           {
             Header.path = Some (Proxy.path proxy);
             Header.member = Some member;
             Header.interface = Some interface;
             Header.error_name = None;
             Header.destination = Proxy.name proxy;
             Header.reply_serial = None;
             Header.sender = None;
             Header.signature = Some signature;
           }
       };;
     $funcs$
     >>)
