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

let dbus_types args = List.map (fun (_, _, dbus_type, _) -> dbus_type) args
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
       | Flag(_, name, mode, [], bitwise, modul) ->
             (<:str_item<
              type $lid:name$
                  >>)
       | Flag(_, name, mode, values, bitwise, modul) ->
           let typ = match mode with
             | F_poly -> Ast.TyVrnEq(_loc, Ast.tyOr_of_list (List.map (fun (_, name, _) -> Ast.TyVrn(_loc, name)) values))
             | F_variant ->  Ast.sum_type_of_list (List.map (fun (_, name, _) -> (_loc, name, [])) values)
             | F_record -> failwith "not implemented" in
             (<:str_item<
              type $lid:name$ = $typ$
                  >>)
       | _ -> empty_st
     end interf_content @
       [ let signals = Util.filter_map (function
                                          | Signal(doc, dname, cname, (_, ctype)) ->
                                              Some(_loc, cname,
                                                   List.map ctyp_of_type (list_of_tuple ctype))
                                          | _ -> None) interf_content in
           if signals = []
           then empty_st
           else
             (<:str_item<
              type signal =
                  $ Ast.sum_type_of_list signals $
                  >>)
       ])
  :: List.flatten (List.map (fun (name, node) -> gen_types (name :: prefix) node) sons)

let add_rule prefix rules env mapping = function
  | Flag(_, name, mode, values, bitwise, modul) -> begin match mode with
      | F_poly ->
          if not bitwise
          then (rule_convert (typ name []) int
                  (<:expr< function $ Ast.mcOr_of_list
                       (List.map
                          (fun (key, name, _) ->
                             <:match_case< $patt_of_int key$ -> $idexpr_of_string ("`" ^ name)$ >>)
                          values
                        @ [ <:match_case< _ -> raise Reading.Unexpected_key >> ]) $ >>)
                  (<:expr< function $ Ast.mcOr_of_list
                       (List.map
                          (fun (key, name, _) ->
                             <:match_case< $idpatt_of_string ("`" ^ name)$ -> $expr_of_int key$ >>)
                          values) $ >>) :: rules,
                env,
                mapping)
          else (rule_convert (list (typ name [])) int
                  (<:expr< >>)
                  (<:expr< fun l ->
                     List.fold_left
                       (fun acc x -> match x with
                            $ Ast.mcOr_of_list
                              (List.map
                                 (fun (key, name, _) ->
                                    <:match_case< $idpatt_of_string ("`" ^ name)$ -> acc lor $expr_of_int key$ >>)
                                 values) $) 0 l >>) :: rules,
                env,
                mapping)
      | _ -> failwith "not implemented"
    end
  | Convert(_, a, b, a_of_b, b_of_a) ->
      (rule_convert a b
         (match a_of_b with Some s -> idexpr_of_string s | None -> <:expr< >>)
         (match b_of_a with Some s -> idexpr_of_string s | None -> <:expr< >>) :: rules,
       env,
       mapping)
  | _ -> (rules, env, mapping)

let rec gen_writers rules env mapping prefix (Node(interf_name, interf_content, sons)) =
  let (rules, env, mapping) = List.fold_left begin fun (rules, env, mapping) -> function
    | Method(_, dname, cname, (in_args, in_caml_type), _) ->
        let in_args_count = args_count in_caml_type in
        let in_args_names = args_names in_args_count in
        let in_dbus_types = dbus_types in_args in
        let env, writer_code = gen_writer false rules in_caml_type in_dbus_types env in
        let writer_expr =
          GenCode.generate_writer false false (Env.init in_args_count)
            (optimize true 0 8 writer_code).opt_code
            (fun _ -> <:expr< (buffer, i) >>) in
        let args = (<:ident< i >>) :: (<:ident< buffer >>) :: in_args_names in
        let writer_fun_expr = func (List.rev args) writer_expr in
        let env, mapping = find_or_add
          (cname :: prefix)
          writer_fun_expr
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
    | Method(_, dname, cname, _, (out_args, out_caml_type)) ->
        let out_dbus_types = dbus_types out_args in
        let env, reader_code = gen_reader false rules out_caml_type out_dbus_types env in
        let reader_expr =
          GenCode.generate_reader false true Env.empty
            (optimize false 0 8 reader_code).opt_code
            (fun env -> match Env.size env with
               | 0 -> (<:expr< __make_result () >>)
               | _ -> appn (<:expr< __make_result >>) (Env.all env)) in
        let env, mapping = find_or_add
          (cname :: prefix)
          (<:expr< fun buffer i __make_result -> $reader_expr$ >>)
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

let gen_funcs internal wmapping rmapping node =
  let rec aux prefix (Node(interf_name, interf_content, sons)) =
    let sts = List.map begin function
      | Method(_, dname, cname, (in_args, in_caml_type), (out_args, out_caml_type)) ->
          let in_args_count = args_count in_caml_type
          and out_args_count = args_count out_caml_type in
          let in_args_names = args_names in_args_count in
          let in_dbus_types = dbus_types in_args
          and out_dbus_types = dbus_types out_args in
          let in_dbus_sig = signature_of_dbus_types in_dbus_types
          and out_dbus_sig = signature_of_dbus_types out_dbus_types in

          let convertor_name = cname :: prefix in
          let writer_id = List.assoc convertor_name wmapping
          and reader_id = List.assoc convertor_name rmapping in

          let common_desc =
            [ expr_of_str dname;
              expr_of_str in_dbus_sig;
              expr_of_str out_dbus_sig;
              (<:expr< LocalLEReader.$id:reader_id$ >>);
              (<:expr< LocalBEReader.$id:reader_id$ >>) ] in

          let desc = match internal with
            | true -> common_desc
            | false -> expr_of_str interf_name :: common_desc
          in

          let make_tuple_of_out_args =
            (<:expr< $lid:"__make_tuple_" ^ string_of_int out_args_count$ >>) in
          let writer_expr =
            appn (<:expr< __writer >>) in_args_names in
            (<:str_item<
               let __writer = if Info.native_byte_order = Header.Little_endian
               then LocalLEWriter.$id:writer_id$
               else LocalBEWriter.$id:writer_id$
             let __desc = $ Ast.exCom_of_list desc $
             let $lid:cname$ x =
               $ func in_args_names
                 (<:expr< __method_call
                    __desc
                    $writer_expr$
                    Connection.raw_send_message_sync
                    $make_tuple_of_out_args$
                    x >>) $
             let $lid:cname ^ "_async"$ x =
               $ func (in_args_names @ [ <:ident< handler >> ])
                 (<:expr< __method_call
                    __desc
                    $writer_expr$
                    Connection.raw_send_message_async
                    handler
                    x >>) $
             let $lid:cname ^ "_cookie"$ x =
               $ func in_args_names
                 (<:expr< __method_call
                    __desc
                    $writer_expr$
                    Cookie.raw_send_message_with_cookie
                    $make_tuple_of_out_args$
                    x >>) $
               >>)
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
    end interf_content in
    let st = Ast.stSem_of_list sts in
    let st = match contain_dbus_declaration interf_content with
      | false -> st
      | true ->
          let t = String.capitalize (Str.global_replace Util.dot_regexp "_" interf_name) in
            (<:str_item<
             type t = [ $Ast.TyVrn(_loc, t)$ ]
             let interface = Interface.make_interface_for_proxy $Ast.ExVrn(_loc, t)$ $expr_of_str interf_name$;;
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
    | Method(_, _, _, _, (_, out_caml_type)) ->
        IntSet.add (args_count out_caml_type) set
    | _ -> set
  end set interf_content in
    List.fold_left
      (fun set (_, node) -> collect_tuple_maker set node) set sons

let gen internal rules node =
  let types_st = Ast.stSem_of_list (gen_types [] node) in
  let (_, writers, wmapping) = gen_writers rules [] [] [] node
  and (_, readers, rmapping) = gen_readers rules [] [] [] node in
  let funcs = gen_funcs internal wmapping rmapping node in
  let make_method_call_func connection_expr fields =
    (<:expr<
       call_func $connection_expr$
       {
         Header.byte_order = Info.native_byte_order;
         Header.message_type = Header.Method_call;
         Header.flags = Header.default_flags;
         Header.length = ();
         Header.serial = ();
         Header.fields = $fields$
       }
       writer
       (fun header buffer i ->
          if (match header.Header.fields.Header.signature with
                | Some s -> s
                | None -> "") = out_sig
          then match header.Header.byte_order with
            | Header.Little_endian -> le_reader buffer i result_func
            | Header.Big_endian -> be_reader buffer i result_func
          else raise Wire.Reading.Unexpected_signature) >>) in
  let method_call_func =
    if not internal
    then
      (<:str_item<
         let __method_call (interface, member, in_sig, out_sig, le_reader, be_reader) writer call_func result_func proxy =
           $make_method_call_func <:expr< Proxy.connection proxy >>
           <:expr<
             {
               Header.path = Some (Proxy.path proxy);
               Header.member = Some member;
               Header.interface = Some interface;
               Header.error_name = None;
               Header.destination = Proxy.name proxy;
               Header.reply_serial = None;
               Header.sender = Proxy.sender proxy;
               Header.signature = Some in_sig;
             }
           >>$ >>)
    else
      (<:str_item<
         let __method_call (member, in_sig, out_sig, le_reader, be_reader) writer call_func result_func bus =
           $make_method_call_func <:expr< Bus.connection bus >>
           <:expr<
             {
               Header.path = Some "/org/freedesktop/DBus";
               Header.member = Some member;
               Header.interface = Some "org.freedesktop.DBus";
               Header.error_name = None;
               Header.destination = Some "org.freedesktop.DBus";
               Header.reply_serial = None;
               Header.sender = None;
               Header.signature = Some in_sig;
             }
           >>$ >>)
  in
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
     $method_call_func$;;
     $funcs$
     >>)
