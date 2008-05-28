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
let module_writer_name func_name bo = Printf.sprintf "%s_%s_writer" (String.capitalize func_name) bo
let module_reader_name func_name bo = Printf.sprintf "%s_%s_reader" (String.capitalize func_name) bo

let args_count caml_type = List.length (list_of_tuple caml_type)
let args_names count = Util.gen_list (fun n -> <:ident< $lid:"v" ^ string_of_int n$ >>) 0 count

let st_of_env env = Ast.stSem_of_list
  (List.map (fun (id, expr) ->
               <:str_item<
                 let $id:id$ = $expr$
                   >>) (List.rev env))

let rec gen rules (Node(interf_name, interf_content, sons)) =
  let (rules, st) = List.fold_left begin fun (rules, st) -> function
    | Method(dname, cname, (in_args, in_caml_type), (out_args, out_caml_type)) ->
        let in_args_count = args_count in_caml_type
        and out_args_count = args_count out_caml_type in

        let in_args_names = args_names in_args_count
        and out_args_names = args_names out_args_count in

        let in_dbus_types = dbus_types in_args
        and out_dbus_types = dbus_types out_args in

        let auxw, writer_code = gen_writer false rules in_caml_type in_dbus_types []
        and auxr, reader_code = gen_reader false rules out_caml_type out_dbus_types [] in

        let writer_expr =
          GenCode.generate_writer false false (Env.init in_args_count)
            (optimize true 0 8 writer_code).opt_code
            (fun _ -> <:expr< (buffer, i) >>)
        and reader_expr =
          GenCode.generate_reader false true Env.empty
            (optimize false 0 8 reader_code).opt_code
            (fun env -> match Env.size env with
               | 0 -> (<:expr< __make_result () >>)
               | _ -> appn (<:expr< __make_result >>) (Env.all env)) in

        let writer_fun_expr = func (List.rev in_args_names) writer_expr
        and reader_fun_expr = func [ <:ident< __make_result >> ] reader_expr in

        let modw_st =
          (<:str_item<
             $st_of_env auxw$;;
           let writer buffer i = $writer_fun_expr$
             >>)
        and modr_st =
          (<:str_item<
             $st_of_env auxr$;;
           let reader buffer i = $reader_fun_expr$
             >>) in

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

        let func_expr = func in_args_names
          (<:expr< call_func
             (Proxy.connection proxy)
             {
               Header.byte_order = Info.native_byte_order;
               Header.message_type = Header.Method_call;
               Header.flags = Header.default_flags;
               Header.length = ();
               Header.serial = ();
               Header.fields =
                 {
                   Header.path = Some (Proxy.path proxy);
                   Header.member = Some $expr_of_str dname$;
                   Header.interface = Some $expr_of_str interf_name$;
                   Header.error_name = None;
                   Header.destination = Proxy.name proxy;
                   Header.reply_serial = None;
                   Header.sender = None;
                   Header.signature = Some $expr_of_str in_dbus_sig$;
                 }
             }
             (fun bo buffer i ->
                match bo with
                  | Header.Little_endian ->
                      $appn <:expr< $uid:module_writer_name cname "le"$.writer buffer i >> in_args_names$
                  | Header.Big_endian ->
                      $appn <:expr< $uid:module_writer_name cname "be"$.writer buffer i >> in_args_names$)
             (fun header buffer i ->
                $ sig_match_expr
                  (<:expr< match header.Header.byte_order with
                     | Header.Little_endian ->
                         $uid:module_reader_name cname "le"$.reader buffer i result_func
                     | Header.Big_endian ->
                         $uid:module_reader_name cname "be"$.reader buffer i result_func >>)
                  (<:expr< raise Wire.Reading.Unexpected_signature >>) $) >>) in


        let make_tuple_of_out_args = match out_args_count with
          | 0 -> <:expr< fun _ -> () >>
          | _ -> func out_args_names (Ast.exCom_of_list (List.map expr_of_id out_args_names)) in

        let intern = (<:ident< $lid:"__" ^ cname ^ "_call"$ >>) in

        let st =
          (<:str_item<
             $st$;;
           module $uid:module_writer_name cname "le"$ = struct
             open Values.LEWriter;;
             open Wire.LEWriter;;
             $modw_st$
           end
           module $uid:module_writer_name cname "be"$ = struct
             open Values.BEWriter;;
             open Wire.BEWriter;;
             $modw_st$
           end
           module $uid:module_reader_name cname "le"$ = struct
             open Values.LEReader;;
             open Wire.LEReader;;
             $modr_st$
           end
           module $uid:module_reader_name cname "be"$ = struct
             open Values.BEReader;;
             open Wire.BEReader;;
             $modr_st$
           end
           let $id:intern$ = fun call_func result_func proxy -> $func_expr$

           let $lid:cname$ = $id:intern$ Connection.raw_send_message_sync $make_tuple_of_out_args$
           let $lid:cname ^ "_async"$ proxy =
             $ func (in_args_names @ [ <:ident< handler >> ])
               (appn <:expr< $id:intern$ Connection.raw_send_message_async handler proxy >> in_args_names) $
           let $lid:cname ^ "_cookie"$ = $id:intern$ Cookie.raw_send_message_with_cookie $make_tuple_of_out_args$
             >>) in
          (rules, st)
    | _ -> (rules, st)
  end (rules, <:str_item< >>) interf_content in
  let st = match List.exists (function
                                | Method _
                                | Signal _
                                | Property _ -> true
                                | _ -> false) interf_content with
    | false -> st
    | true -> (<:str_item<
               type t = unit
               type proxy = t Proxy.t
               let interface = Interface.make_interface_for_proxy () $expr_of_str interf_name$;;
               $st$
               >>) in
    Ast.stSem_of_list
      (st :: List.map (fun (name, node) ->
                         (<:str_item<
                          module $uid:name$ =
                          struct
                            $gen rules node$
                          end >>)) sons)
