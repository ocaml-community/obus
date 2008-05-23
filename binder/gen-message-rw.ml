(*
 * gen-message-rw.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This file is used to generate the marshaling/unmarshaling functions for header fields *)

open Types
open GenSerializer
open Optimize
open Helpers

let trace = true

let field code name camlt dbust =
  (patt_of_chr (char_of_int code),
   expr_of_chr (char_of_int code),
   name, camlt, dbust)

let fields = typ "fields" []
let message_type = typ "message_type" []
let flags = typ "flags" []
let protocol_version = typ "protocol_version" []

let protocol_version_value = (<:expr< Constant.protocol_version >>)
let protocol_version_error = (<:expr<
                                (fun p ->
                                   Printf.printf
                                     "invalid protocol version : %d (obus support only %d)"
                                     p Constant.protocol_version) >>)

let rules =
  rule_record_option
    fields  char
    [field 1 "path" string [Tstring];
     field 2 "member" string [Tstring];
     field 3 "interface" string [Tstring];
     field 4 "error" string [Tstring];
     field 5 "reply" int32 [Tuint32];
     field 6 "destination" string [Tstring];
     field 7 "sender" string [Tstring];
     field 8 "signature" string [Tstring]]
  :: rule_convert message_type int
    (<:expr< function
       | 0 -> Invalid
       | 1 -> Method_call
       | 2 -> Method_return
       | 3 -> Error
       | 4 -> Signal
       | _ -> raise (Content_error "unknown message type") >>)
    (<:expr< function
       | Invalid -> 0
       | Method_call -> 1
       | Method_return -> 2
       | Error -> 3
       | Signal -> 4 >>)
  :: rule_convert flags int
    (<:expr< fun flags ->
       { no_reply_expected = flags land 1 = 1;
         no_auto_start = flags land 2 = 2 } >>)
    (<:expr< fun flags ->
       (if flags.no_reply_expected then 1 else 0) lor
         (if flags.no_auto_start then 2 else 0) >>)
  :: rule_constant protocol_version int
    protocol_version_value
    protocol_version_error
  :: default_rules

(* It does not contain the first byte for byte order because it must
   be readed before doing anything *)
let _, header_reader_code =
  gen_reader trace rules
    (tuple [message_type; flags; protocol_version; int; int32; int])
    [Tbyte; Tbyte; Tbyte; Tuint32; Tuint32; Tuint32]
    []
let _, header_writer_code =
  gen_writer trace rules
    (tuple [message_type; flags; protocol_version; int; int32])
    [Tbyte; Tbyte; Tbyte; Tuint32; Tuint32]
    []

let renv, fields_reader_code = gen_reader trace rules fields [Tarray(Tstructure [Tbyte; Tvariant])] []
let wenv, fields_writer_code = gen_writer trace rules fields [Tarray(Tstructure [Tbyte; Tvariant])] []

let header_reader_expr =
  GenCode.generate_reader false (Env.init 0)
    (optimize 1 8 header_reader_code).opt_code (fun _ -> <:expr< (v0, v1, v2, v3, v4) >>)

let header_writer_expr =
  GenCode.generate_writer false (Env.init 5)
    (optimize 1 8 header_writer_code).opt_code (fun _ -> <:expr< >>)

let fields_reader_expr =
  (<:expr< $id:match renv with
     | [(id, _)] -> id
     | _ -> assert false$ 0 limit >>)

let fields_writer_expr =
  GenCode.generate_writer false (Env.init 1)
    (optimize 4 8 fields_writer_code).opt_code (fun _ -> <:expr< buffer >>)

let reader_implem module_name =
  (<:str_item<
   open Wire
   open Header

   module $uid:module_name$ =
   struct
     open Wire.$uid:module_name$

       $ Ast.stSem_of_list
       (List.map (fun (id, expr) ->
                    <:str_item<
                      let $id:id$ = $expr$
                        >>) renv) $

     let read_header buffer =
       let i = 1 in
         $header_reader_expr$

     let read_fields buffer limit =
       $fields_reader_expr$
   end
     >>)

let writer_implem module_name =
  (<:str_item<
   open Wire
   open Header

   module $uid:module_name$ =
   struct
     open Wire.$uid:module_name$

       $ Ast.stSem_of_list
       (List.map (fun (id, expr) ->
                    <:str_item<
                      let $id:id$ = $expr$
                        >>) wenv) $

     let write_header v4 v3 v2 v1 v0 =
       let i = 1 in
         $header_writer_expr$

     let write_fields v0 =
       let i = 12 in
         $fields_writer_expr$
   end
     >>)

let _ =
  Printers.OCaml.print_implem
    (<:str_item<
       $reader_implem "LEReader"$;;
     $reader_implem "BEReader"$;;
     $writer_implem "LEWriter"$;;
     $writer_implem "BEWriter"$
     >>)
