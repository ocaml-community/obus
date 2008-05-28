(*
 * gen-header-rw.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This file is used to generate the marshaling/unmarshaling functions
   for header fields *)

open Camlp4.PreCast
open Types
open GenSerializer
open Optimize
open Helpers

let _loc = Loc.ghost

let trace = false

let field code name camlt dbust =
  (patt_of_chr (char_of_int code),
   expr_of_chr (char_of_int code),
   name, camlt, dbust)

let fields = typ "fields" []
let message_type = typ "message_type" []
let flags = typ "flags" []
let protocol_version = typ "protocol_version" []

let rules =
  rule_record_option
    fields  char
    [field 1 "path" string [Tobject_path];
     field 2 "interface" string [Tstring];
     field 3 "member" string [Tstring];
     field 4 "error_name" string [Tstring];
     field 5 "reply_serial" int32 [Tuint32];
     field 6 "destination" string [Tstring];
     field 7 "sender" string [Tstring];
     field 8 "signature" string [Tsignature]]
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
    (<:expr< Constant.protocol_version >>)
    (<:expr<
       (fun p ->
          failwith
            (Printf.sprintf
               "invalid protocol version : %d (obus support only %d)"
               p Constant.protocol_version)) >>)
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
  GenCode.generate_reader false true (Env.init 0)
    (optimize false 1 8 header_reader_code).opt_code (fun _ -> <:expr< (v0, v1, v2, v3, v4) >>)

let header_writer_expr =
  GenCode.generate_writer false true (Env.init 4)
    (optimize true 1 8 header_writer_code).opt_code (fun _ -> <:expr< () >>)

let fields_reader_expr =
  (<:expr< $id:match renv with
     | [(id, _)] -> id
     | _ -> assert false$ buffer 0 limit >>)

let fields_writer_expr =
  GenCode.generate_writer false false (Env.init 1)
    ((optimize true 4 8 fields_writer_code).opt_code
     @ [AbstractCode.Align 8]) (fun _ -> <:expr< (buffer, i) >>)

let implem bo =
  (<:str_item<
   module $uid:bo ^ "Reader"$ =
   struct
     open Wire.$uid:bo ^ "Reader"$

       $ Ast.stSem_of_list
       (List.map (fun (id, expr) ->
                    <:str_item<
                      let $id:id$ = $expr$
                        >>) renv) $

     let read_constant_part buffer =
       let i = 1 in
         $header_reader_expr$

     let read_fields buffer limit =
       $fields_reader_expr$
   end

   module $uid:bo ^ "Writer"$ =
   struct
     open Wire.$uid:bo ^ "Writer"$

       $ Ast.stSem_of_list
       (List.map (fun (id, expr) ->
                    <:str_item<
                      let $id:id$ = $expr$
                        >>) wenv) $

     let write_constant_part buffer v3 v2 v1 v0 =
       let i = 1 in
         $header_writer_expr$

     let write_fields buffer v0 =
       let i = 12 in
         $fields_writer_expr$
   end
     >>)

let _ =
  Printers.OCaml.print_implem
    (<:str_item<
     open Wire
     $implem "LE"$;;
     $implem "BE"$
     >>)
