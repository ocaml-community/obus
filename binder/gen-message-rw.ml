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
open Camlp4.PreCast

let _loc = Loc.ghost

let field code name camlt dbust =
  (Ast.PaChr(_loc, String.make 1 (char_of_int code)),
   Ast.ExChr(_loc, String.make 1 (char_of_int code)),
   "Header." ^ name, camlt, dbust)

let header_fields = typ "Header.fields" []

let rules =
  rule_record_option
    header_fields  char
    [field 1 "path" string [Tstring];
     field 2 "member" string [Tstring];
     field 3 "interface" string [Tstring];
     field 4 "error" string [Tstring];
     field 5 "reply" int32 [Tuint32];
     field 6 "destination" string [Tstring];
     field 7 "sender" string [Tstring];
     field 8 "signature" string [Tstring]]
  :: default_rules

let renv, rcode = gen_reader rules header_fields (darray (dstructure (tuple [dbyte; dvariant]))) []
let wenv, wcode = gen_writer rules header_fields (darray (dstructure (tuple [dbyte; dvariant]))) []

let id_reader = match renv with
  | [(id, _)] -> id
  | _ -> assert false

let reader_implem module_name =
  (<:str_item<
   open Wire

   module $uid:module_name$ =
   struct
     open Wire.$uid:module_name$
     open Buffer

       $ Ast.stSem_of_list
       (List.map (fun (id, expr) ->
                    <:str_item<
                      let $id:id$ = $expr$
                        >>) renv) $

     let read limit = $id:id_reader$ 0 limit
   end
     >>)

let writer_implem module_name =
  (<:str_item<
   open Wire

   module $uid:module_name$ =
   struct
     open Wire.$uid:module_name$
     open Buffer

       $ Ast.stSem_of_list
       (List.map (fun (id, expr) ->
                    <:str_item<
                      let $id:id$ = $expr$
                        >>) wenv) $

     let write v0 =
       let i = 12 in
         $ GenCode.generate_writer false (Env.init 1)
           (optimize 4 8 wcode).opt_code (fun env -> <:expr< >>) $ 0 limit
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
