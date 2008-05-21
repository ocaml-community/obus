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
open Camlp4.PreCast

let _loc = Loc.ghost

let field code name camlt dbust =
  (Ast.PaChr(_loc, char_of_int code),
   Ast.ExChr(_loc, char_of_int code),
   "Header." ^ name, camlt, dbust)

let header_fields = typ "Header.fields" []

let rules =
  rule_record_option
    header_fields  char
    [field 1 "path" string dstring;
     field 2 "member" string dstring;
     field 3 "interface" string dstring;
     field 4 "error" me dme;
     field 5 "reply" rial drial;
     field 6 "destination" string dstring;
     field 7 "sender" string dstring;
     field 8 "signature" string dstring]
  :: default_rules

let renv, rexpr = gen_reader rules 4 8 header_fields (darray (dstructure [dbyte; dvariant])) []
let wenv, wexpr = gen_writer rules 4 8 header_fields (darray (dstructure [dbyte; dvariant])) []

let rexpr = match rexpr with
    (* TODO: change this ugly thing *)
  | <:expr<
      int_uint32 i len; let i = i + 4 = $e$
        >> -> e
  | _ -> assert false

let reader_implem module_name =
  (<:str_item<
   open Wire

   module $uid:module_name$ =
   struct
     open Wire.$uid:module_name$
     open Buffer

     let read limit =
       let i = 0 in
         $rexpr$
   end
     >>)

let _ =
  Printers.OCaml.print_implem
    (<:str_item<
       $reader_implem "LEReader"$;
     $reader_implem "BEReader"$;
     $writer_implem "LEWriter"$;
     $writer_implem "BEWriter"$
     >>)
