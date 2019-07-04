(*
 * term.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Terms manipulation *)

open Printf
open OBus_introspect_ext

(* +-----------------------------------------------------------------+
   | D-Bus types --> term (implementation)                           |
   +-----------------------------------------------------------------+ *)

let rec impl = function
  | Term("byte", []) -> term "basic_byte" []
  | Term("boolean", []) -> term "basic_boolean" []
  | Term("int16", []) -> term "basic_int16" []
  | Term("int32", []) -> term "basic_int32" []
  | Term("int64", []) -> term "basic_int64" []
  | Term("uint16", []) -> term "basic_uint16" []
  | Term("uint32", []) -> term "basic_uint32" []
  | Term("uint64", []) -> term "basic_uint64" []
  | Term("double", []) -> term "basic_double" []
  | Term("string", []) -> term "basic_string" []
  | Term("signature", []) -> term "basic_signature" []
  | Term("object_path", []) -> term "basic_object_path" []
  | Term("unix_fd", []) -> term "basic_unix_fd" []
  | Term("array", [Term("byte", [])]) -> term "byte_array" []
  | Term("dict", [tk; tv]) -> term "dict" [tk; impl tv]
  | Term(name, tl) -> term name (List.map impl tl)
  | Tuple tl -> tuple (List.map impl tl)

(* +-----------------------------------------------------------------+
   | D-Bus types --> term (interface)                                |
   +-----------------------------------------------------------------+ *)

let rec intf = function
  | Term("byte", []) -> term "char" []
  | Term("boolean", []) -> term "bool" []
  | Term("int16", []) -> term "int" []
  | Term("int32", []) -> term "int32" []
  | Term("int64", []) -> term "int64" []
  | Term("uint16", []) -> term "int" []
  | Term("uint32", []) -> term "int32" []
  | Term("uint64", []) -> term "int64" []
  | Term("double", []) -> term "float" []
  | Term("string", []) -> term "string" []
  | Term("signature", []) -> term "OBus_value.signature" []
  | Term("object_path", []) -> term "OBus_path.t" []
  | Term("unix_fd", []) -> term "Unix.file_descr" []
  | Term("array", [Term("byte", [])]) -> term "string" []
  | Term("array", [t]) -> term "list" [intf t]
  | Term("dict", [tk; tv]) -> term "list" [tuple [intf tk; intf tv]]
  | Term("variant", []) -> term "OBus_value.V.single" []
  | Term(name, tl) -> term name (List.map intf tl)
  | Tuple tl -> tuple (List.map intf tl)

(* +-----------------------------------------------------------------+
   | Term printing (implementation)                                  |
   +-----------------------------------------------------------------+ *)

let rec print_impl top oc = function
  | Term(id, []) ->
      output_string oc id
  | Term(id, tl) ->
      if not top then output_char oc '(';
      output_string oc id;
      List.iter
        (fun t ->
           output_char oc ' ';
           print_impl false oc t)
        tl;
      if not top then output_char oc ')'
  | Tuple [] ->
      if not top then output_char oc '(';
      output_string oc "structure seq0";
      if not top then output_char oc ')'
  | Tuple tl ->
      if not top then output_char oc '(';
      fprintf oc "structure (seq%d" (List.length tl);
      List.iter
        (fun t ->
           output_char oc ' ';
           print_impl false oc t)
        tl;
      output_char oc ')';
      if not top then output_char oc ')'

(* +-----------------------------------------------------------------+
   | Term printing (interface)                                       |
   +-----------------------------------------------------------------+ *)

let rec print_intf top oc = function
  | Term(id, []) -> output_string oc id
  | Term(id, [t]) -> fprintf oc "%a %s" (print_intf false) t id
  | Term(id, tl) -> fprintf oc "(%a) %s" (print_seq true ", ") tl id
  | Tuple [] -> output_string oc "unit"
  | Tuple tl -> if top then print_seq false " * " oc tl else fprintf oc "(%a)" (print_seq false " * ") tl

and print_seq top sep oc = function
  | [] -> ()
  | [t] -> print_intf top oc t
  | t :: tl -> fprintf oc "%a%s%a" (print_intf top) t sep (print_seq top sep) tl
