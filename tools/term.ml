(*
 * term.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* D-Bus types as term (for printing) *)

module T : sig
  type t =
      private
    | Term of string * t list
    | Tuple of t list
  val term : string -> t list -> t
  val tuple : t list -> t
end = struct
  type t =
    | Term of string * t list
    | Tuple of t list
  let term id args = Term(id, args)
  let tuple = function
    | [t] -> t
    | l -> Tuple l
end

include T

open Printf
open OBus_value

(* +-----------------------------------------------------------------+
   | D-Bus types --> term (implementation)                           |
   +-----------------------------------------------------------------+ *)

let impl_of_basic = function
  | T.Byte -> term "byte" []
  | T.Boolean -> term "boolean" []
  | T.Int16 -> term "int16" []
  | T.Int32 -> term "int32" []
  | T.Int64 -> term "int64" []
  | T.Uint16 -> term "uint16" []
  | T.Uint32 -> term "uint32" []
  | T.Uint64 -> term "uint64" []
  | T.Double -> term "double" []
  | T.String -> term "string" []
  | T.Signature -> term "signature" []
  | T.Object_path -> term "object_path" []
  | T.Unix_fd -> term "unix_fd" []

let rec impl_of_single = function
  | T.Basic T.Byte -> term "basic_byte" []
  | T.Basic T.Boolean -> term "basic_boolean" []
  | T.Basic T.Int16 -> term "basic_int16" []
  | T.Basic T.Int32 -> term "basic_int32" []
  | T.Basic T.Int64 -> term "basic_int64" []
  | T.Basic T.Uint16 -> term "basic_uint16" []
  | T.Basic T.Uint32 -> term "basic_uint32" []
  | T.Basic T.Uint64 -> term "basic_uint64" []
  | T.Basic T.Double -> term "basic_double" []
  | T.Basic T.String -> term "basic_string" []
  | T.Basic T.Signature -> term "basic_signature" []
  | T.Basic T.Object_path -> term "basic_object_path" []
  | T.Basic T.Unix_fd -> term "basic_unix_fd" []
  | T.Structure tl -> term "structure" [impl_of_sequence tl]
  | T.Array(T.Basic T.Byte) -> term "byte_array" []
  | T.Array t -> term "array" [impl_of_single t]
  | T.Dict(tk, tv) -> term "dict" [impl_of_basic tk; impl_of_single tv]
  | T.Variant -> term "variant" []

and impl_of_sequence tl = tuple (List.map impl_of_single tl)

(* +-----------------------------------------------------------------+
   | D-Bus types --> term (interface)                                |
   +-----------------------------------------------------------------+ *)

let intf_of_basic = function
  | T.Byte -> term "char" []
  | T.Boolean -> term "bool" []
  | T.Int16 -> term "int" []
  | T.Int32 -> term "int32" []
  | T.Int64 -> term "int64" []
  | T.Uint16 -> term "int" []
  | T.Uint32 -> term "int32" []
  | T.Uint64 -> term "int64" []
  | T.Double -> term "float" []
  | T.String -> term "string" []
  | T.Signature -> term "OBus_value.signature" []
  | T.Object_path -> term "OBus_path.t" []
  | T.Unix_fd -> term "Unix.file_descr" []

let rec intf_of_single = function
  | T.Basic t -> intf_of_basic t
  | T.Structure tl -> intf_of_sequence tl
  | T.Array(T.Basic T.Byte) -> term "string" []
  | T.Array t -> term "list" [intf_of_single t]
  | T.Dict(tk, tv) -> term "list" [tuple [intf_of_basic tk; intf_of_single tv]]
  | T.Variant -> term "OBus_value.V.single" []

and intf_of_sequence tl = tuple (List.map intf_of_single tl)

(* +-----------------------------------------------------------------+
   | D-Bus types --> term (client)                                   |
   +-----------------------------------------------------------------+ *)

let client_of_basic = function
  | T.Byte -> term "char" []
  | T.Boolean -> term "bool" []
  | T.Int16 -> term "int" []
  | T.Int32 -> term "int" []
  | T.Int64 -> term "int64" []
  | T.Uint16 -> term "int" []
  | T.Uint32 -> term "int" []
  | T.Uint64 -> term "int64" []
  | T.Double -> term "float" []
  | T.String -> term "string" []
  | T.Signature -> term "OBus_value.signature" []
  | T.Object_path -> term "OBus_proxy.t" []
  | T.Unix_fd -> term "Unix.file_descr" []

let rec client_of_single = function
  | T.Basic t -> client_of_basic t
  | T.Structure tl -> client_of_sequence tl
  | T.Array(T.Basic T.Byte) -> term "string" []
  | T.Array t -> term "list" [client_of_single t]
  | T.Dict(tk, tv) -> term "list" [tuple [client_of_basic tk; client_of_single tv]]
  | T.Variant -> term "OBus_value.V.single" []

and client_of_sequence tl = tuple (List.map client_of_single tl)

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
      output_string oc "seq0"
  | Tuple tl ->
      if not top then output_char oc '(';
      fprintf oc "seq%d" (List.length tl);
      List.iter
        (fun t ->
           output_char oc ' ';
           print_impl false oc t)
        tl;
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
