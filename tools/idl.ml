(*
 * idl.ml
 * ------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open Camlp4.PreCast
open Syntax
open OBus_introspect
open OBus_value

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

exception Parse_failure of Lexing.position * string

let interfaces = Gram.Entry.mk "interfaces"

EXTEND Gram
  GLOBAL: interfaces;

  ident:
    [ [ n = LIDENT -> n
      | n = UIDENT -> n ] ];

  interfaces:
    [ [ l = LIST0 interface -> l ] ];

  name:
    [ [ n = ident; "."; rest = SELF ->
          n ^ "." ^ rest
      | n = ident ->
          n ] ];

  interface:
    [ [ LIDENT "interface"; name = name; "{"; members = LIST0 member; "}" ->
          let rec get_members = function
            | [] -> []
            | `Member m :: rest -> m :: get_members rest
            | `Annotation _ :: rest -> get_members rest
          in
          let rec get_annotations = function
            | [] -> []
            | `Member _ :: rest -> get_annotations rest
            | `Annotation a :: rest -> a :: get_annotations rest
          in
          (name, get_members members, get_annotations members) ] ];

  member:
    [ [ LIDENT "method"; name = ident; ":"; i_args = arguments; "->"; o_args = arguments; annotations = annotations ->
          `Member(Method(name, i_args, o_args, annotations))
      | LIDENT "signal"; name = ident; ":"; args = arguments; annotations = annotations ->
          `Member(Signal(name, args, annotations))
      | LIDENT "property"; "."; LIDENT "r"; name = ident; ":"; typ = single_type; annotations = annotations ->
          `Member(Property(name, typ, Read, annotations))
      | LIDENT "property"; "."; LIDENT "w"; name = ident; ":"; typ = single_type; annotations = annotations ->
          `Member(Property(name, typ, Write, annotations))
      | LIDENT "property"; "."; LIDENT "rw"; name = ident; ":"; typ = single_type; annotations = annotations ->
          `Member(Property(name, typ, Read_write, annotations))
      | LIDENT "annotation"; name = STRING; "="; value = STRING ->
          `Annotation(name, value)
      ] ];

  annotations:
    [ [ LIDENT "with"; "{"; l = LIST1 annotation; "}" -> l
      | -> [] ] ];

  annotation:
    [ [ name = name; "="; value = STRING -> (name, value) ] ];

  arguments:
    [ [ "("; l = LIST0 argument SEP ","; ")" -> l ] ];

  argument:
    [ [ name = ident; ":"; typ = single_type -> (Some name, typ) ] ];

  single_type:
    [ [ t = basic_type -> T.Basic t
      | t = SELF; LIDENT "array" -> T.Array t
      | "("; t = SELF; ")"; LIDENT "array" -> T.Array t
      | "("; t = SELF; rest = dict_or_tuple -> begin
          match rest with
            | `Structure tl ->
                T.Structure(t :: tl)
            | `Dict tv ->
                match t with
                  | T.Basic tk ->
                      T.Dict(tk, tv)
                  | _ ->
                      Loc.raise _loc (Failure "key of dictionaries must be basic types")
        end
      | LIDENT "variant" -> T.Variant
      ] ];

  dict_or_tuple:
    [ [ "*"; tl = LIST0 single_type SEP "*"; ")"; LIDENT "structure" ->
          `Structure tl
      | ","; tv = single_type; ")"; LIDENT "dict" ->
          `Dict tv
      | ")"; LIDENT "structure" ->
          `Structure []
      ] ];

  basic_type:
    [ [ LIDENT "byte" -> T.Byte
      | LIDENT "boolean" -> T.Boolean
      | LIDENT "int16" -> T.Int16
      | LIDENT "int32" -> T.Int32
      | LIDENT "int64" -> T.Int64
      | LIDENT "uint16" -> T.Uint16
      | LIDENT "uint32" -> T.Uint32
      | LIDENT "uint64" -> T.Uint64
      | LIDENT "double" -> T.Double
      | LIDENT "string" -> T.String
      | LIDENT "signature" -> T.Signature
      | LIDENT "object_path" -> T.Object_path
      | LIDENT "unix_fd" -> T.Unix_fd ] ];
END

let is_ident s =
  let rec loop i =
    if i = String.length s then
      true
    else
      match s.[i] with
        | 'A' .. 'Z'
        | 'a' .. 'z'
        | '_'
        | '\192' .. '\214'
        | '\216' .. '\246'
        | '\248' .. '\255'
        | '\''
        | '0' .. '9' -> loop (i + 1)
        | _ -> false
  in
  loop 0

let parse file_name =
  let ic = open_in file_name in
  let tokens = Gram.filter (Gram.lex (Loc.mk file_name) (Stream.of_channel ic)) in
  let result = Gram.parse_tokens_after_filter interfaces
    (Stream.from
       (fun i ->
          match Stream.next tokens with
            | (KEYWORD id, loc) when is_ident id -> Some(LIDENT id, loc)
            | (token, loc) -> Some(token, loc)))
  in
  close_in ic;
  result

(* +-----------------------------------------------------------------+
   | Printing                                                        |
   +-----------------------------------------------------------------+ *)

let string_of_basic = function
  | T.Byte -> "byte"
  | T.Boolean -> "boolean"
  | T.Int16 -> "int16"
  | T.Int32 -> "int32"
  | T.Int64 -> "int64"
  | T.Uint16 -> "uint16"
  | T.Uint32 -> "uint32"
  | T.Uint64 -> "uint64"
  | T.Double -> "double"
  | T.String -> "string"
  | T.Signature -> "signature"
  | T.Object_path -> "object_path"
  | T.Unix_fd -> "unix_fd"

let rec print_type top oc = function
  | T.Basic t ->
      output_string oc (string_of_basic t)
  | T.Structure [] ->
      if not top then output_char oc '(';
      output_string oc "unit structure";
      if not top then output_char oc ')'
  | T.Structure [t] ->
      if not top then output_char oc '(';
      print_type false oc t;
      output_string oc " structure";
      if not top then output_char oc ')'
  | T.Structure(t :: l) ->
      if not top then output_char oc '(';
      output_char oc '(';
      print_type true oc t;
      List.iter
        (fun t ->
           output_string oc " * ";
           print_type true oc t)
        l;
      output_string oc ") structure";
      if not top then output_char oc ')'
  | T.Array t ->
      if not top then output_char oc '(';
      print_type false oc t;
      output_string oc " array";
      if not top then output_char oc ')'
  | T.Dict(tk, tv) ->
      if not top then output_char oc '(';
      output_char oc '(';
      output_string oc (string_of_basic tk);
      output_string oc ", ";
      print_type true oc tv;
      output_string oc ") dict";
      if not top then output_char oc ')'
  | T.Variant ->
      output_string oc "variant"

let print_args oc args =
  let rec aux i = function
    | [] ->
        ()
    | [(None, typ)] ->
        fprintf oc "x%d : %a" i (print_type true) typ
    | [(Some name, typ)] ->
        fprintf oc "%s : %a" name (print_type true) typ
    | (None, typ) :: l ->
        fprintf oc "x%d : %a, " i (print_type true) typ;
        aux (i + 1) l
    | (Some name, typ) :: l ->
        fprintf oc "%s : %a, " name (print_type true) typ;
        aux (i + 1) l
  in
  output_char oc '(';
  aux 1 args;
  output_char oc ')'

let print_annotations oc = function
  | [] ->
      ()
  | l ->
      output_string oc "    with {\n";
      List.iter (fun (name, value) -> fprintf oc "      %s = %S\n" name value) l;
      output_string oc "    }\n"

let print file_name interfaces =
  let oc = open_out file_name in
  List.iter
    (function (name, members, annotations) ->
       fprintf oc "\ninterface %s {\n" name;
       List.iter (fun (name, value) -> fprintf oc "  annotation %s = %S\n" name value) annotations;
       List.iter
         (function
            | Method(name, i_args, o_args, annotations) ->
                fprintf oc "  method %s : %a -> %a\n" name print_args i_args print_args o_args;
                print_annotations oc annotations
            | Signal(name, args, annotations) ->
                fprintf oc "  signal %s : %a\n" name print_args args;
                print_annotations oc annotations
            | Property(name, typ, access, annotations) ->
                fprintf oc "  property.%s %s : %a\n"
                  (match access with
                     | Read -> "r"
                     | Write -> "w"
                     | Read_write -> "rw")
                  name (print_type true) typ;
                print_annotations oc annotations)
         members;
       output_string oc "}\n")
    interfaces;
  close_out oc
