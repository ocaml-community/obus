(*
 * oBus_idl.ml
 * -----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open Camlp4.PreCast
open Syntax
open OBus_introspect_ext
open OBus_value

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

let interfaces = Gram.Entry.mk "interfaces"

let parse_int typ str =
  match typ with
    | T.Byte -> V.Byte(char_of_int (int_of_string str))
    | T.Int16 -> V.Int16(int_of_string str)
    | T.Int32 -> V.Int32(Int32.of_string str)
    | T.Int64 -> V.Int64(Int64.of_string str)
    | T.Uint16 -> V.Uint16(int_of_string str)
    | T.Uint32 -> V.Uint32(Int32.of_string str)
    | T.Uint64 -> V.Uint64(Int64.of_string str)
    | _ -> assert false

EXTEND Gram
  GLOBAL: interfaces;

  ident:
    [ [ n = LIDENT -> n
      | n = UIDENT -> n
      ] ];

  interfaces:
    [ [ l = LIST0 interface -> l ] ];

  name:
    [ [ n = ident; "."; rest = SELF ->
          n ^ "." ^ rest
      | n = ident ->
          n ] ];

  interface:
    [ [ "interface"; name = name; "{"; members = LIST0 member; "}" ->
          let rec get_members = function
            | [] -> []
            | `Member m :: rest -> m :: get_members rest
            | `Annotation _ :: rest -> get_members rest
            | `Symbol _ :: rest -> get_members rest
          in
          let rec get_annotations = function
            | [] -> []
            | `Member _ :: rest -> get_annotations rest
            | `Annotation a :: rest -> a :: get_annotations rest
            | `Symbol _ :: rest ->  get_annotations rest
          in
          let rec get_symbols = function
            | [] -> []
            | `Member _ :: rest -> get_symbols rest
            | `Annotation _ :: rest -> get_symbols rest
            | `Symbol s :: rest -> s :: get_symbols rest
          in
          (name, get_members members, get_symbols members, get_annotations members) ] ];

  member:
    [ [ "method"; name = ident; ":"; i_args = arguments; "->"; o_args = arguments; annotations = annotations ->
          `Member(Method(name, i_args, o_args, annotations))
      | "signal"; name = ident; ":"; args = arguments; annotations = annotations ->
          `Member(Signal(name, args, annotations))
      | "property_r"; name = ident; ":"; typ = type_term; annotations = annotations ->
          `Member(Property(name, typ, Read, annotations))
      | "property_w"; name = ident; ":"; typ = type_term; annotations = annotations ->
          `Member(Property(name, typ, Write, annotations))
      | "property_rw"; name = ident; ":"; typ = type_term; annotations = annotations ->
          `Member(Property(name, typ, Read_write, annotations))
      | "annotation"; name = STRING; "="; value = STRING ->
          `Annotation(name, value)
      | "enum"; name = ident; ":"; typ = key_type; "{"; values = LIST1 value; "}" ->
          `Symbol(name, sym_enum typ (List.map (fun (key, value) -> (parse_int typ key, value)) values))
      | "flag"; name = ident; ":"; typ = key_type; "{"; values = LIST1 value; "}" ->
          `Symbol(name, sym_flag typ (List.map (fun (key, value) -> (parse_int typ key, value)) values))
      ] ];

  value:
    [ [ key = INT; ":"; value = ident -> (key, value)
      | "-"; key = INT; ":"; value = ident -> ("-" ^ key, value)
      | "+"; key = INT; ":"; value = ident -> (key, value)
      ] ];

  annotations:
    [ [ "with"; "{"; l = LIST1 annotation; "}" -> l
      | -> [] ] ];

  annotation:
    [ [ name = name; "="; value = STRING -> (name, value) ] ];

  arguments:
    [ [ "("; l = LIST0 argument SEP ","; ")" -> l ] ];

  argument:
    [ [ name = ident; ":"; typ = type_term -> (Some name, typ)
      | "_"; ":"; typ = type_term -> (None, typ) ] ];

  type_term:
    [ "star"
      [ t = SELF; "*"; tl = type_tuple -> tuple (t :: tl) ]
    | "type_term1"
      [ t = SELF; id = ident -> term id [t] ]
    | "simple"
      [ id = ident -> term id []
      | "("; t = SELF; ","; tl = type_args; ")"; id = ident -> term id (t :: tl)
      | "("; t = SELF; ")" -> t ]
    ];

  type_tuple:
    [ [ t = type_term LEVEL "type_term1"; "*"; tl = SELF -> t :: tl
      | t = type_term LEVEL "type_term1" -> [t] ] ];

  type_args:
    [ [ t = type_term; ","; tl = SELF -> t :: tl
      | t = type_term -> [t] ] ];

  key_type:
    [ [ id = LIDENT ->
          match id with
            | "byte" -> T.Byte
            | "int16" -> T.Int16
            | "int32" -> T.Int32
            | "int64" -> T.Int64
            | "uint16" -> T.Uint16
            | "uint32" -> T.Uint32
            | "uint64" -> T.Uint64
            | _ -> Loc.raise _loc (Failure(sprintf "invalid key type: %s" id)) ] ];
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
  let result = Gram.parse interfaces (Loc.mk file_name) (Stream.of_channel ic) in
  close_in ic;
  result

(* +-----------------------------------------------------------------+
   | Printing                                                        |
   +-----------------------------------------------------------------+ *)

let rec print_term top oc = function
  | Term(id, []) -> output_string oc id
  | Term(id, [t]) -> fprintf oc "%a %s" (print_term false) t id
  | Term(id, tl) -> fprintf oc "(%a) %s" (print_seq true ", ") tl id
  | Tuple tl -> if top then print_seq false " * " oc tl else fprintf oc "(%a)" (print_seq false " * ") tl

and print_seq top sep oc = function
  | [] -> ()
  | [t] -> print_term top oc t
  | t :: tl -> fprintf oc "%a%s%a" (print_term top) t sep (print_seq top sep) tl

let print_args oc args =
  let rec aux = function
    | [] ->
        ()
    | [(None, typ)] ->
        fprintf oc "_ : %a" (print_term true) typ
    | [(Some name, typ)] ->
        fprintf oc "%s : %a" name (print_term true) typ
    | (None, typ) :: l ->
        fprintf oc "_ : %a, " (print_term true) typ;
        aux l
    | (Some name, typ) :: l ->
        fprintf oc "%s : %a, " name (print_term true) typ;
        aux l
  in
  output_char oc '(';
  aux args;
  output_char oc ')'

let print_annotations oc = function
  | [] ->
      ()
  | l ->
      output_string oc "    with {\n";
      List.iter (fun (name, value) -> fprintf oc "      %s = %S\n" name value) l;
      output_string oc "    }\n"

let string_of_key = function
  | T.Byte -> "byte"
  | T.Int16 -> "int16"
  | T.Int32 -> "int32"
  | T.Int64 -> "int64"
  | T.Uint16 -> "uint16"
  | T.Uint32 -> "uint32"
  | T.Uint64 -> "uint64"
  | _ -> assert false

let print file_name interfaces =
  let oc = open_out file_name in
  List.iter
    (function (name, members, symbols, annotations) ->
       fprintf oc "\ninterface %s {\n" name;
       List.iter
         (fun (name, sym) ->
            let keyword, typ, values =
              match sym with
                | Sym_enum(typ, values) -> "enum", typ, values
                | Sym_flag(typ, values) -> "flag", typ, values
            in
            fprintf oc "  %s %s : %s {\n" keyword name (string_of_key typ);
            let values =
              List.map
                (fun (key, name) ->
                   ((match key with
                       | V.Byte x ->
                           sprintf "%x" (Char.code x)
                       | V.Int16 x | V.Uint16 x ->
                           sprintf "%x" x
                       | V.Int32 x | V.Uint32 x ->
                           sprintf "%lx" x
                       | V.Int64 x | V.Uint64 x ->
                           sprintf "%Lx" x
                       | _ ->
                           assert false),
                    name))
                values
            in
            let max_len = List.fold_left (fun m (key, name) -> max m (String.length key)) 0 values in
            List.iter
              (fun (key, name) ->
                 fprintf oc "    0x%s%s: %s\n" (String.make (max_len - String.length key) '0') key name)
              values;
            fprintf oc "  }\n")
         symbols;
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
                  name (print_term true) typ;
                print_annotations oc annotations)
         members;
       output_string oc "}\n")
    interfaces;
  close_out oc
