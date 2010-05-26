(*
 * oBus_introspect_ext.ml
 * ----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open OBus_value

(* +-----------------------------------------------------------------+
   | Annotations                                                     |
   +-----------------------------------------------------------------+ *)

let obus_enum = "org.ocamlcore.forge.obus.Enum"
let obus_flag = "org.ocamlcore.forge.obus.Flag"
let obus_type = "org.ocamlcore.forge.obus.Type"
let obus_itype = "org.ocamlcore.forge.obus.IType"
let obus_otype = "org.ocamlcore.forge.obus.OType"

(* +-----------------------------------------------------------------+
   | Extended types                                                  |
   +-----------------------------------------------------------------+ *)

type basic =
  | Byte
  | Boolean
  | Int16
  | Int32
  | Int64
  | Uint16
  | Uint32
  | Uint64
  | Double
  | String
  | Signature
  | Object_path
  | Unix_fd
  | Enum of T.basic * (V.basic * string) list
  | Flag of T.basic * (V.basic * string) list

type single =
  | Basic of basic
  | Structure of single list
  | Array of single
  | Dict of basic * single
  | Variant

type sequence = single list

let byte = Byte
let boolean = Boolean
let int16 = Int16
let int32 = Int32
let int64 = Int64
let uint16 = Uint16
let uint32 = Uint32
let uint64 = Uint64
let double = Double
let string = String
let signature = Signature
let object_path = Object_path
let unix_fd = Unix_fd

let check_values func typ values =
  match typ with
    | T.Byte
    | T.Int16
    | T.Int32
    | T.Int64
    | T.Uint16
    | T.Uint32
    | T.Uint64 ->
        List.iter
          (fun (value, name) ->
             if V.type_of_basic value <> typ then
               ksprintf invalid_arg "OBus_introspect_ext.%s: unexpected type" func)
          values
    | _ ->
        ksprintf
          invalid_arg
          "OBus_introspect_ext.%s: type '%s' is not supported for enumerations"
          func
          (T.string_of_basic typ)

let enum typ values =
  check_values "enum" typ values;
  Enum(typ, values)

let flag typ values =
  check_values "flag" typ values;
  Flag(typ, values)

let basic t = Basic t
let structure t = Structure t
let array t = Array t
let dict tk tv = Dict(tk, tv)
let variant = Variant

(* +-----------------------------------------------------------------+
   | Stripping                                                       |
   +-----------------------------------------------------------------+ *)

let strip_basic = function
  | Byte -> T.Byte
  | Boolean -> T.Boolean
  | Int16 -> T.Int16
  | Int32 -> T.Int32
  | Int64 -> T.Int64
  | Uint16 -> T.Uint16
  | Uint32 -> T.Uint32
  | Uint64 -> T.Uint64
  | Double -> T.Double
  | String -> T.String
  | Signature -> T.Signature
  | Object_path -> T.Object_path
  | Unix_fd -> T.Unix_fd
  | Enum(t, _) -> t
  | Flag(t, _) -> t

let rec strip_single = function
  | Basic t -> T.Basic(strip_basic t)
  | Structure tl -> T.structure(List.map strip_single tl)
  | Array t ->  T.Array(strip_single t)
  | Dict(tk, tv) -> T.Dict(strip_basic tk, strip_single tv)
  | Variant -> T.Variant

let strip_sequence l = List.map strip_single l

(* +-----------------------------------------------------------------+
   | Projections                                                     |
   +-----------------------------------------------------------------+ *)

let project_basic = function
  | T.Byte -> Byte
  | T.Boolean -> Boolean
  | T.Int16 -> Int16
  | T.Int32 -> Int32
  | T.Int64 -> Int64
  | T.Uint16 -> Uint16
  | T.Uint32 -> Uint32
  | T.Uint64 -> Uint64
  | T.Double -> Double
  | T.String -> String
  | T.Signature -> Signature
  | T.Object_path -> Object_path
  | T.Unix_fd -> Unix_fd

let rec project_single = function
  | T.Basic t -> Basic(project_basic t)
  | T.Structure tl -> structure(List.map project_single tl)
  | T.Array t -> Array(project_single t)
  | T.Dict(tk, tv) -> Dict(project_basic tk, project_single tv)
  | T.Variant -> Variant

let project_sequence l = List.map project_single l

(* +-----------------------------------------------------------------+
   | Symbols and environments                                        |
   +-----------------------------------------------------------------+ *)

type term = OBus_type_ext_lexer.term =
  | Term of string * term list
  | Tuple of term list

let term name args = Term(name, args)
let tuple = function
  | [t] -> t
  | l -> Tuple l

type symbol =
  | Sym_enum of OBus_value.T.basic * (OBus_value.V.basic * string) list
  | Sym_flag of OBus_value.T.basic * (OBus_value.V.basic * string) list

let sym_enum typ values =
  check_values "sym_enum" typ values;
  Sym_enum(typ, values)

let sym_flag typ values =
  check_values "sym_flag" typ values;
  Sym_flag(typ, values)

type env = (string * symbol) list

exception Resolve_error of string

let rec resolve env = function
  | Term(name, args) -> begin
      let args = List.map (resolve env) args in
      match try Some(List.assoc name env) with Not_found -> None with
        | Some(Sym_enum(typ, values)) ->
            Basic(Enum(typ, values))
        | Some(Sym_flag(typ, values)) ->
            Basic(Flag(typ, values))
        | None ->
            match name, args with
              | "byte", [] -> Basic Byte
              | "boolean", [] -> Basic Boolean
              | "int16", [] -> Basic Int16
              | "int32", [] -> Basic Int32
              | "int64", [] -> Basic Int64
              | "uint16", [] -> Basic Uint16
              | "uint32", [] -> Basic Uint32
              | "uint64", [] -> Basic Uint64
              | "double", [] -> Basic Double
              | "string", [] -> Basic String
              | "signature", [] -> Basic Signature
              | "object_path", [] -> Basic Object_path
              | "unix_fd", [] -> Basic Unix_fd
              | "array", [t] -> Array t
              | "dict", [Basic tk; tv] -> Dict(tk, tv)
              | "dict", [tk; tv] -> raise (Resolve_error "type of a dictionary key must be a basic type")
              | "variant", [] -> Variant
              | _ -> raise (Resolve_error(sprintf "unbounded symbol: %S with arity %d" name (List.length args)))
    end
  | Tuple l ->
      Structure(List.map (resolve env) l)

(* +-----------------------------------------------------------------+
   | Projection D-Bus types -> terms                                 |
   +-----------------------------------------------------------------+ *)

let term_of_basic = function
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

let rec term_of_single = function
  | T.Basic t -> term_of_basic t
  | T.Array t -> term "array" [term_of_single t]
  | T.Dict(tk, tv) -> term "dict" [term_of_basic tk; term_of_single tv]
  | T.Structure tl -> tuple (List.map term_of_single tl)
  | T.Variant -> term "variant" []

let term_of_sequence l = tuple (List.map term_of_single l)

(* +-----------------------------------------------------------------+
   | Exended ast                                                     |
   +-----------------------------------------------------------------+ *)

type name = string

type annotation = name * string
type argument = name option * term

type access = OBus_introspect.access = Read | Write | Read_write

type member =
  | Method of name * argument list * argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * term * access * annotation list

type interface = name * member list * (string * symbol) list * annotation list

(* +-----------------------------------------------------------------+
   | Printing/parsing                                                |
   +-----------------------------------------------------------------+ *)

open Printf

let rec string_of_term = function
  | Term(name, []) ->
      name
  | Term(name, args) ->
      "(" ^ String.concat " " (name :: List.map string_of_term args) ^ ")"
  | Tuple typs ->
      "<" ^ String.concat "," (List.map string_of_term typs) ^ ">"

let string_of_enum name typ values =
  sprintf "%s:%s=%s"
    name
    (match typ with
       | T.Byte -> "byte"
       | T.Int16 -> "int16"
       | T.Int32 -> "int32"
       | T.Int64 -> "int64"
       | T.Uint16 -> "uint16"
       | T.Uint32 -> "uint32"
       | T.Uint64 -> "uint64"
       | _ -> assert false)
    (String.concat ","
       (List.map
          (fun (key, value) ->
             sprintf "%s:%s"
               (match key with
                  | V.Byte x ->
                      string_of_int (Char.code x)
                  | V.Int16 x | V.Uint16 x ->
                      string_of_int x
                  | V.Int32 x | V.Uint32 x ->
                      Int32.to_string x
                  | V.Int64 x | V.Uint64 x ->
                      Int64.to_string x
                  | _ ->
                      assert false)
               value)
          values))

let string_of_flag = string_of_enum

let term_of_string str =
  try
    OBus_type_ext_lexer.single (Lexing.from_string str)
  with OBus_type_ext_lexer.Fail(pos, msg) ->
    ksprintf failwith "failed to parse extended type %S, at position %d: %s" str pos msg

let enum_of_string str =
  try
    OBus_type_ext_lexer.enum_and_flag (Lexing.from_string str)
  with OBus_type_ext_lexer.Fail(pos, msg) ->
    ksprintf failwith "failed to parse extended symbol %S, at position %d: %s" str pos msg

let flag_of_string = enum_of_string

(* +-----------------------------------------------------------------+
   | Encoding                                                        |
   +-----------------------------------------------------------------+ *)

let set_annotation name value annotations =
  let rec loop acc = function
    | [] ->  (name, value) :: acc
    | (name', _) :: rest when name = name' -> (name, value) :: List.rev_append acc rest
    | a :: rest -> loop (a :: acc) rest
  in
  loop [] annotations

let encode_arguments env args annotation_name annotations =
  let rec loop acc use_ext = function
    | [] ->
        (List.rev acc, use_ext)
    | (name, typ) :: rest ->
        let ext_typ = resolve env typ in
        let std_typ = strip_single ext_typ in
        (* Check whether the type contains extended types: *)
        if project_single std_typ = ext_typ then
          loop ((name, std_typ) :: acc) use_ext rest
        else
          loop ((name, std_typ) :: acc) true rest
  in
  let args', use_ext = loop [] false args in
  if use_ext then
    (args',
     set_annotation annotation_name
       (string_of_term
          (tuple
             (List.map snd args)))
       annotations)
  else
    (args', annotations)

let encode (name, members, symbols, annotations) =
  let env = symbols in
  let members =
    List.map
      (function
         | Method(name, i_args, o_args, annotations) ->
             let i_args, annotations = encode_arguments env i_args obus_itype annotations in
             let o_args, annotations = encode_arguments env o_args obus_otype annotations in
             OBus_introspect.Method(name, i_args, o_args, annotations)
         | Signal(name, args, annotations) ->
             let args, annotations = encode_arguments env args obus_type annotations in
             OBus_introspect.Signal(name, args, annotations)
         | Property(name, typ, access, annotations) -> begin
             match encode_arguments env [(None, typ)] obus_type annotations with
               | [(None, typ)], annotations ->
                   OBus_introspect.Property(name, typ, access, annotations)
               | _ ->
                   assert false
           end)
      members
  in
  let annotations =
    List.map
      (function
         | (name, Sym_enum(typ, values)) ->
             (obus_enum, string_of_enum name typ values)
         | (name, Sym_flag(typ, values)) ->
             (obus_flag, string_of_flag name typ values))
      symbols
    @ annotations
  in
  (name, members, annotations)

let get_annotation name annotations =
  let rec loop acc = function
    | [] ->
        (acc, None)
    | (name', value) :: rest ->
        if name = name' then
          (List.rev_append acc rest, Some value)
        else
          loop ((name', value) :: acc) rest
  in
  loop [] annotations

let decode_arguments args annotation_name annotations =
  match get_annotation annotation_name annotations with
    | (annotations, None) ->
        (List.map (fun (name, typ) -> (name, term_of_single typ)) args,
         annotations)
    | (annotations, Some value) ->
        (List.map2
           (fun (name, _) term -> (name, term))
           args
           (match term_of_string value with
              | Tuple l -> l
              | t -> [t]),
         annotations)

let decode (name, members, annotations) =
  let members =
    List.map
      (function
         | OBus_introspect.Method(name, i_args, o_args, annotations) ->
             let i_args, annotations = decode_arguments i_args obus_itype annotations in
             let o_args, annotations = decode_arguments o_args obus_otype annotations in
             Method(name, i_args, o_args, annotations)
         | OBus_introspect.Signal(name, args, annotations) ->
             let args, annotations = decode_arguments args obus_type annotations in
             Signal(name, args, annotations)
         | OBus_introspect.Property(name, typ, access, annotations) -> begin
             match decode_arguments [(None, typ)] obus_type annotations with
               | [(None, typ)], annotations ->
                   Property(name, typ, access, annotations)
               | _ ->
                   assert false
           end)
      members
  in
  let symbols, annotations =
    List.partition
      (fun (name, value) -> name = obus_enum || name = obus_flag)
      annotations
  in
  let symbols =
    List.map
      (fun (name, value) ->
         if name = obus_enum then
           let name, typ, values = enum_of_string value in
           (name, sym_enum typ values)
         else if name = obus_flag then
           let name, typ, values = flag_of_string value in
           (name, sym_flag typ values)
         else
           assert false)
      symbols
  in
  (name, members, symbols, annotations)
