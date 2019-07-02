(*
 * obus_gen_interface.ml
 * ---------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open OBus_value
open OBus_introspect_ext
open OBus_introspect

let mode : [ `Both | `Client | `Server ] ref = ref `Both
let prog_name = Filename.basename Sys.argv.(0)

(* +-----------------------------------------------------------------+
   | Common printers                                                 |
   +-----------------------------------------------------------------+ *)

let term_intf typ =
  Term.intf (OBus_introspect_ext.term_of_single typ)

let term_impl typ =
  Term.impl (OBus_introspect_ext.term_of_single typ)

let tuple_intf types =
  Term.intf (OBus_introspect_ext.term_of_sequence types)

let tuple_impl types =
  Term.impl (OBus_introspect_ext.term_of_sequence types)

let print_record oc members =
  output_string oc "  type 'a members = {\n";
  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           fprintf oc "    m_%s : 'a OBus_object.t -> %a -> %a;\n"
             name
             (Term.print_intf true)
             (tuple (List.map (fun (name, typ) -> term_intf typ) i_args))
             (Term.print_intf true)
             (term "Lwt.t" [(tuple (List.map (fun (name, typ) -> term_intf typ) o_args))])
       | Signal(name, args, annotations) ->
           ()
       | Property(name, typ, Read, annotations) ->
           fprintf oc "    p_%s : 'a OBus_object.t -> %a;\n"
             name
             (Term.print_intf true)
             (term "React.signal" [term_intf typ])
       | Property(name, typ, Write, annotations) ->
           fprintf oc "    p_%s : 'a OBus_object.t -> %a -> unit Lwt.t;\n"
             name
             (Term.print_intf true)
             (term_intf typ)
       | Property(name, typ, Read_write, annotations) ->
           fprintf oc "    p_%s : ('a OBus_object.t -> %a) * ('a OBus_object.t -> %a -> unit Lwt.t);\n"
             name
             (Term.print_intf true)
             (term "React.signal" [term_intf typ])
             (Term.print_intf true)
             (term_intf typ))
    members;
  output_string oc "  }\n"

let print_symbol oc name sym =
  let typ, values =
    match sym with
      | Sym_enum(typ, values) -> typ, values
      | Sym_flag(typ, values) -> typ, values
  in
  fprintf oc "  type type_%s =\n" name;
  match values with
    | [] ->
        ()
    | (key, name) :: rest ->
        fprintf oc "    [ `%s" (String.capitalize_ascii name);
        List.iter (fun (key, name) -> fprintf oc "\n    | `%s" (String.capitalize_ascii name)) rest;
        fprintf oc " ]\n"

(* +-----------------------------------------------------------------+
   | Implementation generation                                       |
   +-----------------------------------------------------------------+ *)

let string_of_integer_enum = function
  | V.Byte x ->
      sprintf "%C" x
  | V.Int16 x | V.Uint16 x ->
      sprintf "%d" x
  | V.Int32 x | V.Uint32 x ->
      sprintf "%ldl" x
  | V.Int64 x | V.Uint64 x ->
      sprintf "%LdL" x
  | _ ->
      assert false

let string_of_integer_flag = function
  | V.Byte x ->
      sprintf "%d" (Char.code x)
  | V.Int16 x | V.Uint16 x ->
      sprintf "%d" x
  | V.Int32 x | V.Uint32 x ->
      sprintf "%ldl" x
  | V.Int64 x | V.Uint64 x ->
      sprintf "%LdL" x
  | _ ->
      assert false

let print_args oc args =
  fprintf oc "(arg%d" (List.length args);
  List.iter
    (function
       | (None, typ) ->
           fprintf oc "\n                       (None, %a)"
             (Term.print_impl true) (term_impl typ)
       | (Some name, typ) ->
           fprintf oc "\n                       (Some %S, %a)"
             name (Term.print_impl true) (term_impl typ))
    args;
  output_char oc ')'

let print_impl oc name members symbols annotations =
  fprintf oc "module %s =\n\
              struct\n\
             \  let interface = %S\n"
    (String.capitalize_ascii (Utils.file_name_of_interface_name name))
    name;

  (***** Symbols *****)

  List.iter
    (fun (name, sym) ->
       print_symbol oc name sym;
       match sym with
         | Sym_enum(typ, values) ->
             fprintf oc "  let cast_%s = function\n" name;
             List.iter
               (fun (key, name) ->
                  fprintf oc "    | `%s -> %s\n"
                    (String.capitalize_ascii name)
                    (string_of_integer_enum key))
               values;
             fprintf oc "  let make_%s = function\n" name;
             List.iter
               (fun (key, name) ->
                  fprintf oc "    | %s -> `%s\n"
                    (string_of_integer_enum key)
                    (String.capitalize_ascii name))
               values;
             fprintf oc "    | n -> Printf.ksprintf failwith \"invalid value for \\\"%s\\\": %s\" n\n"
               name
               (match typ with
                  | T.Byte -> "%c"
                  | T.Int16 | T.Uint16 -> "%d"
                  | T.Int32 | T.Uint32 -> "%ld"
                  | T.Int64 | T.Uint64 -> "%Ld"
                  | _ -> assert false)
         | Sym_flag(typ, values) ->
             fprintf oc "  let cast_%s l =\n\
                        \    let rec loop acc = function\n\
                        \      | [] -> %sacc\n"
               name
               (if typ = T.Byte then "char_of_int " else "");
             List.iter
               (fun (key, name) ->
                  fprintf oc "      | `%s :: rest -> loop (%s) rest\n"
                    (String.capitalize_ascii name)
                    (match key with
                       | V.Byte x ->
                           sprintf "acc lor %d" (Char.code x)
                       | V.Int16 x | V.Uint16 x ->
                           sprintf "acc lor %d" x
                       | V.Int32 x | V.Uint32 x ->
                           sprintf "Int32.logor acc %ldl" x
                       | V.Int64 x | V.Uint64 x ->
                           sprintf "Int64.logor acc %LdL" x
                       | _ ->
                           assert false))
               values;
             fprintf oc "    in\n\
                        \    loop %s l\n"
               (match typ with
                  | T.Byte | T.Int16 | T.Uint16 -> "0"
                  | T.Int32 | T.Uint32 -> "0l"
                  | T.Int64 | T.Uint64 -> "0L"
                  | _ -> assert false);
             fprintf oc "  let make_%s n =\n\
                        \    let l = [] in\n"
               name;
             if typ = T.Byte then
               fprintf oc "    let n = int_of_char n in\n";
             List.iter
               (fun (key, name) ->
                  fprintf oc "    let l = if %s then `%s :: l else l in\n"
                    (match key with
                       | V.Byte x ->
                           sprintf "n land %d <> 0" (Char.code x)
                       | V.Int16 x | V.Uint16 x ->
                           sprintf "n land %d <> 0" x
                       | V.Int32 x | V.Uint32 x ->
                           sprintf "Int32.logand n %ldl <> 0l" x
                       | V.Int64 x | V.Uint64 x ->
                           sprintf "Int64.logand n %LdL <> 0L" x
                       | _ -> assert false)
                    (String.capitalize_ascii name))
               values;
             fprintf oc "    l\n")
    symbols;

  (***** Member description *****)

  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           fprintf oc "  let m_%s = {\n\
                      \    Method.interface = interface;\n\
                      \    Method.member = %S;\n\
                      \    Method.i_args = %a;\n\
                      \    Method.o_args = %a;\n\
                      \    Method.annotations = [%s];\n\
                      \  }\n"
             name name print_args i_args print_args o_args
             (String.concat ";\n                          "
                (List.map
                   (fun (name, value) -> sprintf "(%s, %S)" (Utils.make_annotation name) value)
                   annotations))
       | Signal(name, args, annotations) ->
           fprintf oc "  let s_%s = {\n\
                      \    Signal.interface = interface;\n\
                      \    Signal.member = %S;\n\
                      \    Signal.args = %a;\n\
                      \    Signal.annotations = [%s];\n\
                      \  }\n"
             name name print_args args
             (String.concat ";\n                          "
                (List.map
                   (fun (name, value) -> sprintf "(%s, %S)" (Utils.make_annotation name) value)
                   annotations))
       | Property(name, typ, access, annotations) ->
           fprintf oc "  let p_%s = {\n\
                      \    Property.interface = interface;\n\
                      \    Property.member = %S;\n\
                      \    Property.typ = %a;\n\
                      \    Property.access = Property.%s;\n\
                      \    Property.annotations = [%s];\n\
                      \  }\n"
             name name (Term.print_impl true) (term_impl typ)
             (match access with
                | Read -> "readable"
                | Write -> "writable"
                | Read_write -> "readable_writable")
             (String.concat ";\n                          "
                (List.map
                   (fun (name, value) -> sprintf "(%s, %S)" (Utils.make_annotation name) value)
                   annotations)))
    members;

  (***** Interface description *****)

  if !mode <> `Client then begin
    if List.exists (function Method _ | Property _ -> true | _ -> false) members then
      print_record oc members;
    output_string oc "  let make members =\n";
    fprintf oc "    OBus_object.make_interface_unsafe interface\n\
               \      [\n";
    List.iter
      (fun (name, value) ->
         fprintf oc "        (%s, %S);\n" (Utils.make_annotation name) value)
      annotations;
    fprintf oc "      ]\n\
               \      [|\n";
    List.iter
      (function
         | Method(name, i_args, o_args, annotations) ->
             fprintf oc "        method_info m_%s members.m_%s;\n" name name
         | _ ->
             ())
      members;
    output_string oc "      |]\n      [|\n";
    List.iter
      (function
         | Signal(name, args, annotations) ->
             fprintf oc "        signal_info s_%s;\n" name
         | _ ->
             ())
      members;
    output_string oc "      |]\n      [|\n";
    List.iter
      (function
         | Property(name, typ, Read, annotations) ->
             fprintf oc "        property_r_info p_%s members.p_%s;\n" name name
         | Property(name, typ, Write, annotations) ->
             fprintf oc "        property_w_info p_%s members.p_%s;\n" name name
         | Property(name, typ, Read_write, annotations) ->
             fprintf oc "        property_rw_info p_%s (fst members.p_%s) (snd members.p_%s);\n" name name name
         | _ ->
             ())
      members;
    output_string oc "      |]\n";
  end;
  output_string oc "end\n"

(* +-----------------------------------------------------------------+
   | Interface generation                                            |
   +-----------------------------------------------------------------+ *)

let string_of_key_type = function
  | T.Byte -> "char"
  | T.Int16 | T.Uint16 -> "int"
  | T.Int32 | T.Uint32 -> "int32"
  | T.Int64 | T.Uint64 -> "int64"
  | _ -> assert false

let print_intf oc name members symbols annotations =
  fprintf oc "module %s : sig\n" (String.capitalize_ascii (Utils.file_name_of_interface_name name));
  fprintf oc "  val interface : OBus_name.interface\n";

  (***** Symbols *****)

  List.iter
    (fun (name, sym) ->
       print_symbol oc name sym;
       match sym with
         | Sym_enum(typ, values) ->
             fprintf oc "  val make_%s : %s -> type_%s\n"
               name
               (string_of_key_type typ)
               name;
             fprintf oc "  val cast_%s : type_%s -> %s\n"
               name
               name
               (string_of_key_type typ)
         | Sym_flag(typ, values) ->
             fprintf oc "  val make_%s : %s -> type_%s list\n"
               name
               (string_of_key_type typ)
               name;
             fprintf oc "  val cast_%s : type_%s list -> %s\n"
               name
               name
               (string_of_key_type typ))
    symbols;

  (***** Member description *****)

  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           fprintf oc "  val m_%s : %a\n"
             name
             (Term.print_intf true)
             (term "Method.t"
                [tuple_intf (List.map snd i_args);
                 tuple_intf (List.map snd o_args)])
       | Signal(name, args, annotations) ->
           fprintf oc "  val s_%s : %a\n"
             name
             (Term.print_intf true)
             (term "Signal.t"
                [tuple_intf (List.map snd args)])
       | Property(name, typ, access, annotations) ->
           fprintf oc "  val p_%s : %a\n"
             name
             (Term.print_intf true)
             (term "Property.t"
                [term_intf typ;
                 term
                   (match access with
                      | Read -> "[ `readable ]"
                      | Write -> "[ `writable ]"
                      | Read_write -> "[ `readable | `writable ]")
                   []]))
    members;

  (***** Interface description *****)

  if !mode <> `Client then begin
    if List.exists (function Method _ | Property _ -> true | _ -> false) members then begin
      print_record oc members;
      output_string oc "  val make : 'a members -> 'a OBus_object.interface\n"
    end else
      output_string oc "  val make : unit -> 'a OBus_object.interface\n";
  end;
  output_string oc "end\n"

(* +-----------------------------------------------------------------+
   | Sorting                                                         |
   +-----------------------------------------------------------------+ *)

let compare_members ma mb =
  match ma, mb with
    | Method(name_a, i_args_a, _, _), Method(name_b, i_args_b, _, _) ->
        String.compare name_a name_b
    | Signal(name_a, _, _), Signal(name_b, _, _) ->
        String.compare name_a name_b
    | Property(name_a, _, _, _), Property(name_b, _, _, _) ->
        String.compare name_a name_b
    | Method _, _ -> -1
    | _, Method _ -> 1
    | Signal _, _ -> -1
    | _, Signal _ -> 1

let sort_members members = List.sort compare_members members

(* +-----------------------------------------------------------------+
   | Entry-point                                                     |
   +-----------------------------------------------------------------+ *)

let usage_message =
  Printf.sprintf "Usage: %s <options> <file>\n\
                  Generate OCaml modules for D-Bus interfaces.\n\
                  options are:" prog_name

let keep_common = ref false
let prefix = ref None

let args = [
  "-keep-common", Arg.Set keep_common, "do not ignore common interfaces";
  "-o", Arg.String(fun str -> prefix := Some str), "<prefix> output file prefix";
  "-mode",
  Arg.Symbol(["both"; "client"; "server"],
             (function
                | "both" -> mode := `Both
                | "client" -> mode := `Client
                | "server" -> mode := `Server
                | _ -> assert false)),
  " code generation mode, defaults to \"both\""
]

let () =
  let sources = ref [] in
  Arg.parse args (fun s -> sources := s :: !sources) usage_message;

  let source =
    match !sources with
      | [s] -> s
      | _ -> Arg.usage args usage_message; exit 1
  in

  let interfaces = Utils.parse_file source in

  let prefix =
    match !prefix with
      | Some str -> str
      | None -> (try Filename.chop_extension source with Invalid_argument _ -> source) ^ "_interfaces"
  in

  let oc_impl = open_out (prefix ^ ".ml") and oc_intf = open_out (prefix ^ ".mli") in

  fprintf oc_impl
    "(* File auto-generated by %s, DO NOT EDIT. *)\n\
     open OBus_value\n\
     open OBus_value.C\n\
     open OBus_member\n"
    prog_name;

  if !mode <> `Client then
    output_string oc_impl "open OBus_object\n";

  fprintf oc_intf
    "(* File auto-generated by %s, DO NOT EDIT. *)\n\
     open OBus_member\n"
    prog_name;

  Utils.IFSet.iter
    (fun ((name, members, symbols, annotations) as interface) ->
       if !keep_common ||
         (match OBus_name.split name with
            | "org" :: "freedesktop" :: "DBus" :: _ -> false
            | _ -> true) then begin
           (* We keeps only symbols from the extended interface *)
           let name, members, annotations = OBus_introspect_ext.encode interface in
           let members = sort_members members and annotations = List.sort compare annotations in
           print_impl oc_impl name members symbols annotations;
           print_intf oc_intf name members symbols annotations
         end)
    interfaces;

  close_out oc_impl;
  close_out oc_intf
