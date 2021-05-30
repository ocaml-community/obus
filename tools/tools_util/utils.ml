(*
 * utils.ml
 * --------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open OBus_value
open OBus_introspect_ext

module IFSet = Set.Make(struct
                          type t = OBus_introspect_ext.interface
                          let compare (n1, _, _, _) (n2, _, _, _) = String.compare n1 n2
                        end)

let parse_xml fname =
  let ic = open_in fname in
  try
    let interfaces, _ = OBus_introspect.input (Xmlm.make_input ~entity:(fun _ -> Some "") ~strip:true (`Channel ic)) in
    close_in ic;
    let interfaces = List.map OBus_introspect_ext.decode interfaces in
    List.fold_left (fun acc iface -> IFSet.add iface acc) IFSet.empty interfaces
  with
    | OBus_introspect.Parse_failure((line, column), msg) ->
        Printf.eprintf "%s:%d:%d: %s.\n%!" fname line column msg;
        exit 1

let parse_idl fname =
  try
    List.fold_left (fun acc iface -> IFSet.add iface acc) IFSet.empty (OBus_idl.parse_file fname)
  with exn ->
    Format.eprintf "@[<v0>%s@]@." (Printexc.to_string exn);
    exit 1

let parse_file fname =
  if Filename.check_suffix fname ".obus" then
    parse_idl fname
  else
    parse_xml fname

let file_name_of_interface_name name =
  let result = Bytes.create (String.length name) in
  for i = 0 to String.length name - 1 do
    if name.[i] = '.' then
      Bytes.set result i '_'
    else
      Bytes.set result i name.[i]
  done;
  Bytes.unsafe_to_string result

let paren top s = if top then s else sprintf "(%s)" s

let make_names l =
  let rec aux n = function
    | [] -> []
    | _ :: l -> sprintf "x%d" n :: aux (n + 1) l
  in
  aux 1 l

let rec convertor conv top = function
  | Term(name, []) ->
      conv top name
  | Term("array", [t]) -> begin
      match convertor conv false t with
        | Some f -> Some(paren top (sprintf "List.map %s" f))
        | None -> None
    end
  | Term("dict", [tk; tv]) -> begin
      match convertor conv true tk, convertor conv true tv with
        | None, None ->
            None
        | Some fk, None ->
            Some(paren top (sprintf "List.map (fun (k, v) -> (%s k, v))" fk))
        | None, Some fv ->
            Some(paren top (sprintf "List.map (fun (k, v) -> (k, %s v))" fv))
        | Some fk, Some fv ->
            Some(paren top (sprintf "List.map (fun (k, v) -> (%s k, %s v))" fk fv))
    end
  | Term(name, args) ->
      None
  | Tuple tl ->
      let l = List.map (convertor conv true) tl in
      if List.exists (fun f -> f <> None) l then begin
        let names = make_names tl in
        Some(sprintf "(fun (%s) -> (%s))"
               (String.concat ", " names)
               (String.concat ", " (List.map2
                                      (fun name conv ->
                                         match conv with
                                           | Some f -> sprintf "%s %s" f name
                                           | None -> name)
                                      names l)))
      end else
        None

let dbus_symbols = [
  "byte";
  "boolean";
  "int16";
  "int32";
  "int64";
  "uint16";
  "uint32";
  "uint64";
  "double";
  "string";
  "signature";
  "object_path";
  "unix_fd";
  "array";
  "dict";
  "variant";
]

let convertor_send top typ =
  convertor
    (fun top t ->
       match t with
         | "int32" | "uint32" -> Some "Int32.of_int"
         | "object_path" -> Some "OBus_proxy.path"
         | name when List.mem name dbus_symbols -> None
         | name -> Some("cast_" ^ name))
    top typ

let convertor_recv top typ =
  convertor
    (fun top t ->
       match t with
         | "int32" | "uint32" -> Some "Int32.to_int"
         | "object_path" -> Some(paren top ("(fun x -> OBus_proxy.make ~peer:(OBus_context.sender context) ~path:x)"))
         | name when List.mem name dbus_symbols -> None
         | name -> Some("make_" ^ name))
    top typ

let make_annotation = function
  | "org.freedesktop.DBus.Deprecated" -> "OBus_introspect.deprecated"
  | "org.freedesktop.DBus.GLib.CSymbol" -> "OBus_introspect.csymbol"
  | "org.freedesktop.DBus.Method.NoReply" -> "OBus_introspect.no_reply"
  | "org.freedesktop.DBus.Property.EmitsChangedSignal" -> "OBus_introspect.emits_changed_signal"
  | "org.ocamlcore.forge.obus.Enum" -> "OBus_introspect_ext.obus_enum"
  | "org.ocamlcore.forge.obus.Flag" -> "OBus_introspect_ext.obus_flag"
  | "org.ocamlcore.forge.obus.Type" -> "OBus_introspect_ext.obus_type"
  | "org.ocamlcore.forge.obus.IType" -> "OBus_introspect_ext.obus_itype"
  | "org.ocamlcore.forge.obus.OType" -> "OBus_introspect_ext.obus_otype"
  | name -> Printf.sprintf "%S" name
