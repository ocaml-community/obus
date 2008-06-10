(*
 * obus-introspect.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus
open Xparser
open Parser

let recursive = ref false
let mli = ref false
let anons = ref []
let session = ref false
let system = ref false

let args = [
  "-rec", Arg.Set recursive, "introspect recursively all sub-nodes";
  "-caml", Arg.Set mli, "print in caml style instead of xml";
  "-session", Arg.Set session, "the service is on the session bus (the default)";
  "-system", Arg.Set system, "the service is on the system bus";
]

let usage_msg = Printf.sprintf "Usage: %s <option> <destination> <path>
Introspect a DBus service (print only interfaces).
options are:" (Filename.basename (Sys.argv.(0)))

let raw_document =
  elt "node" a0
    (s2
       (any (elt "interface" (a1 (ars "name")) (s1 (any raw))
               (fun name content -> (name, content))))
       (any (elt "node" (a1 (ars "name")) s0
               (fun path -> path))))
    (fun interfs nodes -> (interfs, nodes))

let source_xml_parser = XmlParser.make ()
let _ = XmlParser.prove source_xml_parser false

let parse_string str =
  try
    Xparser.parse raw_document (XmlParser.parse source_xml_parser (XmlParser.SString str))
  with
      Xml.Error err ->
        Log.print "error while parsing xml: %s\n" (Xml.error err);
        exit 2

let uniq l =
  List.rev
    (List.fold_left
       (fun acc (name, content) ->
          if List.mem_assoc name acc
          then acc
          else (name, content) :: acc) [] l)

let rec get_interfaces bus service path =
  let interfaces, nodes = parse_string (Introspectable.introspect bus service path) in
  let subs = match !recursive with
    | true ->
        List.flatten (List.map (fun name -> get_interfaces bus service (path ^ "/" ^ name)) nodes)
    | false ->
        []
  in
    interfaces @ subs

let _ =
  Arg.parse args
    (fun arg -> anons := arg :: !anons)
    usage_msg;

  let service, path = match !anons with
    | [path; service] -> (service, path)
    | _ -> Arg.usage args usage_msg; exit 1
  in

  let bus = match !session, !system with
    | true, true ->
        Log.print "must specify at most one of -session, -system\n\n";
        Arg.usage args usage_msg;
        exit 1
    | false, false
    | true, false -> Bus.session ()
    | false, true -> Bus.system ()
  in

  let interfaces = uniq (get_interfaces bus service path) in
  let xml = Xml.Element("node", [],
                        List.map (fun (name, content) ->
                                    Xml.Element("interface", [("name", name)], content)) interfaces) in

    match !mli with
      | false ->
          print_endline (Xml.to_string_fmt xml)
      | true ->
          let node =
            List.fold_left
              (fun node (dbus_name, caml_names, content, proxy_typ, to_proxy) ->
                 Tree.insert caml_names (dbus_name, content, proxy_typ, to_proxy) node)
              Tree.empty
              (Xparser.parse document xml)
          in
            PrintInterf.print false stdout node

