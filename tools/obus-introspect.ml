(*
 * obus-introspect.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Common
open OBus_type

let recursive = ref false
let mli = ref false
let anons = ref []
let session = ref false
let system = ref false
let obj_mode = ref false

let args = [
  "-rec", Arg.Set recursive, "introspect recursively all sub-nodes";
  "-caml", Arg.Set mli, "print in caml style instead of xml";
  "-session", Arg.Set session, "the service is on the session bus (the default)";
  "-system", Arg.Set system, "the service is on the system bus";
  "-objects", Arg.Set obj_mode, "list objects with interfaces they implements instead of interfaces";
]

let usage_msg = Printf.sprintf "Usage: %s <option> <destination> <path>
Introspect a DBus service (print only interfaces).
options are:" (Filename.basename (Sys.argv.(0)))

open XParser

let raw_document =
  elt "node"
    (perform
       interfaces <-- any (elt "interface"
                             (perform
                                name <-- ar "name";
                                content <-- any raw;
                                return (name, content)));
       subs <-- any (elt "node" (ar "name"));
       return (interfaces, subs))

open Lwt

let (&) a b = a b
let (|>) a b x = b (a x)

let introspect bus service path =
  OBus_connection.method_call bus
    ~destination:service
    ~path:path
    ~interface:"org.freedesktop.DBus.Introspectable"
    ~member:"Introspect"
    (<< string >>)

module Interf_map = Map.Make(struct type t = string let compare = compare end)

let rec get (nodes, map) bus service path =
  (perform
     (interfaces, subs) <-- introspect bus service path
     >>= (fun str -> return & parse_source raw_document (XmlParser.SString str));
     let map = List.fold_left (fun map (name, content) -> Interf_map.add name content map) map interfaces in
     let nodes = (path, List.map fst interfaces) :: nodes in
     match !recursive with
       | true ->
           Lwt_util.fold_left (fun acc name -> get acc bus service & OBus_path.append path name) (nodes, map) subs
       | false ->
           return (nodes, map))

let main service path =
  (perform
     bus <-- Lazy.force begin match !session, !system with
       | true, true ->
           prerr_endline "must specify at most one of -session, -system\n\n";
           Arg.usage args usage_msg;
           exit 1
       | false, false
       | true, false -> OBus_bus.session
       | false, true -> OBus_bus.system
     end;
     (nodes, map) <-- get ([], Interf_map.empty) bus service path;
     let _ = match !obj_mode with
       | false ->
           let xml = Xml.Element("node", [],
                                 Interf_map.fold (fun name content acc ->
                                                    Xml.Element("interface", [("name", name)], content) :: acc)
                                   map []) in
           begin match !mli with
             | false ->
                 print_endline & Xml.to_string_fmt xml
             | true ->
                 let interfaces, _ = parse_xml IParser.document xml in
                 List.iter (print_interf Format.std_formatter) interfaces
           end
       | true ->
           List.iter begin fun (path, interfaces) ->
             print_endline path;
             List.iter (Printf.printf " + %s\n") interfaces;
             print_newline ();
           end nodes
     in
     return ())

let _ =
  Arg.parse args
    (fun arg -> anons := arg :: !anons)
    usage_msg;

  let service, path = match !anons with
    | [path; service] -> (service, path)
    | _ -> Arg.usage args usage_msg; exit 1
  in

    Lwt_unix.run & main service path
