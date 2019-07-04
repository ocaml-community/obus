(*
 * oBus_introspect.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_xml_parser

type name = string

type annotation = name * string
type argument = name option * OBus_value.T.single

type access = Read | Write | Read_write

type member =
  | Method of name * argument list * argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * OBus_value.T.single * access * annotation list

type interface = name * member list * annotation list
type node = OBus_path.element
type document = interface list * node list

exception Parse_failure = OBus_xml_parser.Parse_failure

let () =
  Printexc.register_printer
    (function
       | Parse_failure((line, column), msg) ->
           Some(Printf.sprintf "failed to parse D-Bus introspection document, at line %d, column %d: %s" line column msg)
       | _ ->
           None)

let annotations p =
  any p (elt "annotation"
           (fun p ->
              let name = ar p "name" in
              let value = ar p "value" in
              (name, value)))

type direction = In | Out

let atype p =
  let signature = ar p "type" in
  match OBus_value.signature_of_string signature with
    | [] -> failwith p "empty signature"
    | [t] -> t
    | _ -> Printf.ksprintf (failwith p) "this signature contains more than one single type: %S" signature

let arguments p =
  any p (elt "arg"
           (fun p ->
              let name = ao p "name" in
              let dir = afd p "direction" In [("in", In); ("out", Out)] in
              let typ = atype p in
              (dir, (name, typ))))

let mk_aname test p =
  let name = ar p "name" in
  match test name with
    | Some error -> failwith p (OBus_string.error_message error)
    | None -> name

let amember = mk_aname OBus_name.validate_member
let anode = mk_aname OBus_path.validate_element
let ainterface = mk_aname OBus_name.validate_interface

let method_decl =
  elt "method"
    (fun p ->
       let name = amember p in
       let args = arguments p in
       let ins, outs =
         OBus_util.split
           (function
              | (In, x) -> OBus_util.InL x
              | (Out, x) -> OBus_util.InR x)
           args
       in
       let annots = annotations p in
       (Method(name, ins, outs, annots)))

let signal_decl =
  elt "signal"
    (fun p ->
       let name = amember p in
       let args = arguments p in
       let annots = annotations p in
       (Signal(name, List.map snd args, annots)))

let property_decl =
  elt "property"
    (fun p ->
       let name = amember p in
       let access = afr p "access" [("read", Read); ("write", Write); ("readwrite", Read_write)] in
       let typ = atype p in
       let annots = annotations p in
       (Property(name, typ, access, annots)))

let node =
  elt "node" (fun p ->
                let name = anode p in
                match OBus_path.validate_element name with
                  | None -> name
                  | Some error -> failwith p (OBus_string.error_message { error with OBus_string.typ = "node name" }))

let interface =
  elt "interface"
    (fun p ->
       let name = ainterface p in
       let decls = any p (union [method_decl;
                                 signal_decl;
                                 property_decl]) in
       let annots = annotations p in
       (name, decls, annots))

let document =
  elt "node"
    (fun p ->
       let interfs = any p interface in
       let subs = any p node in
       (interfs, subs))

let input xi = OBus_xml_parser.input xi document

type xml = Element of string * (string * string) list * xml list

let to_xml (ifaces, nodes) =
  let pannots = List.map (fun (n, v) -> Element("annotation", [("name", n); ("value", v)], [])) in
  let pargs dir = List.map (fun (n, t) ->
                             let attrs = [("type", OBus_value.string_of_signature [t])] in
                             let attrs = match dir with
                               | Some In -> ("direction", "in") :: attrs
                               | Some Out -> ("direction", "out") :: attrs
                               | None -> attrs in
                             let attrs = match n with
                               | Some n -> ("name", n) :: attrs
                               | None -> attrs in
                             Element("arg", attrs, [])) in
  Element("node", [],
          List.map (fun (name, content, annots) ->
                      Element("interface", [("name", name)],
                              pannots annots
                              @ List.map
                                (function
                                   | Method(name, ins, outs, annots) ->
                                       Element("method", [("name", name)],
                                               pargs (Some In) ins
                                               @ pargs (Some Out) outs
                                               @ pannots annots)
                                   | Signal(name, args, annots) ->
                                       Element("signal", [("name", name)],
                                               pargs None args
                                               @ pannots annots)
                                   | Property(name, typ, access, annots) ->
                                       Element("property",
                                               [("name", name);
                                                ("type", OBus_value.string_of_signature [typ]);
                                                ("access", match access with
                                                   | Read -> "read"
                                                   | Write -> "write"
                                                   | Read_write -> "readwrite")],
                                               pannots annots))
                                content)) ifaces
          @ List.map (fun n -> Element("node", [("name", n)], [])) nodes)

let output xo doc =
  let rec aux (Element(name, attrs, children)) =
    Xmlm.output xo (`El_start(("", name), List.map (fun (name, value) -> (("", name), value)) attrs));
    List.iter aux children;
    Xmlm.output xo `El_end
  in
  Xmlm.output xo (`Dtd(Some "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"\n\
                             \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">"));
  aux (to_xml doc)

(* +-----------------------------------------------------------------+
   | Annotations                                                     |
   +-----------------------------------------------------------------+ *)

let deprecated = "org.freedesktop.DBus.Deprecated"
let csymbol = "org.freedesktop.DBus.GLib.CSymbol"
let no_reply = "org.freedesktop.DBus.Method.NoReply"
let emits_changed_signal = "org.freedesktop.DBus.Property.EmitsChangedSignal"
